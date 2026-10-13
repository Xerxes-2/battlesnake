namespace Snake.Engine

open System
open System.Diagnostics
open System.Threading

/// Result of one search: the chosen move, the deepest fully completed depth, the score and the node count.
[<Struct>]
type SearchInfo =
    { Move: Dir
      Depth: int
      Score: float
      Nodes: int64 }

/// Iterative-deepening paranoid alpha-beta over simultaneous moves.
///
/// One ply = I pick a move, then the opponents pick a joint reply knowing my move (pessimistic, which is what
/// makes head-on collisions and cut-offs safe). Only opponents whose head can reach mine within the remaining
/// plies are searched adversarially; far-away snakes play a cheap default move, so 4-snake games still get depth.
/// All boards and move buffers are preallocated per ply; a search does not allocate.
[<Sealed>]
type Searcher(geo: Geometry, maxSnakes: int, weights: Weights) =
    static let maxPly = 64
    static let win = 1e6
    static let checkMask = 255L

    let analyzer = Analyzer(geo, maxSnakes, weights)
    let boards = Array.init (maxPly + 2) (fun _ -> Board(geo, maxSnakes))
    let moves = Array.init (maxPly + 1) (fun _ -> Array.zeroCreate<Dir> maxSnakes)
    let myMoves = Array.init (maxPly + 1) (fun _ -> Array.zeroCreate<Dir> 4)
    let adv = Array.init (maxPly + 1) (fun _ -> Array.zeroCreate<int> maxSnakes)
    let advMoves = Array.init (maxPly + 1) (fun _ -> Array.zeroCreate<Dir> (maxSnakes * 4))
    let advCount = Array.init (maxPly + 1) (fun _ -> Array.zeroCreate<int> maxSnakes)
    let odo = Array.init (maxPly + 1) (fun _ -> Array.zeroCreate<int> maxSnakes)
    let tmp = Array.zeroCreate<Dir> 4
    let rootScore = Array.zeroCreate<float> 4

    // Transposition table: position hash -> best move (ordering hint) and, if enabled, a score bound at some
    // remaining depth. Positions recur across iterative-deepening iterations and, via the persistent table, on the
    // next turn and during pondering. The hash covers bodies, lengths, health, food and hazards, i.e. everything
    // the value depends on given (depth, me, weights), which are fixed per Searcher.
    static let ttBits = 19
    let ttMask = (1 <<< ttBits) - 1
    let ttKey = Array.zeroCreate<uint64> (1 <<< ttBits)
    let ttVal = Array.zeroCreate<float> (1 <<< ttBits)
    let ttDepth = Array.zeroCreate<byte> (1 <<< ttBits)
    let ttFlag = Array.zeroCreate<byte> (1 <<< ttBits) // 0 none, 1 exact, 2 lower bound, 3 upper bound
    let ttMove = Array.zeroCreate<byte> (1 <<< ttBits)
    let ttAge = Array.zeroCreate<byte> (1 <<< ttBits)
    let mutable age = 0uy
    let mutable useScores = true
    let mutable exactDepthOnly = false
    let mutable stopRequested = false
    let histMe = Array.zeroCreate<int> (geo.Cells * 4)
    let histOpp = Array.zeroCreate<int> (geo.Cells * 4)
    let key = Array.zeroCreate<int> 4
    let mutable ordering = true
    let mutable maxDepth = maxPly

    let mutable me = 0
    let mutable deadline = 0L
    let mutable nodes = 0L
    let mutable aborted = false
    let mutable nodeLimit = 0L

    /// Stop after this many nodes as well as at the deadline (0 = no limit). Makes self-play deterministic
    /// and independent of how loaded the machine is.
    member _.NodeLimit
        with get () = nodeLimit
        and set v = nodeLimit <- v

    new(geo, maxSnakes) = Searcher(geo, maxSnakes, Weights.Default)

    /// Move ordering on/off (off = the plain fixed order; for tests and A/B comparisons).
    member _.Ordering
        with get () = ordering
        and set v = ordering <- v

    /// Deepest iteration to run (for fixed-depth tests).
    member _.MaxDepth
        with get () = maxDepth
        and set v = maxDepth <- v

    /// Reuse stored scores, not just best-move hints.
    member _.UseScores
        with get () = useScores
        and set v = useScores <- v

    /// Only reuse scores stored at exactly the same remaining depth (keeps fixed-depth results identical; tests).
    member _.ExactDepthOnly
        with get () = exactDepthOnly
        and set v = exactDepthOnly <- v

    /// Ask a running Search (on another thread) to stop soon. Cleared when the next Search starts.
    member _.Stop() = Volatile.Write(&stopRequested, true)

    /// Forget hints and history (e.g. a new game).
    member _.Clear() =
        Array.Clear ttKey
        Array.Clear ttFlag
        Array.Clear histMe
        Array.Clear histOpp

    // Never 0, so an empty slot cannot match
    member private _.Hash(b: Board) = b.Hash ||| 1UL

    /// Sorts dst.[off .. off+n-1] by descending hist.[head*4+dir], with `first` (if present) in front.
    member private _.Order(dst: Dir[], off: int, n: int, hist: int[], head: int, first: int) =
        for i in 0 .. n - 1 do
            let d = int dst.[off + i]
            key.[i] <- if d = first then Int32.MaxValue else hist.[head * 4 + d]

        for i in 1 .. n - 1 do
            let k, m = key.[i], dst.[off + i]
            let mutable j = i - 1

            while j >= 0 && key.[j] < k do
                key.[j + 1] <- key.[j]
                dst.[off + j + 1] <- dst.[off + j]
                j <- j - 1

            key.[j + 1] <- k
            dst.[off + j + 1] <- m

    /// A cell is blocked next turn if a body segment will still be there: anything except a tail
    /// that is about to move (a tail stacked after eating has occupancy 2 and stays).
    static member Blocked(b: Board, c: int) =
        let o = b.Occupancy c

        if o = 0 then
            false
        elif o > 1 then
            true
        else
            let mutable tail = false

            for s in 0 .. b.NumSnakes - 1 do
                if b.Alive s && b.Body(s, b.Length s - 1) = c then tail <- true

            not tail

    /// Writes snake s's candidate moves into dst and returns how many. Moves into walls or bodies are dropped
    /// unless nothing else is left, in which case all in-bounds moves are kept (we still need to pick one).
    member private _.Gen(b: Board, s: int, dst: Dir[], off: int) =
        let h = b.Head s
        let mutable k = 0

        for d in Dir.all do
            let nb = geo.Neighbor(h, d)

            if nb >= 0 && not (Searcher.Blocked(b, nb)) then
                dst.[off + k] <- d
                k <- k + 1

        if k = 0 then
            for d in Dir.all do
                if geo.Neighbor(h, d) >= 0 then
                    dst.[off + k] <- d
                    k <- k + 1

        if k = 0 then
            dst.[off] <- Dir.Up
            k <- 1

        k

    /// Move for a snake that is not searched: the safe move whose target has the most open neighbours.
    member private this.Default(b: Board, s: int) =
        let n = this.Gen(b, s, tmp, 0)
        let h = b.Head s
        let mutable best = tmp.[0]
        let mutable bestFree = -1

        for i in 0 .. n - 1 do
            let t = geo.Neighbor(h, tmp.[i])

            if t >= 0 then
                let mutable free = 0

                for d in Dir.all do
                    let nb = geo.Neighbor(t, d)
                    if nb >= 0 && not (Searcher.Blocked(b, nb)) then free <- free + 1

                if free > bestFree then
                    bestFree <- free
                    best <- tmp.[i]

        best

    /// Win/loss scores encode the ply they happen at; the table stores them relative to the node instead.
    static member ToTT(v: float, ply: int) =
        if v > win / 2.0 then v + float ply
        elif v < -win / 2.0 then v - float ply
        else v

    static member FromTT(v: float, ply: int) =
        if v > win / 2.0 then v - float ply
        elif v < -win / 2.0 then v + float ply
        else v

    member private _.CheckTime() =
        if (nodeLimit > 0L && nodes >= nodeLimit)
           || ((nodes &&& checkMask) = 0L
               && (Stopwatch.GetTimestamp() > deadline || Volatile.Read(&stopRequested))) then
            aborted <- true

    /// Value of boards.[ply] for me with `depth` plies left, inside the window (alpha, beta).
    /// At ply 0 the per-move scores land in rootScore and myMoves.[0] holds the move order.
    member private this.Max(ply: int, depth: int, alpha: float, beta: float, nMine: int) : float =
        let b = boards.[ply]
        let mv = moves.[ply]
        let mine = myMoves.[ply]
        let nm = if ply = 0 then nMine else this.Gen(b, me, mine, 0)
        let myHead0 = b.Head me
        let mutable hash = 0UL
        let mutable slot = 0

        let mutable cut = nan

        if ordering && ply > 0 then
            hash <- this.Hash b
            slot <- int hash &&& ttMask
            let hit = ttKey.[slot] = hash && ttFlag.[slot] <> 0uy

            if hit && useScores then
                let td = int ttDepth.[slot]

                if td = depth || (td > depth && not exactDepthOnly) then
                    let v = Searcher.FromTT(ttVal.[slot], ply)

                    match ttFlag.[slot] with
                    | 1uy -> cut <- v
                    | 2uy when v >= beta -> cut <- v
                    | 3uy when v <= alpha -> cut <- v
                    | _ -> ()

            if Double.IsNaN cut && nm > 1 then
                this.Order(mine, 0, nm, histMe, myHead0, (if hit then int ttMove.[slot] else -1))

        if not (Double.IsNaN cut) then cut else

        // Opponents: adversarial if their head can meet mine within the remaining plies
        let myHead = b.Head me
        let mx, my = geo.X myHead, geo.Y myHead
        let advs = adv.[ply]
        let am = advMoves.[ply]
        let ac = advCount.[ply]
        let mutable na = 0

        for s in 0 .. b.NumSnakes - 1 do
            if s <> me && b.Alive s then
                let h = b.Head s
                let dist = abs (geo.X h - mx) + abs (geo.Y h - my)

                if dist <= 2 * depth then
                    advs.[na] <- s
                    ac.[na] <- this.Gen(b, s, am, na * 4)
                    if ordering && ac.[na] > 1 then this.Order(am, na * 4, ac.[na], histOpp, h, -1)
                    na <- na + 1
                else
                    mv.[s] <- this.Default(b, s)

        let od = odo.[ply]
        let child = boards.[ply + 1]
        let mutable best = Double.NegativeInfinity
        let mutable bestI = 0
        let mutable i = 0

        while i < nm && not aborted do
            mv.[me] <- mine.[i]
            let a = max alpha best
            let mutable worst = Double.PositiveInfinity
            Array.Clear(od, 0, na)
            let mutable fin = false

            while not fin do
                for j in 0 .. na - 1 do
                    mv.[advs.[j]] <- am.[j * 4 + od.[j]]

                child.CopyFrom b
                child.Step mv
                nodes <- nodes + 1L
                this.CheckTime()

                let v =
                    if not (child.Alive me) then
                        // Dying later is better: gives the opponents more chances to blunder
                        -win + float ply
                    elif child.NumSnakes > 1 && child.AliveCount = 1 then
                        win - float ply
                    elif depth = 1 then
                        analyzer.Evaluate(child, me)
                    else
                        this.Max(ply + 1, depth - 1, a, min beta worst, 0)

                if v < worst then worst <- v

                if worst <= a || aborted then
                    // This move cannot beat what we already have; credit the opponents' refutation
                    if ordering && not aborted && depth > 1 then
                        for j in 0 .. na - 1 do
                            let s = advs.[j]
                            let i4 = b.Head s * 4 + int mv.[s]
                            histOpp.[i4] <- histOpp.[i4] + depth * depth

                    fin <- true
                else
                    let mutable j = 0
                    let mutable carry = true

                    while carry && j < na do
                        od.[j] <- od.[j] + 1

                        if od.[j] = ac.[j] then
                            od.[j] <- 0
                            j <- j + 1
                        else
                            carry <- false

                    if carry then fin <- true

            if not aborted then
                if ply = 0 then rootScore.[i] <- worst
                if worst > best then
                    best <- worst
                    bestI <- i

                if best >= beta then i <- nm // cut-off
                else i <- i + 1

        if ordering && not aborted && nm > 1 then
            let d = mine.[bestI]
            let i4 = myHead0 * 4 + int d
            histMe.[i4] <- histMe.[i4] + depth * depth

        if ordering && not aborted && ply > 0 then
            // Same position: keep the deeper result (it may come from pondering, i.e. an older search).
            // Other position: keep the deeper entry of the current search, entries of older searches can go.
            let keep =
                ttFlag.[slot] <> 0uy
                && depth < int ttDepth.[slot]
                && (ttKey.[slot] = hash || ttAge.[slot] = age)

            if not keep then
                ttKey.[slot] <- hash
                ttVal.[slot] <- Searcher.ToTT(best, ply)
                ttDepth.[slot] <- byte depth
                ttFlag.[slot] <- (if best <= alpha then 3uy elif best >= beta then 2uy else 1uy)
                ttMove.[slot] <- byte mine.[bestI]
                ttAge.[slot] <- age

        best

    /// Searches until the Stopwatch timestamp `stopAt`. Always returns a legal move, even with no time at all.
    member this.Search(root: Board, mySnake: int, stopAt: int64) : SearchInfo =
        me <- mySnake
        deadline <- stopAt
        nodes <- 0L
        aborted <- false
        boards.[0].CopyFrom root

        age <- age + 1uy
        Volatile.Write(&stopRequested, false)

        // Old history still says something about this game, but recent results should dominate
        for i in 0 .. histMe.Length - 1 do
            histMe.[i] <- histMe.[i] >>> 2
            histOpp.[i] <- histOpp.[i] >>> 2

        let mine = myMoves.[0]
        let nm = this.Gen(boards.[0], me, mine, 0)
        let mutable result = { Move = mine.[0]; Depth = 0; Score = 0.0; Nodes = 0L }

        if nm > 1 then
            let mutable depth = 1
            let mutable fin = false

            while not fin && depth <= min maxPly maxDepth do
                for i in 0 .. nm - 1 do
                    rootScore.[i] <- Double.NegativeInfinity

                let v = this.Max(0, depth, Double.NegativeInfinity, Double.PositiveInfinity, nm)

                // Best fully searched move; the previous best is searched first, so even a partial
                // iteration only switches when a move proved strictly better.
                let mutable bi = -1

                for i in 0 .. nm - 1 do
                    if rootScore.[i] > Double.NegativeInfinity && (bi < 0 || rootScore.[i] > rootScore.[bi]) then
                        bi <- i

                // Partial iterations are only usable once the previous best (searched first) has an exact score;
                // any move that beat it afterwards is exact as well. Otherwise moves were compared at different depths.
                if bi >= 0 && (not aborted || rootScore.[0] > Double.NegativeInfinity) then
                    result <-
                        { Move = mine.[bi]
                          Depth = (if aborted then result.Depth else depth)
                          Score = rootScore.[bi]
                          Nodes = nodes }

                if aborted then
                    fin <- true
                else
                    // Re-order: best first, the rest by score (stable insertion sort on 4 items)
                    for i in 1 .. nm - 1 do
                        let s, m = rootScore.[i], mine.[i]
                        let mutable j = i - 1

                        while j >= 0 && rootScore.[j] < s do
                            rootScore.[j + 1] <- rootScore.[j]
                            mine.[j + 1] <- mine.[j]
                            j <- j - 1

                        rootScore.[j + 1] <- s
                        mine.[j + 1] <- m

                    // Proven win or loss: deeper search cannot change the outcome
                    if abs v > win / 2.0 then fin <- true
                    depth <- depth + 1

        { result with Nodes = nodes }

    /// Convenience overload: search for `ms` milliseconds from now.
    member this.SearchFor(root: Board, mySnake: int, ms: float) =
        let stopAt = Stopwatch.GetTimestamp() + int64 (ms * float Stopwatch.Frequency / 1000.0)
        this.Search(root, mySnake, stopAt)
