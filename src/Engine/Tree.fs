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
      Nodes: int64
      /// The paranoid search saw only losses, Move/Depth/Score come from the nearest-opponent-only re-search
      Relaxed: bool }

/// Iterative-deepening paranoid alpha-beta over simultaneous moves.
///
/// One ply = I pick a move, then the opponents pick a joint reply knowing my move (pessimistic, which is what
/// makes head-on collisions and cut-offs safe). Only opponents whose head can reach mine within the remaining
/// plies are searched adversarially; far-away snakes play a cheap default move, so 4-snake games still get depth.
/// All boards and move buffers are preallocated per ply; a search does not allocate.
/// Transposition table entry, 16 bytes. The slot index is the hash's low bits. Data is the score's bits;
/// Key = meta ^^^ Data, meta = check (hash high 32 bits) << 32 | depth << 24 | flag << 16 | move << 8 | age,
/// flag 0 none, 1 exact, 2 lower bound, 3 upper bound. The XOR makes a torn entry (two threads writing the same
/// slot at once, possible since the table is shared without locks) fail the check instead of mixing fields.
[<Struct; NoEquality; NoComparison>]
type TTEntry =
    val mutable Data: uint64
    val mutable Key: uint64

/// A transposition table, shareable by several Searchers (Lazy SMP). Age counts searches, for replacement.
[<Sealed; AllowNullLiteral>]
type TTable(bits: int) =
    let entries = Array.zeroCreate<TTEntry> (1 <<< bits)
    let mutable age = 0uy
    member _.Entries = entries
    member _.Mask = (1 <<< bits) - 1

    member _.Age
        with get () = age
        and set v = age <- v

    member _.Clear() = Array.Clear entries

[<Sealed>]
type Searcher(geo: Geometry, maxSnakes: int, weights: Weights, shared: TTable) =
    static let maxPly = 64
    static let win = 1e6
    static let likelyWin = 1e5
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
    // A shared table is aged by its owner (ParallelSearcher); our own we age per Search
    let ownTable = isNull shared
    let table = if ownTable then TTable(ttBits) else shared
    let ttMask = table.Mask
    // One 16-byte entry per slot so a probe touches a single cache line (separate arrays per field cost up to six
    // misses per probe, which showed up on the A1's small caches).
    let tt = table.Entries
    let mutable age = 0uy
    // Lazy SMP helper index (0 = main). Odd helpers start deepening at depth 2 so threads spread over depths.
    let mutable helper = 0
    // How helpers spread over depths: 0 = odd helpers start at depth 2 only; 2 = Stockfish-style depth skipping.
    // A1, 250 ms, 4 threads: +0.25 ply (0) vs +0.53 ply (2); 1 (rotating move order near the root) was worse.
    let mutable diversity = 2
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
    let mutable soundProofs = true
    // Re-search after a paranoid proven loss: only the nearest opponent is adversarial, no TT scores in or out
    let mutable relaxed = false
    // Relaxed mode also keeps opponents within 2 steps adversarial (they can reach our next cell); false = 0.7.0's
    // nearest-only, which walked into a second, equally close opponent (game 7a8e3ab9)
    let mutable relaxNear = true

    /// Stop after this many nodes as well as at the deadline (0 = no limit). Makes self-play deterministic
    /// and independent of how loaded the machine is.
    member _.NodeLimit
        with get () = nodeLimit
        and set v = nodeLimit <- v

    /// false restores the pre-0.6.1 behaviour (far lone opponent played by Default, its deaths count as proven),
    /// for A/B in the arena only.
    member _.SoundProofs
        with get () = soundProofs
        and set v = soundProofs <- v

    new(geo, maxSnakes, weights) = Searcher(geo, maxSnakes, weights, null)
    new(geo, maxSnakes) = Searcher(geo, maxSnakes, Weights.Default, null)

    static member TTBits = ttBits
    static member val internal SkipSize = [| 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 4; 4; 4; 4; 4; 4; 4; 4 |]
    static member val internal SkipPhase = [| 0; 1; 0; 1; 2; 3; 0; 1; 2; 3; 4; 5; 0; 1; 2; 3; 4; 5; 6; 7 |]

    member _.Helper
        with get () = helper
        and set v = helper <- v

    member _.RelaxNear
        with get () = relaxNear
        and set v = relaxNear <- v

    member _.Diversity
        with get () = diversity
        and set v = diversity <- v

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
        table.Clear()
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
            let e = &tt.[slot]
            let data = e.Data
            let meta = e.Key ^^^ data
            let flag = byte (meta >>> 16)
            let hit = uint32 (meta >>> 32) = uint32 (hash >>> 32) && flag <> 0uy

            if hit && useScores && not relaxed then
                let td = int (byte (meta >>> 24))

                if td = depth || (td > depth && not exactDepthOnly) then
                    let v = Searcher.FromTT(BitConverter.UInt64BitsToDouble data, ply)

                    match flag with
                    | 1uy -> cut <- v
                    | 2uy when v >= beta -> cut <- v
                    | 3uy when v <= alpha -> cut <- v
                    | _ -> ()

            if Double.IsNaN cut && nm > 1 then
                this.Order(mine, 0, nm, histMe, myHead0, (if hit then int (byte (meta >>> 8)) else -1))

        if not (Double.IsNaN cut) then cut else

        // Opponents: adversarial if their head can meet mine within the remaining plies
        let myHead = b.Head me
        let mx, my = geo.X myHead, geo.Y myHead
        let advs = adv.[ply]
        let am = advMoves.[ply]
        let ac = advCount.[ply]
        let mutable na = 0
        let mutable opps = 0

        for s in 0 .. b.NumSnakes - 1 do
            if s <> me && b.Alive s then opps <- opps + 1

        // Snakes moved by Default this node: their deaths are guesses, not forced (see the win case below)
        let mutable simplified = false
        let mutable nearest = -1

        if relaxed then
            let mutable bd = Int32.MaxValue

            for s in 0 .. b.NumSnakes - 1 do
                if s <> me && b.Alive s then
                    let h = b.Head s
                    let dist = abs (geo.X h - mx) + abs (geo.Y h - my)
                    if dist < bd then
                        bd <- dist
                        nearest <- s

        for s in 0 .. b.NumSnakes - 1 do
            if s <> me && b.Alive s then
                let h = b.Head s
                let dist = abs (geo.X h - mx) + abs (geo.Y h - my)

                // A lone opponent is always searched: 1v1 the whole board is the fight (its space is our space),
                // and a far opponent walking into a dead end under Default produced fake forced wins (game 60b3aa33).
                if (not relaxed || s = nearest || (relaxNear && dist <= 2)) && ((soundProofs && opps = 1) || dist <= 2 * depth) then
                    advs.[na] <- s
                    ac.[na] <- this.Gen(b, s, am, na * 4)
                    if ordering && ac.[na] > 1 then this.Order(am, na * 4, ac.[na], histOpp, h, -1)
                    na <- na + 1
                else
                    mv.[s] <- this.Default(b, s)
                    simplified <- true

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
                        // Only proven if every opponent that died this step was searched. A Default snake dying is
                        // likely, not forced: score it high but below win / 2 so it neither ends the iterative
                        // deepening nor counts as a proof in the TT.
                        if simplified && soundProofs then likelyWin - float ply else win - float ply
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

        if ordering && not aborted && ply > 0 && not relaxed then
            // Same position: keep the deeper result (it may come from pondering, i.e. an older search).
            // Other position: keep the deeper entry of the current search, entries of older searches can go.
            let e = &tt.[slot]
            let check = uint32 (hash >>> 32)
            let old = e.Key ^^^ e.Data

            let keep =
                byte (old >>> 16) <> 0uy
                && depth < int (byte (old >>> 24))
                && (uint32 (old >>> 32) = check || byte old = age)

            if not keep then
                let flag = if best <= alpha then 3uy elif best >= beta then 2uy else 1uy
                let data = BitConverter.DoubleToUInt64Bits(Searcher.ToTT(best, ply))

                let meta =
                    (uint64 check <<< 32)
                    ||| (uint64 (byte depth) <<< 24)
                    ||| (uint64 flag <<< 16)
                    ||| (uint64 (byte mine.[bestI]) <<< 8)
                    ||| uint64 age

                e.Data <- data
                e.Key <- meta ^^^ data

        best

    /// Searches until the Stopwatch timestamp `stopAt`. Always returns a legal move, even with no time at all.
    /// Iterative deepening from depth 1 over the root moves in myMoves.[0] (nm > 1), until the deadline,
    /// a proven result or maxDepth.
    member private this.Deepen(nm: int) : SearchInfo =
        let mine = myMoves.[0]
        let mutable result = { Move = mine.[0]; Depth = 0; Score = 0.0; Nodes = 0L; Relaxed = relaxed }
        let mutable depth = if helper % 2 = 1 then 2 else 1
        let mutable fin = false

        while not fin && depth <= min maxPly maxDepth do
          // Stockfish's skip pattern: helper i skips some depths so threads are spread over iterations
          let skip =
              diversity = 2 && helper > 0
              && (let k = (helper - 1) % 20
                  ((depth + Searcher.SkipPhase.[k]) / Searcher.SkipSize.[k]) % 2 = 1)

          if skip then
              depth <- depth + 1
          else
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
                      Nodes = nodes
                      Relaxed = relaxed }

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

        result

    member this.Search(root: Board, mySnake: int, stopAt: int64) : SearchInfo =
        Volatile.Write(&stopRequested, false)
        this.SearchCore(root, mySnake, stopAt)

    /// Clears a pending Stop. ParallelSearcher does this before starting helpers, so a Stop that comes before a
    /// helper got going is not lost.
    member _.ResetStop() = Volatile.Write(&stopRequested, false)

    /// Search without clearing a pending Stop.
    member this.SearchCore(root: Board, mySnake: int, stopAt: int64) : SearchInfo =
        me <- mySnake
        deadline <- stopAt
        nodes <- 0L
        aborted <- false
        boards.[0].CopyFrom root

        if ownTable then table.Age <- table.Age + 1uy
        age <- table.Age

        // Old history still says something about this game, but recent results should dominate
        for i in 0 .. histMe.Length - 1 do
            histMe.[i] <- histMe.[i] >>> 2
            histOpp.[i] <- histOpp.[i] >>> 2

        let mine = myMoves.[0]
        let nm = this.Gen(boards.[0], me, mine, 0)
        let mutable result = { Move = mine.[0]; Depth = 0; Score = 0.0; Nodes = 0L; Relaxed = false }

        if nm > 1 then
            result <- this.Deepen nm

            let mutable opps = 0

            for s in 0 .. root.NumSnakes - 1 do
                if s <> me && root.Alive s then opps <- opps + 1

            // Every move loses against all opponents coordinating, which real opponents do not do (73 won ladder
            // games had such "proven losses"). Rather than just dying as late as possible, pick the best move
            // against the nearest opponent alone, with whatever time is left.
            if soundProofs && opps >= 2 && result.Score < -win / 2.0 && not aborted then
                relaxed <- true
                let r = this.Deepen nm
                relaxed <- false
                if r.Depth > 0 then result <- r

        { result with Nodes = nodes }

    /// Convenience overload: search for `ms` milliseconds from now.
    member this.SearchFor(root: Board, mySnake: int, ms: float) =
        let stopAt = Stopwatch.GetTimestamp() + int64 (ms * float Stopwatch.Frequency / 1000.0)
        this.Search(root, mySnake, stopAt)

/// Lazy SMP: `threads` Searchers share one table and search the same root at once; the helpers mostly fill the
/// table (their cut-offs and exact scores let the main thread skip work), and the deepest finished result wins.
/// Each Searcher keeps its own boards, history and evaluator, so nothing but the table is shared.
[<Sealed>]
type ParallelSearcher(geo: Geometry, maxSnakes: int, weights: Weights, threads: int) =
    let table = TTable(Searcher.TTBits)

    let workers =
        Array.init (max 1 threads) (fun i ->
            let s = Searcher(geo, maxSnakes, weights, table)
            s.Helper <- i
            s)

    let results = Array.zeroCreate<SearchInfo> workers.Length

    member _.Threads = workers.Length
    member _.Main = workers.[0]

    member _.Diversity
        with get () = workers.[0].Diversity
        and set v =
            for w in workers do
                w.Diversity <- v

    member _.Clear() =
        for w in workers do
            w.Clear()

    /// Searches with the main thread plus up to `helpers` extra threads (capped at Threads - 1), until `stopAt`.
    member _.Search(root: Board, me: int, stopAt: int64, helpers: int) : SearchInfo =
        table.Age <- table.Age + 1uy
        let n = Math.Clamp(helpers, 0, workers.Length - 1)

        if n = 0 then
            workers.[0].Search(root, me, stopAt)
        else
            for i in 1..n do
                workers.[i].ResetStop()

            let tasks =
                Array.init n (fun k ->
                    let i = k + 1
                    Tasks.Task.Run(fun () -> results.[i] <- workers.[i].SearchCore(root, me, stopAt)))

            let main = workers.[0].Search(root, me, stopAt)

            // The main thread decides when to stop (deadline or a proven result); helpers follow
            for i in 1..n do
                workers.[i].Stop()

            Tasks.Task.WaitAll tasks
            let mutable best = main
            let mutable nodes = main.Nodes

            for i in 1..n do
                let r = results.[i]
                nodes <- nodes + r.Nodes
                // A helper that completed a deeper iteration in the same mode knows more than the main thread
                if r.Relaxed = main.Relaxed && r.Depth > best.Depth then best <- r

            { best with Nodes = nodes }
