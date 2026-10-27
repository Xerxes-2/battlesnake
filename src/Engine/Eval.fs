namespace Snake.Engine

open System
open System.Numerics

/// Board analysis: Voronoi territory that accounts for when bodies vacate.
/// The body segment at index k (counting from the head) on a snake of length L leaves after L-k turns.
/// So a cell is only passable once the arrival distance d >= its vacate time.
/// All buffers are preallocated, so it does not allocate; that is why this one is an object (scratch space for the
/// hot path), while everything that only reads its result is a plain function (see Eval).
[<Sealed>]
type Territory(geo: Geometry, maxSnakes: int) =
    let cells = geo.Cells
    let dist = Array.zeroCreate<int> cells
    /// -1 unreached, -2 reached by several snakes at the same time (contested), otherwise the snake index
    let owner = Array.zeroCreate<int> cells
    let vacate = Array.zeroCreate<int> cells
    let queue = Array.zeroCreate<int> cells
    let area = Array.zeroCreate<int> maxSnakes
    let nearestFood = Array.zeroCreate<int> maxSnakes

    // Bitboard flood fill (boards up to 128 cells, which covers 7x7 and 11x11): per snake a frontier,
    // plus a "contested" pseudo-player whose frontier carries the -2 owner on, exactly like the scalar BFS.
    let bitsOk = cells <= 128
    let frLo = Array.zeroCreate<uint64> (maxSnakes + 1)
    let frHi = Array.zeroCreate<uint64> (maxSnakes + 1)

    let maskOf (f: int -> bool) (half: int) =
        let mutable m = 0UL

        if bitsOk then
            for c in half * 64 .. min cells (half * 64 + 64) - 1 do
                if f c then m <- m ||| (1UL <<< (c - half * 64))

        m

    let fullLo, fullHi = maskOf (fun _ -> true) 0, maskOf (fun _ -> true) 1
    let nc0Lo, nc0Hi = maskOf (fun c -> geo.X c <> 0) 0, maskOf (fun c -> geo.X c <> 0) 1
    let ncLLo, ncLHi = maskOf (fun c -> geo.X c <> geo.Width - 1) 0, maskOf (fun c -> geo.X c <> geo.Width - 1) 1
    let w1 = geo.Width

    member _.Area(s: int) = area[s]
    /// Distance to the nearest food inside the snake's own territory; Int32.MaxValue when there is none.
    member _.NearestFood(s: int) = nearestFood[s]
    /// Only filled by VoronoiScalar (the bitboard version computes areas and food distances only).
    member _.Owner(c: int) = owner[c]
    member _.Dist(c: int) = dist[c]

    /// Fills Area and NearestFood. Uses the bitboard version when the board fits in 128 cells.
    member this.Voronoi(b: Board) = if bitsOk then this.VoronoiBits b else this.VoronoiScalar b

    /// Same result as VoronoiScalar for Area/NearestFood, one BFS level per iteration over whole bitboards.
    /// A board is two uint64 halves (cells 0-63 in lo, 64-127 in hi); UInt128 was only ~1.3x faster than the
    /// scalar BFS because its variable shifts are branchy, the hand-split version is what makes this pay off.
    /// Both halves in one Vector128 (NEON EXT / SSE byte shifts for the carries) gave the same results at 0.41x the
    /// speed on the A1 and 0.49x on x86: the cross-lane carries and lane extracts cost more than the halved op count.
    member _.VoronoiBits(b: Board) =
        let n = b.NumSnakes
        let mutable onLo = 0UL
        let mutable onHi = 0UL
        let mutable twLo = 0UL
        let mutable twHi = 0UL
        let mutable maxLen = 0

        // Every living segment, from the board's incrementally kept bitboard
        let mutable blLo = b.OccLo
        let mutable blHi = b.OccHi

        for s in 0 .. n - 1 do
            area[s] <- 0
            nearestFood[s] <- Int32.MaxValue
            frLo[s] <- 0UL
            frHi[s] <- 0UL

            if b.Alive s then
                let len = b.Length s
                if len > maxLen then maxLen <- len

                let h = b.Head s
                let hl = if h < 64 then 1UL <<< h else 0UL
                let hh = if h < 64 then 0UL else 1UL <<< (h - 64)
                twLo <- twLo ||| (onLo &&& hl)
                twHi <- twHi ||| (onHi &&& hh)
                onLo <- onLo ||| hl
                onHi <- onHi ||| hh
                frLo[s] <- hl
                frHi[s] <- hh

        // Two heads on one cell: contested from the start
        for s in 0 .. n - 1 do
            frLo[s] <- frLo[s] &&& ~~~twLo
            frHi[s] <- frHi[s] &&& ~~~twHi

        frLo[n] <- twLo
        frHi[n] <- twHi
        let mutable rLo = onLo
        let mutable rHi = onHi
        let fdLo = b.FoodLo
        let fdHi = b.FoodHi

        let w = w1
        let iw = 64 - w1
        let mutable d = 0
        let mutable active = true

        while active do
            d <- d + 1

            // Segment k frees up once d >= len - k. A cell stacked after eating (k and k-1 on the same cell)
            // is freed by its younger copy one level later, matching the scalar max over segments.
            if d < maxLen then
                for s in 0 .. n - 1 do
                    if b.Alive s then
                        let k = b.Length s - d

                        if k >= 1 then
                            let c = b.Body(s, k)

                            if c <> b.Body(s, k - 1) then
                                if c < 64 then
                                    blLo <- blLo &&& ~~~(1UL <<< c)
                                else
                                    blHi <- blHi &&& ~~~(1UL <<< (c - 64))

            let freeLo = fullLo &&& ~~~(rLo ||| blLo)
            let freeHi = fullHi &&& ~~~(rHi ||| blHi)
            onLo <- 0UL
            onHi <- 0UL
            twLo <- 0UL
            twHi <- 0UL

            for p in 0..n do
                let lo = frLo[p]
                let hi = frHi[p]

                if (lo ||| hi) <> 0UL then
                    // left/right shifts by one column (masked against wrapping), up/down by one row
                    let eLo =
                        (((lo <<< 1) &&& nc0Lo)
                         ||| (((lo >>> 1) ||| (hi <<< 63)) &&& ncLLo)
                         ||| (lo <<< w)
                         ||| ((lo >>> w) ||| (hi <<< iw)))
                        &&& freeLo

                    let eHi =
                        ((((hi <<< 1) ||| (lo >>> 63)) &&& nc0Hi)
                         ||| ((hi >>> 1) &&& ncLHi)
                         ||| ((hi <<< w) ||| (lo >>> iw))
                         ||| (hi >>> w))
                        &&& freeHi

                    twLo <- twLo ||| (onLo &&& eLo)
                    twHi <- twHi ||| (onHi &&& eHi)
                    onLo <- onLo ||| eLo
                    onHi <- onHi ||| eHi
                    frLo[p] <- eLo
                    frHi[p] <- eHi

            for s in 0 .. n - 1 do
                let oLo = frLo[s] &&& ~~~twLo
                let oHi = frHi[s] &&& ~~~twHi

                if (oLo ||| oHi) <> 0UL then
                    area[s] <- area[s] + BitOperations.PopCount oLo + BitOperations.PopCount oHi

                    if nearestFood[s] = Int32.MaxValue && ((oLo &&& fdLo) ||| (oHi &&& fdHi)) <> 0UL then
                        nearestFood[s] <- d

                frLo[s] <- oLo
                frHi[s] <- oHi

            frLo[n] <- frLo[n] ||| twLo
            frHi[n] <- frHi[n] ||| twHi
            rLo <- rLo ||| onLo
            rHi <- rHi ||| onHi
            active <- (onLo ||| onHi) <> 0UL

    /// Reference BFS (any board size); also fills Owner and Dist.
    member _.VoronoiScalar(b: Board) =
        let n = b.NumSnakes
        Array.Fill(owner, -1)
        Array.Clear(vacate)

        for s in 0 .. n - 1 do
            area[s] <- 0
            nearestFood[s] <- Int32.MaxValue

            if b.Alive s then
                let len = b.Length s

                for k in 0 .. len - 1 do
                    let c = b.Body(s, k)
                    let v = len - k
                    if v > vacate[c] then vacate[c] <- v

        let mutable qt = 0

        for s in 0 .. n - 1 do
            if b.Alive s then
                let h = b.Head s

                if owner[h] = -1 then
                    owner[h] <- s
                    dist[h] <- 0
                    queue[qt] <- h
                    qt <- qt + 1
                else
                    owner[h] <- -2

        let mutable qh = 0

        while qh < qt do
            let c = queue[qh]
            qh <- qh + 1
            let o = owner[c]
            let d = dist[c] + 1

            for i in 0..3 do
                let nb = geo.Neighbor(c, enum<Dir> i)

                if nb >= 0 then
                    if owner[nb] = -1 then
                        if d >= vacate[nb] then
                            owner[nb] <- o
                            dist[nb] <- d
                            queue[qt] <- nb
                            qt <- qt + 1
                    elif dist[nb] = d && owner[nb] <> o then
                        owner[nb] <- -2

        for i in 0 .. qt - 1 do
            let c = queue[i]
            let o = owner[c]

            if o >= 0 && dist[c] > 0 then
                area[o] <- area[o] + 1

                if b.IsFood c && dist[c] < nearestFood[o] then nearestFood[o] <- dist[c]

/// The raw evaluation features of one snake. Kept separate from the weights so the Texel tuner can compute them once
/// and re-weigh them many times.
[<Struct>]
type Features =
    {
        MyArea: int
        MaxOppArea: int
        MyLen: int
        MaxOppLen: int
        /// Living opponents
        Others: int
        Hp: int
        /// Distance to the nearest food in my territory, Int32.MaxValue when there is none
        NearestFood: int
    }

module Eval =
    /// Score for a dead snake / the only survivor.
    [<Literal>]
    let Loss = -1e6

    [<Literal>]
    let Win = 1e6

    /// The evaluation formula.
    let combine (w: Weights) (f: Features) =
        let cap = int w.LenCap
        let lenDiff = if f.Others = 0 then 0 else Math.Clamp(f.MyLen - f.MaxOppLen, -cap, cap)
        let mutable score = w.Area * float f.MyArea - w.OppArea * float f.MaxOppArea + w.Len * float lenDiff

        // Not enough territory to hold our own body: we will most likely get trapped
        let need = w.TrapRatio * float f.MyLen

        if float f.MyArea < need then score <- score - w.Trapped * (need - float f.MyArea)

        let nf = f.NearestFood
        let hp = f.Hp

        if nf = Int32.MaxValue then
            if float hp < w.NoFoodHp then score <- score - w.NoFood * (w.NoFoodHp - float hp)
        else
            if nf >= hp then score <- score - w.Starve

            let fw =
                if float hp < w.HungryHp then w.FoodHungry
                elif float lenDiff < w.LeadLen then w.FoodBehind
                else w.FoodAhead

            score <- score - fw * float nf

        score

    /// Features of snake me from a filled Territory (Territory.Voronoi b already run).
    let features (t: Territory) (b: Board) (me: int) =
        let mutable others = 0
        let mutable maxOppArea = 0
        let mutable maxOppLen = 0

        for s in 0 .. b.NumSnakes - 1 do
            if s <> me && b.Alive s then
                others <- others + 1
                maxOppArea <- max maxOppArea (t.Area s)
                maxOppLen <- max maxOppLen (b.Length s)

        { MyArea = t.Area me
          MaxOppArea = maxOppArea
          MyLen = b.Length me
          MaxOppLen = maxOppLen
          Others = others
          Hp = b.Health me
          NearestFood = t.NearestFood me }

    /// Score from snake me's point of view: Loss if dead, Win if the only survivor, else the weighted features.
    /// `t` is scratch space (its contents are overwritten).
    let evaluate (w: Weights) (t: Territory) (b: Board) (me: int) : float =
        if not (b.Alive me) then
            Loss
        else
            let mutable others = 0

            for s in 0 .. b.NumSnakes - 1 do
                if s <> me && b.Alive s then others <- others + 1

            if b.NumSnakes > 1 && others = 0 then
                Win
            else
                t.Voronoi b
                combine w (features t b me)

/// Baseline policy: one-ply paranoid search. For each of my moves, enumerate every combination of opponent replies
/// (excluding off-board moves and turning back onto the neck) and take the worst case; break ties on the average.
module Policy =
    /// A decision function with its own scratch space (not thread-safe; make one per thread).
    let create (geo: Geometry) (maxSnakes: int) : Board -> int -> Dir =
        let territory = Territory(geo, maxSnakes)
        let scratch = Board(geo, maxSnakes)
        let moves = Array.zeroCreate<Dir> maxSnakes
        let opp = Array.zeroCreate<int> maxSnakes
        let oppMoves = Array.zeroCreate<Dir> (maxSnakes * 4)
        let oppCount = Array.zeroCreate<int> maxSnakes
        let odo = Array.zeroCreate<int> maxSnakes

        fun (b: Board) (me: int) ->
            let n = b.NumSnakes
            let mutable no = 0

            for s in 0 .. n - 1 do
                if s <> me && b.Alive s then
                    opp[no] <- s
                    let h = b.Head s
                    let neck = if b.Length s > 1 then b.Body(s, 1) else -1
                    let mutable k = 0

                    for d in Dir.all do
                        let nb = geo.Neighbor(h, d)

                        if nb >= 0 && nb <> neck then
                            oppMoves[no * 4 + k] <- d
                            k <- k + 1

                    if k = 0 then
                        oppMoves[no * 4] <- Dir.Up
                        k <- 1

                    oppCount[no] <- k
                    no <- no + 1

            let myHead = b.Head me
            let mutable best = Dir.Up
            let mutable bestWorst = Double.NegativeInfinity
            let mutable bestAvg = Double.NegativeInfinity

            for d in Dir.all do
                if geo.Neighbor(myHead, d) >= 0 then
                    moves[me] <- d
                    Array.Clear(odo)
                    let mutable worst = Double.PositiveInfinity
                    let mutable sum = 0.0
                    let mutable cnt = 0
                    let mutable fin = false

                    while not fin do
                        for i in 0 .. no - 1 do
                            moves[opp[i]] <- oppMoves[i * 4 + odo[i]]

                        scratch.CopyFrom b
                        scratch.Step moves
                        let v = Eval.evaluate Weights.defaults territory scratch me
                        worst <- min worst v
                        sum <- sum + v
                        cnt <- cnt + 1
                        // Odometer: advance to the next combination of opponent moves
                        let mutable i = 0
                        let mutable carry = true

                        while carry && i < no do
                            odo[i] <- odo[i] + 1

                            if odo[i] = oppCount[i] then
                                odo[i] <- 0
                                i <- i + 1
                            else
                                carry <- false

                        if carry then fin <- true

                    let avg = sum / float cnt

                    if worst > bestWorst || (worst = bestWorst && avg > bestAvg) then
                        best <- d
                        bestWorst <- worst
                        bestAvg <- avg

            best
