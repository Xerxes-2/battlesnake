namespace Snake.Engine

open System

/// Directions use the official Battlesnake coordinates: y=0 is the bottom row, and Up means y+1.
type Dir =
    | Up = 0
    | Down = 1
    | Left = 2
    | Right = 3

module Dir =
    let all = [| Dir.Up; Dir.Down; Dir.Left; Dir.Right |]

    let toApi (d: Dir) =
        match d with
        | Dir.Up -> "up"
        | Dir.Down -> "down"
        | Dir.Left -> "left"
        | _ -> "right"

/// Board geometry. Cells are numbered cell = y * width + x. The neighbour table is precomputed and shared by every Board.
[<Sealed>]
type Geometry(width: int, height: int) =
    let cells = width * height

    let nbr =
        Array.init (cells * 4) (fun i ->
            let c, d = i / 4, i % 4
            let x, y = c % width, c / width

            match d with
            | 0 -> if y + 1 < height then c + width else -1
            | 1 -> if y > 0 then c - width else -1
            | 2 -> if x > 0 then c - 1 else -1
            | _ -> if x + 1 < width then c + 1 else -1)

    // Zobrist keys for Board.Hash. Snake slots beyond zSnakes share keys (only costs hash quality).
    static let zSnakes = 16
    static let zSeed = 12345
    let zrng = Random(zSeed)
    let z () = uint64 (zrng.NextInt64()) ^^^ (uint64 (zrng.NextInt64()) <<< 1)
    let zHead = Array.init (zSnakes * cells) (fun _ -> z ())
    let zBody = Array.init (zSnakes * cells) (fun _ -> z ())
    let zHp = Array.init (zSnakes * 101) (fun _ -> z ())
    let zLen = Array.init (zSnakes * 256) (fun _ -> z ())
    let zFood = Array.init cells (fun _ -> z ())
    let zHaz = Array.init cells (fun _ -> z ())

    member _.Width = width
    member _.Height = height
    member _.Cells = cells
    member _.Cell(x: int, y: int) = y * width + x
    member _.X(c: int) = c % width
    member _.Y(c: int) = c / width

    /// Returns -1 when the neighbour is off the board.
    member _.Neighbor(c: int, d: Dir) = nbr.[c * 4 + int d]

    member _.ZHead(s: int, c: int) = zHead.[(s &&& (zSnakes - 1)) * cells + c]
    member _.ZBody(s: int, c: int) = zBody.[(s &&& (zSnakes - 1)) * cells + c]
    /// Health and length together (they always change together in Step)
    member _.ZStats(s: int, hp: int, len: int) =
        let s = s &&& (zSnakes - 1)
        zHp.[s * 101 + Math.Clamp(hp, 0, 100)] ^^^ zLen.[s * 256 + min 255 len]

    member _.ZFood(c: int) = zFood.[c]
    member _.ZHaz(c: int) = zHaz.[c]

/// Mutable game state used by search. Copy-make: search copies with CopyFrom and then calls Step; there is no undo.
/// Each snake's body is stored in a ring buffer: segment k is at ring[s*ringSize + (headPos+k) & mask].
[<Sealed; AllowNullLiteral>]
type Board(geo: Geometry, maxSnakes: int) =
    let cells = geo.Cells

    let ringSize =
        let mutable r = 1

        while r < cells + 4 do
            r <- r * 2

        r

    let ringMask = ringSize - 1
    let health = Array.zeroCreate<int> maxSnakes
    let length = Array.zeroCreate<int> maxSnakes
    let headPos = Array.zeroCreate<int> maxSnakes
    let alive = Array.zeroCreate<bool> maxSnakes
    let ring = Array.zeroCreate<int16> (maxSnakes * ringSize)
    let food = Array.zeroCreate<bool> cells
    let hazard = Array.zeroCreate<byte> cells
    /// Number of body segments (heads included) on each cell, counting only living snakes.
    let occ = Array.zeroCreate<byte> cells
    // Scratch space for Step
    let newHead = Array.zeroCreate<int> maxSnakes
    let elim = Array.zeroCreate<bool> maxSnakes
    let mutable numSnakes = 0
    let mutable turn = 0
    let mutable hazardDamage = 0
    /// Zobrist hash of everything the game's future depends on except the turn number: living snakes' heads,
    /// bodies, health and length, food and hazards. Maintained incrementally; see ComputeHash.
    let mutable hash = 0UL

    member internal _.HealthArr = health
    member internal _.LengthArr = length
    member internal _.HeadPosArr = headPos
    member internal _.AliveArr = alive
    member internal _.RingArr = ring
    member internal _.FoodArr = food
    member internal _.HazardArr = hazard
    member internal _.OccArr = occ

    member _.Geometry = geo
    member _.MaxSnakes = maxSnakes
    member _.NumSnakes = numSnakes

    member _.Turn
        with get () = turn
        and set v = turn <- v

    member _.HazardDamage
        with get () = hazardDamage
        and set v = hazardDamage <- v

    member _.Hash = hash

    /// From-scratch version of Hash (reference for tests).
    member this.ComputeHash() =
        let mutable h = 0UL

        for s in 0 .. numSnakes - 1 do
            if alive.[s] then
                h <- h ^^^ geo.ZHead(s, this.Head s) ^^^ geo.ZStats(s, health.[s], length.[s])

                for k in 1 .. length.[s] - 1 do
                    h <- h ^^^ geo.ZBody(s, this.Body(s, k))

        for c in 0 .. cells - 1 do
            if food.[c] then h <- h ^^^ geo.ZFood c
            if hazard.[c] > 0uy then h <- h ^^^ geo.ZHaz c

        h

    member _.Health(s: int) = health.[s]
    member _.Length(s: int) = length.[s]
    member _.Alive(s: int) = alive.[s]
    member _.Body(s: int, k: int) = int ring.[s * ringSize + ((headPos.[s] + k) &&& ringMask)]
    member _.Head(s: int) = int ring.[s * ringSize + headPos.[s]]
    member _.IsFood(c: int) = food.[c]
    member _.Hazard(c: int) = int hazard.[c]
    member _.Occupancy(c: int) = int occ.[c]

    member _.AliveCount =
        let mutable n = 0

        for s in 0 .. numSnakes - 1 do
            if alive.[s] then n <- n + 1

        n

    member _.SetFood(c: int, v: bool) =
        if food.[c] <> v then
            food.[c] <- v
            hash <- hash ^^^ geo.ZFood c

    member _.AddHazard(c: int) =
        if hazard.[c] = 0uy then hash <- hash ^^^ geo.ZHaz c
        hazard.[c] <- hazard.[c] + 1uy

    /// body.[0] is the head. Returns the snake's index.
    member _.AddSnake(hp: int, body: int[]) : int =
        if numSnakes >= maxSnakes then invalidOp "too many snakes"
        if body.Length = 0 || body.Length > ringSize then invalidArg (nameof body) "bad body length"
        let s = numSnakes
        numSnakes <- numSnakes + 1
        health.[s] <- hp
        length.[s] <- body.Length
        headPos.[s] <- 0
        alive.[s] <- true

        for k in 0 .. body.Length - 1 do
            ring.[s * ringSize + k] <- int16 body.[k]
            occ.[body.[k]] <- occ.[body.[k]] + 1uy
            hash <- hash ^^^ (if k = 0 then geo.ZHead(s, body.[0]) else geo.ZBody(s, body.[k]))

        hash <- hash ^^^ geo.ZStats(s, hp, body.Length)
        s

    member _.CopyFrom(o: Board) =
        numSnakes <- o.NumSnakes
        turn <- o.Turn
        hazardDamage <- o.HazardDamage
        hash <- o.Hash
        Array.Copy(o.HealthArr, health, numSnakes)
        Array.Copy(o.LengthArr, length, numSnakes)
        Array.Copy(o.HeadPosArr, headPos, numSnakes)
        Array.Copy(o.AliveArr, alive, numSnakes)
        Array.Copy(o.RingArr, ring, numSnakes * ringSize)
        Array.Copy(o.FoodArr, food, cells)
        Array.Copy(o.HazardArr, hazard, cells)
        Array.Copy(o.OccArr, occ, cells)

    member this.Clone() =
        let b = Board(geo, maxSnakes)
        b.CopyFrom this
        b

    /// Removes the snake's segments; its health/length hash term is handled by Step.
    member private _.Kill(s: int) =
        alive.[s] <- false
        let b = s * ringSize

        for k in 0 .. length.[s] - 1 do
            let c = int ring.[b + ((headPos.[s] + k) &&& ringMask)]
            occ.[c] <- occ.[c] - 1uy
            hash <- hash ^^^ (if k = 0 then geo.ZHead(s, c) else geo.ZBody(s, c))

    /// Advances one turn following the official standard rules. moves.[s] is snake s's move; entries for dead snakes are ignored.
    /// Order: move -> health -1 -> hazard damage -> eat -> eliminate (out of bounds/starvation first, then collisions with the survivors).
    /// Random food spawning is not simulated.
    member this.Step(moves: Dir[]) =
        turn <- turn + 1

        // 1. Move: push the new head, pop the tail
        for s in 0 .. numSnakes - 1 do
            elim.[s] <- false

            if alive.[s] then
                // Health and length change below; their hash term is put back at the end for survivors
                hash <- hash ^^^ geo.ZStats(s, health.[s], length.[s])
                let b = s * ringSize
                let h = int ring.[b + headPos.[s]]
                let tailIdx = b + ((headPos.[s] + length.[s] - 1) &&& ringMask)
                let tail = int ring.[tailIdx]
                // Old head becomes body segment 1, the tail segment leaves (nothing changes for length 1)
                if length.[s] > 1 then hash <- hash ^^^ geo.ZBody(s, h) ^^^ geo.ZBody(s, tail)
                occ.[tail] <- occ.[tail] - 1uy
                let nh = geo.Neighbor(h, moves.[s])
                newHead.[s] <- nh
                headPos.[s] <- (headPos.[s] - 1) &&& ringMask
                // When the move goes off the board, leave a placeholder on the old head cell so Kill's bookkeeping stays balanced
                let c = if nh >= 0 then nh else h
                ring.[b + headPos.[s]] <- int16 c
                occ.[c] <- occ.[c] + 1uy
                hash <- hash ^^^ geo.ZHead(s, h) ^^^ geo.ZHead(s, c)
                health.[s] <- health.[s] - 1

        // 2. Hazard damage (none on the standard ladder; no damage when there is food on the cell)
        if hazardDamage > 0 then
            for s in 0 .. numSnakes - 1 do
                let nh = newHead.[s]

                if alive.[s] && nh >= 0 && hazard.[nh] > 0uy && not food.[nh] then
                    health.[s] <- health.[s] - hazardDamage * int hazard.[nh]

        // 3. Eat: health back to full and the tail is duplicated. Several snakes can eat the same food; it is removed afterwards
        let mutable ate = false

        for s in 0 .. numSnakes - 1 do
            let nh = newHead.[s]

            if alive.[s] && nh >= 0 && food.[nh] then
                ate <- true
                health.[s] <- 100
                let b = s * ringSize
                let tail = ring.[b + ((headPos.[s] + length.[s] - 1) &&& ringMask)]
                ring.[b + ((headPos.[s] + length.[s]) &&& ringMask)] <- tail
                length.[s] <- length.[s] + 1
                occ.[int tail] <- occ.[int tail] + 1uy
                hash <- hash ^^^ geo.ZBody(s, int tail)

        if ate then
            for s in 0 .. numSnakes - 1 do
                let nh = newHead.[s]

                if alive.[s] && nh >= 0 && food.[nh] then
                    food.[nh] <- false
                    hash <- hash ^^^ geo.ZFood nh

        // 4a. Eliminate out-of-bounds and starved snakes first; they are no longer obstacles for collision checks
        for s in 0 .. numSnakes - 1 do
            if alive.[s] && (newHead.[s] < 0 || health.[s] <= 0) then
                this.Kill s

        // 4b. Collisions: decide them all against the same state first, then apply together
        for s in 0 .. numSnakes - 1 do
            if alive.[s] then
                let h = newHead.[s]
                let mutable heads = 0

                for t in 0 .. numSnakes - 1 do
                    if alive.[t] && newHead.[t] = h then heads <- heads + 1

                if int occ.[h] > heads then
                    // Some non-head segment is on h: a self collision or a hit on another body
                    elim.[s] <- true
                elif heads > 1 then
                    for t in 0 .. numSnakes - 1 do
                        if t <> s && alive.[t] && newHead.[t] = h && length.[s] <= length.[t] then
                            elim.[s] <- true

        for s in 0 .. numSnakes - 1 do
            if elim.[s] && alive.[s] then this.Kill s

        for s in 0 .. numSnakes - 1 do
            if alive.[s] then hash <- hash ^^^ geo.ZStats(s, health.[s], length.[s])
