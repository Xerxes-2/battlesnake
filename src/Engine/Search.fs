namespace Snake.Engine

open System

/// Evaluation weights. Kept as a flat array so the tuner can perturb them generically; the names are the
/// stable interface (weight files, logs). Defaults are the hand-picked starting point.
[<Sealed>]
type Weights(values: float[]) =
    static let names =
        [| "area"; "oppArea"; "len"; "lenCap"; "trapped"; "starve"; "noFoodHp"; "noFood"
           "hungryHp"; "foodHungry"; "foodBehind"; "foodAhead"; "leadLen" |]

    static let defaults = [| 10.0; 4.0; 30.0; 4.0; 50.0; 5000.0; 30.0; 10.0; 30.0; 8.0; 4.0; 1.0; 2.0 |]

    static member Names = names
    static member Default = Weights(Array.copy defaults)
    /// Tuned by `Arena tune --mode duel --nodes 5000` (SPSA, ~19k games); copy of tune/weights-duel.txt.
    /// Area and oppArea are nearly collinear 1v1 (they sum to roughly the free board), so their split is arbitrary.
    static member val Duel = Weights.Parse "area=0.726,oppArea=1.017,len=72.235,lenCap=5.935,trapped=37.321,starve=3635.142,noFoodHp=35.183,noFood=9.555,hungryHp=41.814,foodHungry=8.704,foodBehind=4.158,foodAhead=0.414,leadLen=1.4"
    /// Tuned by `Arena tune --mode standard --nodes 5000` (SPSA, ~10k games); copy of tune/weights-standard.txt
    static member val Standard = Weights.Parse "area=2.616,oppArea=0.599,len=60.101,lenCap=5.895,trapped=1.078,starve=5854.651,noFoodHp=23.906,noFood=10.768,hungryHp=27.447,foodHungry=9.451,foodBehind=2.741,foodAhead=2.194,leadLen=3.089"
    member _.Values = values
    member _.Area = values.[0]
    member _.OppArea = values.[1]
    member _.Len = values.[2]
    member _.LenCap = values.[3]
    member _.Trapped = values.[4]
    member _.Starve = values.[5]
    member _.NoFoodHp = values.[6]
    member _.NoFood = values.[7]
    member _.HungryHp = values.[8]
    member _.FoodHungry = values.[9]
    member _.FoodBehind = values.[10]
    member _.FoodAhead = values.[11]
    member _.LeadLen = values.[12]

    /// "name=value,name=value" (one per line also works); unknown names are an error, missing ones keep defaults.
    static member Parse(text: string) =
        let v = Array.copy defaults

        for part in text.Split([| ','; '\n'; ';' |], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) do
            match part.Split('=') with
            | [| k; x |] ->
                match Array.tryFindIndex ((=) (k.Trim())) names with
                | Some i -> v.[i] <- Double.Parse(x.Trim(), Globalization.CultureInfo.InvariantCulture)
                | None -> failwith $"unknown weight {k}"
            | _ -> failwith $"bad weight entry {part}"

        Weights v

    override _.ToString() =
        Array.map2 (fun (n: string) (x: float) -> n + "=" + x.ToString("0.###", Globalization.CultureInfo.InvariantCulture)) names values
        |> String.concat ","

/// Board analysis: Voronoi territory that accounts for when bodies vacate.
/// The body segment at index k (counting from the head) on a snake of length L leaves after L-k turns.
/// So a cell is only passable once the arrival distance d >= its vacate time.
/// All buffers are preallocated, so evaluation does not allocate.
[<Sealed>]
type Analyzer(geo: Geometry, maxSnakes: int, w: Weights) =
    let cells = geo.Cells
    let dist = Array.zeroCreate<int> cells
    /// -1 unreached, -2 reached by several snakes at the same time (contested), otherwise the snake index
    let owner = Array.zeroCreate<int> cells
    let vacate = Array.zeroCreate<int> cells
    let queue = Array.zeroCreate<int> cells
    let area = Array.zeroCreate<int> maxSnakes
    let nearestFood = Array.zeroCreate<int> maxSnakes

    new(geo, maxSnakes) = Analyzer(geo, maxSnakes, Weights.Default)

    member _.Area(s: int) = area.[s]
    /// Distance to the nearest food inside the snake's own territory; Int32.MaxValue when there is none.
    member _.NearestFood(s: int) = nearestFood.[s]
    member _.Owner(c: int) = owner.[c]
    member _.Dist(c: int) = dist.[c]

    member _.Voronoi(b: Board) =
        let n = b.NumSnakes
        Array.Fill(owner, -1)
        Array.Clear(vacate)

        for s in 0 .. n - 1 do
            area.[s] <- 0
            nearestFood.[s] <- Int32.MaxValue

            if b.Alive s then
                let len = b.Length s

                for k in 0 .. len - 1 do
                    let c = b.Body(s, k)
                    let v = len - k
                    if v > vacate.[c] then vacate.[c] <- v

        let mutable qt = 0

        for s in 0 .. n - 1 do
            if b.Alive s then
                let h = b.Head s

                if owner.[h] = -1 then
                    owner.[h] <- s
                    dist.[h] <- 0
                    queue.[qt] <- h
                    qt <- qt + 1
                else
                    owner.[h] <- -2

        let mutable qh = 0

        while qh < qt do
            let c = queue.[qh]
            qh <- qh + 1
            let o = owner.[c]
            let d = dist.[c] + 1

            for i in 0..3 do
                let nb = geo.Neighbor(c, enum<Dir> i)

                if nb >= 0 then
                    if owner.[nb] = -1 then
                        if d >= vacate.[nb] then
                            owner.[nb] <- o
                            dist.[nb] <- d
                            queue.[qt] <- nb
                            qt <- qt + 1
                    elif dist.[nb] = d && owner.[nb] <> o then
                        owner.[nb] <- -2

        for i in 0 .. qt - 1 do
            let c = queue.[i]
            let o = owner.[c]

            if o >= 0 && dist.[c] > 0 then
                area.[o] <- area.[o] + 1

                if b.IsFood c && dist.[c] < nearestFood.[o] then
                    nearestFood.[o] <- dist.[c]

    /// Score from snake me's point of view. Death = -1e6, being the only survivor = +1e6.
    member this.Evaluate(b: Board, me: int) : float =
        if not (b.Alive me) then
            -1e6
        else
            let n = b.NumSnakes
            let mutable others = 0

            for s in 0 .. n - 1 do
                if s <> me && b.Alive s then others <- others + 1

            if n > 1 && others = 0 then
                1e6
            else
                this.Voronoi b
                let myLen = b.Length me
                let mutable maxOppArea = 0
                let mutable maxOppLen = 0

                for s in 0 .. n - 1 do
                    if s <> me && b.Alive s then
                        maxOppArea <- max maxOppArea area.[s]
                        maxOppLen <- max maxOppLen (b.Length s)

                let cap = int w.LenCap
                let lenDiff = if others = 0 then 0 else Math.Clamp(myLen - maxOppLen, -cap, cap)
                let myArea = area.[me]
                let mutable score = w.Area * float myArea - w.OppArea * float maxOppArea + w.Len * float lenDiff

                // Not enough territory to hold our own body: we will most likely get trapped
                if myArea < myLen then
                    score <- score - w.Trapped * float (myLen - myArea)

                let hp = b.Health me
                let nf = nearestFood.[me]

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

/// Baseline policy: one-ply paranoid search. For each of my moves, enumerate every combination of opponent replies
/// (excluding off-board moves and turning back onto the neck) and take the worst case; break ties on the average.
[<Sealed>]
type Policy(geo: Geometry, maxSnakes: int) =
    let analyzer = Analyzer(geo, maxSnakes)
    let scratch = Board(geo, maxSnakes)
    let moves = Array.zeroCreate<Dir> maxSnakes
    let opp = Array.zeroCreate<int> maxSnakes
    let oppMoves = Array.zeroCreate<Dir> (maxSnakes * 4)
    let oppCount = Array.zeroCreate<int> maxSnakes
    let odo = Array.zeroCreate<int> maxSnakes

    member _.Analyzer = analyzer

    member _.Decide(b: Board, me: int) : Dir =
        let n = b.NumSnakes
        let mutable no = 0

        for s in 0 .. n - 1 do
            if s <> me && b.Alive s then
                opp.[no] <- s
                let h = b.Head s
                let neck = if b.Length s > 1 then b.Body(s, 1) else -1
                let mutable k = 0

                for d in Dir.all do
                    let nb = geo.Neighbor(h, d)

                    if nb >= 0 && nb <> neck then
                        oppMoves.[no * 4 + k] <- d
                        k <- k + 1

                if k = 0 then
                    oppMoves.[no * 4] <- Dir.Up
                    k <- 1

                oppCount.[no] <- k
                no <- no + 1

        let myHead = b.Head me
        let mutable best = Dir.Up
        let mutable bestWorst = Double.NegativeInfinity
        let mutable bestAvg = Double.NegativeInfinity

        for d in Dir.all do
            if geo.Neighbor(myHead, d) >= 0 then
                moves.[me] <- d
                Array.Clear(odo)
                let mutable worst = Double.PositiveInfinity
                let mutable sum = 0.0
                let mutable cnt = 0
                let mutable fin = false

                while not fin do
                    for i in 0 .. no - 1 do
                        moves.[opp.[i]] <- oppMoves.[i * 4 + odo.[i]]

                    scratch.CopyFrom b
                    scratch.Step moves
                    let v = analyzer.Evaluate(scratch, me)
                    worst <- min worst v
                    sum <- sum + v
                    cnt <- cnt + 1
                    // Odometer: advance to the next combination of opponent moves
                    let mutable i = 0
                    let mutable carry = true

                    while carry && i < no do
                        odo.[i] <- odo.[i] + 1

                        if odo.[i] = oppCount.[i] then
                            odo.[i] <- 0
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
