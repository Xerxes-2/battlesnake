/// Texel-style tuning on ladder games: fit the evaluation weights so that sigmoid(eval / K) predicts how each snake
/// actually finished. Self-play can't teach what the ladder's top snakes punish; their games can.
module Texel

open System
open System.IO
open System.Text.Json
open System.Threading.Tasks
open Snake.Engine

/// The evaluation's raw features for one snake in one frame; the weights only re-weigh these.
[<Struct>]
type Sample =
    {
        F: Features
        /// How the snake finished: duel 1/0.5/0; standard the share of the other snakes it outlived (ties half)
        Label: float32
        /// Validation split, by game
        Valid: bool
    }

let private score (w: Weights) (s: Sample) = Eval.combine w s.F

/// Features of every alive snake in every frame of one saved game (tune/fetch_games.py format), for games with
/// `players` snakes on 11x11 standard rules.
let private gameSamples (players: int) (path: string) : Sample[] =
    use doc = JsonDocument.Parse(File.ReadAllText path)
    let r = doc.RootElement
    let mutable p = Unchecked.defaultof<JsonElement>

    if
        not (r.TryGetProperty("n", &p))
        || p.GetInt32() <> players
        || r.GetProperty("w").GetInt32() <> 11
        || r.GetProperty("h").GetInt32() <> 11
        || r.GetProperty("ruleset").GetString() <> "standard"
    then
        [||]
    else
        let geo = Geometry(11, 11)
        let an = Territory(geo, players)
        // Stable across runs (string.GetHashCode is randomized per process); ids are random hex
        let valid = Convert.ToInt32((Path.GetFileName path).Substring(0, 2), 16) % 5 = 0
        let out = ResizeArray()

        for f in r.GetProperty("frames").EnumerateArray() do
            let turn = f.GetProperty("t").GetInt32()
            let snakes = f.GetProperty("snakes")
            let alive = snakes.GetArrayLength()

            if turn >= 3 && alive >= 2 then
                let b = Board(geo, players)
                let xy (a: JsonElement) i = geo.Cell(a[i].GetInt32(), a[i + 1].GetInt32())
                let deaths = ResizeArray()

                for s in snakes.EnumerateArray() do
                    let body = s.GetProperty("body")

                    b.AddSnake(
                        s.GetProperty("hp").GetInt32(),
                        [| for i in 0..2 .. body.GetArrayLength() - 1 -> xy body i |]
                    )
                    |> ignore

                    deaths.Add(
                        match s.TryGetProperty "dt" with
                        | true, d -> d.GetInt32()
                        | _ -> Int32.MaxValue
                    )

                let food = f.GetProperty("food")

                for i in 0..2 .. food.GetArrayLength() - 1 do
                    b.SetFood(xy food i, true)

                b.Turn <- turn
                an.Voronoi b
                let deadBefore = players - alive

                for me in 0 .. alive - 1 do
                    let outlived =
                        float deadBefore
                        + (Seq.sum
                           <| seq {
                               for s in 0 .. alive - 1 do
                                   if s <> me then
                                       if deaths[s] < deaths[me] then 1.0
                                       elif deaths[s] = deaths[me] then 0.5
                           })

                    out.Add
                        { F = Eval.features an b me
                          Label = float32 (outlived / float (players - 1))
                          Valid = valid }

        out.ToArray()

let private load (dir: string) (players: int) =
    let files = Directory.GetFiles(dir, "*.json")
    let parts = Array.zeroCreate files.Length
    Parallel.For(0, files.Length, fun i -> parts[i] <- gameSamples players files[i]) |> ignore
    let games = parts |> Array.filter (fun a -> a.Length > 0)
    let all = Array.concat games
    printfn "%d games, %d samples (%d validation)" games.Length all.Length (all |> Array.filter _.Valid |> Array.length)
    all

/// Mean squared error of sigmoid(eval / k) against the labels, over training or validation samples.
/// `bias` is an intercept for the fit only: survivors outnumber the dead in the samples (dead snakes leave), so the
/// labels average above 0.5, and without it the tuner inflates `area` (always positive) to shift every prediction.
let private error (samples: Sample[]) (valid: bool) (w: Weights) (k: float) (bias: float) =
    let chunks = Environment.ProcessorCount
    let sums = Array.zeroCreate<float> chunks
    let counts = Array.zeroCreate<int> chunks

    Parallel.For(
        0,
        chunks,
        fun c ->
            let mutable e = 0.0
            let mutable n = 0

            for i in c..chunks .. samples.Length - 1 do
                let s = samples[i]

                if s.Valid = valid then
                    let p = 1.0 / (1.0 + exp (-(score w s + bias) / k))
                    let d = float s.Label - p
                    e <- e + d * d
                    n <- n + 1

            sums[c] <- e
            counts[c] <- n
    )
    |> ignore

    Array.sum sums / float (Array.sum counts)

/// Golden-section search for the scale K that best fits the given weights.
let private golden (f: float -> float) lo0 hi0 =
    let mutable lo = lo0
    let mutable hi = hi0

    for _ in 1..40 do
        let a = lo + (hi - lo) * 0.382
        let b = lo + (hi - lo) * 0.618
        if f a < f b then hi <- b else lo <- a

    (lo + hi) / 2.0

/// Scale K, then the intercept, that best fit the given weights.
let private fitKBias (samples: Sample[]) (w: Weights) =
    let mutable k = 400.0
    let mutable bias = 0.0

    for _ in 1..3 do
        k <- golden (fun k -> error samples false w k bias) 10.0 5000.0
        bias <- golden (fun b -> error samples false w k b) -3000.0 3000.0

    k, bias

let run (argv: string[]) =
    let dir = argv[1]
    let mode = argv[2]
    let players = if mode = "duel" then 2 else 4

    let start =
        if argv.Length > 3 then Weights.parse argv[3]
        elif mode = "duel" then Weights.duel
        else Weights.standard

    let samples = load dir players
    let k, bias0 = fitKBias samples start

    let show (w: Weights) =
        String.Join(",", Array.map2 (fun (n: string) (v: float) -> $"{n}={v:G4}") Weights.names (Weights.toArray w))

    let mean = samples |> Array.averageBy (fun s -> float s.Label)

    let constErr valid =
        samples |> Array.filter (fun s -> s.Valid = valid) |> Array.averageBy (fun s -> (float s.Label - mean) ** 2.0)

    printfn
        "K = %.1f bias %.1f  train %.6f  valid %.6f  (always predicting the mean %.3f: train %.6f valid %.6f)"
        k
        bias0
        (error samples false start k bias0)
        (error samples true start k bias0)
        mean
        (constErr false)
        (constErr true)

    // Texel local search: try each weight up and down by a relative step, keep improvements, shrink the step when
    // nothing helps. Relative steps because the weights differ by orders of magnitude.
    let v = Weights.toArray start
    let mutable bias = bias0
    let mutable best = error samples false start k bias
    let mutable step = 0.2

    while step > 0.005 do
        let mutable improved = true

        while improved do
            improved <- false

            for i in 0 .. v.Length - 1 do
                for dir in [ 1.0 + step; 1.0 / (1.0 + step) ] do
                    let old = v[i]
                    v[i] <- old * dir
                    let e = error samples false (Weights.ofArray v) k bias

                    if e < best - 1e-9 then
                        best <- e
                        improved <- true
                    else
                        v[i] <- old

            for d in [ step * k; -step * k ] do
                let e = error samples false (Weights.ofArray v) k (bias + d)

                if e < best - 1e-9 then
                    best <- e
                    bias <- bias + d
                    improved <- true

        printfn "step %.3f  train %.6f  valid %.6f" step best (error samples true (Weights.ofArray v) k bias)

        step <- step / 2.0

    printfn "bias %.1f" bias
    printfn "result: %s" (show (Weights.ofArray v))

// ---------------------------------------------------------------------------------------------------------------
// Feature screening: dump candidate features for every alive snake in every frame, to test in numpy which of them
// add predictive power on top of the current evaluation (tune/texel/screen.py).

let private columns =
    [| "valid"
       "players"
       "place"
       "die10"
       "die5"
       "turn"
       "me"
       "area"
       "oppArea"
       "len"
       "oppLen"
       "others"
       "hp"
       "nf"
       "eval"
       "deg"
       "solo"
       "frontier"
       "near3"
       "headThreat"
       "oppHeadDist"
       "tailDist"
       "wallDist"
       "contested"
       "minArea2"
       "soloMinusArea" |]

let private dumpGame (players: int) (path: string) : float32[][] =
    use doc = JsonDocument.Parse(File.ReadAllText path)
    let r = doc.RootElement
    let mutable p = Unchecked.defaultof<JsonElement>

    if
        not (r.TryGetProperty("n", &p))
        || p.GetInt32() <> players
        || r.GetProperty("w").GetInt32() <> 11
        || r.GetProperty("h").GetInt32() <> 11
        || r.GetProperty("ruleset").GetString() <> "standard"
    then
        [||]
    else
        let geo = Geometry(11, 11)
        let w = Weights.forSnakes players
        let an = Territory(geo, players)
        let valid = if Convert.ToInt32((Path.GetFileName path).Substring(0, 2), 16) % 5 = 0 then 1f else 0f
        let cells = geo.Cells
        let vac = Array.zeroCreate<int> cells
        let sd = Array.zeroCreate<int> cells
        let q = Array.zeroCreate<int> cells
        let out = ResizeArray()

        for f in r.GetProperty("frames").EnumerateArray() do
            let turn = f.GetProperty("t").GetInt32()
            let snakes = f.GetProperty("snakes")
            let alive = snakes.GetArrayLength()

            if turn >= 3 && alive >= 2 then
                let b = Board(geo, players)
                let xy (a: JsonElement) i = geo.Cell(a[i].GetInt32(), a[i + 1].GetInt32())
                let deaths = ResizeArray()
                let mine = ResizeArray()

                for s in snakes.EnumerateArray() do
                    let body = s.GetProperty("body")

                    b.AddSnake(
                        s.GetProperty("hp").GetInt32(),
                        [| for i in 0..2 .. body.GetArrayLength() - 1 -> xy body i |]
                    )
                    |> ignore

                    deaths.Add(
                        match s.TryGetProperty "dt" with
                        | true, d -> d.GetInt32()
                        | _ -> Int32.MaxValue
                    )

                    mine.Add(s.GetProperty("a").GetString() = "Xerxes-2")

                let food = f.GetProperty("food")

                for i in 0..2 .. food.GetArrayLength() - 1 do
                    b.SetFood(xy food i, true)

                b.Turn <- turn
                an.VoronoiScalar b

                // vacate time of every body cell, as in the Voronoi
                Array.Clear vac

                for s in 0 .. alive - 1 do
                    let len = b.Length s

                    for k in 0 .. len - 1 do
                        let c = b.Body(s, k)
                        vac[c] <- max vac[c] (len - k)

                let mutable contested = 0

                for c in 0 .. cells - 1 do
                    if an.Owner c = -2 then contested <- contested + 1

                let deadBefore = players - alive

                for me in 0 .. alive - 1 do
                    let h = b.Head me
                    let myLen = b.Length me
                    let mutable maxOppArea = 0
                    let mutable maxOppLen = 0
                    let mutable minArea = Int32.MaxValue
                    let mutable outlived = float deadBefore
                    let mutable oppHead = 99

                    for s in 0 .. alive - 1 do
                        if s <> me then
                            maxOppArea <- max maxOppArea (an.Area s)
                            maxOppLen <- max maxOppLen (b.Length s)
                            let oh = b.Head s
                            oppHead <- min oppHead (abs (geo.X oh - geo.X h) + abs (geo.Y oh - geo.Y h))

                            if deaths[s] < deaths[me] then outlived <- outlived + 1.0
                            elif deaths[s] = deaths[me] then outlived <- outlived + 0.5

                    // safe neighbours of the head, and those a head of an equal or longer snake can also reach
                    let mutable deg = 0
                    let mutable threat = 0

                    for i in 0..3 do
                        let nb = geo.Neighbor(h, enum<Dir> i)

                        if nb >= 0 && vac[nb] <= 1 then
                            deg <- deg + 1
                            let mutable t = false

                            for s in 0 .. alive - 1 do
                                if s <> me && b.Length s >= myLen then
                                    let oh = b.Head s
                                    if abs (geo.X oh - geo.X nb) + abs (geo.Y oh - geo.Y nb) = 1 then t <- true

                            if t then threat <- threat + 1

                    // flood fill from my head ignoring the other snakes' heads (bodies still vacate in time)
                    Array.Fill(sd, -1)
                    sd[h] <- 0
                    q[0] <- h
                    let mutable qh = 0
                    let mutable qt = 1

                    while qh < qt do
                        let c = q[qh]
                        qh <- qh + 1

                        for i in 0..3 do
                            let nb = geo.Neighbor(c, enum<Dir> i)

                            if nb >= 0 && sd[nb] < 0 && sd[c] + 1 >= vac[nb] then
                                sd[nb] <- sd[c] + 1
                                q[qt] <- nb
                                qt <- qt + 1

                    let tail = b.Body(me, myLen - 1)
                    let tailDist = if sd[tail] < 0 then 99 else sd[tail]
                    let mutable frontier = 0
                    let mutable near3 = 0

                    for c in 0 .. cells - 1 do
                        if an.Owner c = me && c <> h then
                            if an.Dist c <= 3 then near3 <- near3 + 1
                            let mutable edge = false

                            for i in 0..3 do
                                let nb = geo.Neighbor(c, enum<Dir> i)
                                if nb >= 0 && an.Owner nb <> me && an.Owner nb <> -1 then edge <- true

                            if edge then frontier <- frontier + 1

                    for s in 0 .. alive - 1 do
                        minArea <- min minArea (an.Area s)

                    let die = deaths[me]
                    let nf = an.NearestFood me
                    let x, y = geo.X h, geo.Y h

                    let ev =
                        Eval.combine
                            w
                            { MyArea = an.Area me
                              MaxOppArea = maxOppArea
                              MyLen = myLen
                              MaxOppLen = maxOppLen
                              Others = alive - 1
                              Hp = b.Health me
                              NearestFood = nf }

                    out.Add
                        [| valid
                           float32 players
                           float32 (outlived / float (players - 1))
                           (if die - turn <= 10 then 1f else 0f)
                           (if die - turn <= 5 then 1f else 0f)
                           float32 turn
                           (if mine[me] then 1f else 0f)
                           float32 (an.Area me)
                           float32 maxOppArea
                           float32 myLen
                           float32 maxOppLen
                           float32 (alive - 1)
                           float32 (b.Health me)
                           float32 (min nf 99)
                           float32 ev
                           float32 deg
                           float32 (qt - 1)
                           float32 frontier
                           float32 near3
                           float32 threat
                           float32 oppHead
                           float32 tailDist
                           float32 (min (min x (10 - x)) (min y (10 - y)))
                           float32 contested
                           float32 (if an.Area me = minArea then 1 else 0)
                           float32 (qt - 1 - an.Area me) |]

        out.ToArray()

let dump (argv: string[]) =
    let dir, mode, outFile = argv[1], argv[2], argv[3]
    let players = if mode = "duel" then 2 else 4
    let files = Directory.GetFiles(dir, "*.json")
    let parts = Array.zeroCreate files.Length
    Parallel.For(0, files.Length, fun i -> parts[i] <- dumpGame players files[i]) |> ignore
    let rows = Array.concat parts
    use bw = new BinaryWriter(File.Create outFile)

    for row in rows do
        for v in row do
            bw.Write v

    File.WriteAllText(outFile + ".cols", String.Join(",", columns))
    printfn "%d rows x %d columns -> %s" rows.Length columns.Length outFile
