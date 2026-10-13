/// Local self-play on the official standard map rules (BattlesnakeOfficial/rules: PlaceSnakesFixed,
/// PlaceFoodFixed, standard food spawning). Game rules come from our own engine, which the tests pin to the official one.
///
///   Arena match <botA> <botB> [--mode duel|standard] [--games N] [--par P] [--seed S]
///   Arena tune  [--mode duel|standard] [--nodes N] [--iters N] [--batch B] [--from weights] [--par P]
///
///   bot:  base | search:<ms> | nodes:<n>   optionally followed by @<weights>
///   weights: "name=value,..." or a path to a file in that format (see Weights.Names)
module Snake.Arena.Program

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Snake.Engine

type Bot = Board -> int -> Dir

let parseWeights (s: string) =
    if File.Exists s then Weights.Parse(File.ReadAllText s) else Weights.Parse s

let makeBotW (geo: Geometry) (maxSnakes: int) (spec: string) (w: Weights) : Bot =
    match spec.Split(':') with
    | [| "base" |] ->
        let p = Policy(geo, maxSnakes)
        fun b s -> p.Decide(b, s)
    | [| "search"; ms |] ->
        let ms = float ms
        let sr = Searcher(geo, maxSnakes, w)
        fun b s -> sr.SearchFor(b, s, ms).Move
    | [| "nodes"; n |] ->
        let sr = Searcher(geo, maxSnakes, w)
        sr.NodeLimit <- int64 n
        // Generous deadline: the node limit is what stops the search
        fun b s -> sr.SearchFor(b, s, 10_000.0).Move
    | _ -> failwith $"unknown bot {spec}"

let makeBot geo maxSnakes (spec: string) =
    match spec.IndexOf '@' with
    | -1 -> makeBotW geo maxSnakes spec Weights.Default
    | i -> makeBotW geo maxSnakes spec.[.. i - 1] (parseWeights spec.[i + 1 ..])

let shuffle (rng: Random) (a: 'T[]) =
    for i in a.Length - 1 .. -1 .. 1 do
        let j = rng.Next(i + 1)
        let t = a.[i]
        a.[i] <- a.[j]
        a.[j] <- t

/// Official PlaceSnakesFixed + PlaceFoodFixed for an 11x11 board.
let setup (rng: Random) (n: int) =
    let geo = Geometry(11, 11)
    let b = Board(geo, n)
    let mn, md, mx = 1, 5, 9
    let corners = [| (mn, mn); (mn, mx); (mx, mn); (mx, mx) |]
    let cardinals = [| (mn, md); (md, mn); (md, mx); (mx, md) |]
    shuffle rng corners
    shuffle rng cardinals
    let starts = if rng.Next 2 = 0 then Array.append corners cardinals else Array.append cardinals corners

    for s in 0 .. n - 1 do
        let c = geo.Cell starts.[s]
        b.AddSnake(100, [| c; c; c |]) |> ignore

    // One food diagonal to each head, away from the centre on at least one axis, never centre or corner
    let cx, cy = 5, 5

    for s in 0 .. n - 1 do
        let hx, hy = starts.[s]

        let options =
            [| (hx - 1, hy - 1); (hx - 1, hy + 1); (hx + 1, hy - 1); (hx + 1, hy + 1) |]
            |> Array.filter (fun (px, py) ->
                (px, py) <> (cx, cy)
                && not (b.IsFood(geo.Cell(px, py)))
                && ((px < hx && hx < cx) || (cx < hx && hx < px) || (py < hy && hy < cy) || (cy < hy && hy < py))
                && not ((px = 0 || px = 10) && (py = 0 || py = 10)))

        if options.Length > 0 then
            b.SetFood(geo.Cell options.[rng.Next options.Length], true)

    b.SetFood(geo.Cell(cx, cy), true)
    b

/// Standard map food: top up to minimumFood=1, else spawn one with foodSpawnChance=15. Like the official
/// GetUnoccupiedPoints(includePossibleMoves=false), cells next to a living head are excluded.
let spawnFood (b: Board) (rng: Random) =
    let geo = b.Geometry
    let mutable nFood = 0

    for c in 0 .. geo.Cells - 1 do
        if b.IsFood c then nFood <- nFood + 1

    if nFood < 1 || (100 - rng.Next 100) < 15 then
        let blocked = Array.zeroCreate<bool> geo.Cells

        for s in 0 .. b.NumSnakes - 1 do
            if b.Alive s then
                for d in Dir.all do
                    let nb = geo.Neighbor(b.Head s, d)
                    if nb >= 0 then blocked.[nb] <- true

        let free =
            [| for c in 0 .. geo.Cells - 1 do
                   if b.Occupancy c = 0 && not (b.IsFood c) && not blocked.[c] then c |]

        if free.Length > 0 then b.SetFood(free.[rng.Next free.Length], true)

/// Plays one game; returns the winning seat (-1 = draw) and the number of turns.
let playBots (bots: Bot[]) (seed: int) =
    let rng = Random(seed)
    let n = bots.Length
    let b = setup rng n
    let moves = Array.zeroCreate<Dir> n

    while b.AliveCount > 1 && b.Turn < 1000 do
        for s in 0 .. n - 1 do
            if b.Alive s then moves.[s] <- bots.[s] b s

        b.Step moves
        spawnFood b rng

    let mutable winner = -1

    if b.AliveCount = 1 then
        for s in 0 .. n - 1 do
            if b.Alive s then winner <- s

    winner, b.Turn

let opt (argv: string[]) name dflt =
    match Array.tryFindIndex ((=) name) argv with
    | Some i when i + 1 < argv.Length -> argv.[i + 1]
    | _ -> dflt

/// Seat layout: duel = A B, standard = A B A B; `flip` swaps A and B so both sides get both start orders.
let seats mode flip (a: 'T) (b: 'T) =
    let ab = if flip then [| b; a |] else [| a; b |]
    if mode = "duel" then ab else Array.append ab ab

let geo11 = Geometry(11, 11)

let runMatch (argv: string[]) =
    let a, bSpec = argv.[1], argv.[2]
    let mode = opt argv "--mode" "duel"
    let games = int (opt argv "--games" "100")
    let par = int (opt argv "--par" (string (max 1 (Environment.ProcessorCount / 2))))
    let seed0 = int (opt argv "--seed" "1")
    let mutable winsA = 0
    let mutable winsB = 0
    let mutable draws = 0
    let mutable turns = 0L
    let sw = Diagnostics.Stopwatch.StartNew()

    // Games come in pairs with the same seed and swapped seats, which cancels most start-position luck
    Parallel.For(0, games, ParallelOptions(MaxDegreeOfParallelism = par), fun g ->
        let specs = seats mode (g % 2 = 1) a bSpec
        let isA = seats mode (g % 2 = 1) true false
        let bots = specs |> Array.map (makeBot geo11 specs.Length)
        let w, t = playBots bots (seed0 * 100_003 + g / 2)
        Interlocked.Add(&turns, int64 t) |> ignore

        if w < 0 || a = bSpec then Interlocked.Increment(&draws) |> ignore
        elif isA.[w] then Interlocked.Increment(&winsA) |> ignore
        else Interlocked.Increment(&winsB) |> ignore)
    |> ignore

    let played = float games
    let score = (float winsA + 0.5 * float draws) / played
    let ci = 1.96 * sqrt (score * (1.0 - score) / played)
    printfn "%s vs %s (%s, %d games, %.0fs): A %d / B %d / draw %d  -> A score %.1f%% ± %.1f%%, avg %.0f turns"
        a bSpec mode games sw.Elapsed.TotalSeconds winsA winsB draws (100.0 * score) (100.0 * ci) (float turns / played)
    0

/// SPSA (as in chess engine tuning): each sample perturbs every weight by ±c_i at random, plays θ+ against θ-
/// on a seat-swapped game pair, and moves θ along the perturbation in proportion to the result.
let runTune (argv: string[]) =
    let mode = opt argv "--mode" "duel"
    let nodes = opt argv "--nodes" "3000"
    let iters = int (opt argv "--iters" "200")
    let batch = int (opt argv "--batch" "64")
    let par = int (opt argv "--par" (string Environment.ProcessorCount))
    let out = opt argv "--out" $"weights-{mode}.txt"
    let theta = (match opt argv "--from" "" with "" -> Weights.Default | s -> parseWeights s).Values |> Array.copy
    let k = theta.Length
    // Perturbation ~15% of the starting value (at least 0.5); integer-like weights still move since we round only for lenCap
    let c = theta |> Array.map (fun x -> max 0.5 (0.15 * abs x))
    // Some weights must stay in sensible ranges
    let lo = Array.create k 0.0
    let hi = theta |> Array.map (fun x -> max 10.0 (4.0 * x))
    let lr = float (opt argv "--lr" "0.4")
    let rngMaster = Random(int (opt argv "--seed" "7"))
    let sw = Diagnostics.Stopwatch.StartNew()
    printfn "tuning %s at nodes:%s, %d iters x %d pairs; start %O" mode nodes iters batch (Weights theta)

    for it in 1 .. iters do
        let deltas = Array.init batch (fun _ -> Array.init k (fun _ -> if rngMaster.Next 2 = 0 then -1.0 else 1.0))
        let seeds = Array.init batch (fun _ -> rngMaster.Next())
        let results = Array.zeroCreate<float> batch
        let snapshot = Array.copy theta

        Parallel.For(0, batch * 2, ParallelOptions(MaxDegreeOfParallelism = par), fun j ->
            let i = j / 2
            let flip = j % 2 = 1
            let plus = Weights(Array.init k (fun p -> Math.Clamp(snapshot.[p] + c.[p] * deltas.[i].[p], lo.[p], hi.[p])))
            let minus = Weights(Array.init k (fun p -> Math.Clamp(snapshot.[p] - c.[p] * deltas.[i].[p], lo.[p], hi.[p])))
            let ws = seats mode flip plus minus
            let isPlus = seats mode flip true false
            let bots = ws |> Array.map (fun w -> makeBotW geo11 ws.Length ("nodes:" + nodes) w)
            let w, _ = playBots bots seeds.[i]
            let r = if w < 0 then 0.0 elif isPlus.[w] then 1.0 else -1.0
            // Two writers per slot (the pair); lock-free add on doubles is not available, so use a lock
            lock results (fun () -> results.[i] <- results.[i] + r))
        |> ignore

        // Decay learning rate slowly, as usual for SPSA
        let a = lr / Math.Pow(1.0 + float it / 50.0, 0.602)

        for p in 0 .. k - 1 do
            let mutable g = 0.0

            for i in 0 .. batch - 1 do
                g <- g + results.[i] * deltas.[i].[p]

            theta.[p] <- Math.Clamp(theta.[p] + a * c.[p] * g / float batch * 2.0, lo.[p], hi.[p])

        let decisive = results |> Array.sumBy abs
        File.WriteAllText(out, (Weights theta).ToString().Replace(",", "\n") + "\n")

        if it % 5 = 0 || it = iters then
            printfn "[%4d %5.0fs] decisive %.0f%%  %O" it sw.Elapsed.TotalSeconds (100.0 * decisive / float (2 * batch)) (Weights theta)

    0

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> eprintfn "usage: Arena match <botA> <botB> ... | Arena tune ..."; 2
    | _ when argv.[0] = "match" -> runMatch argv
    | _ when argv.[0] = "tune" -> runTune argv
    | _ -> runMatch (Array.append [| "match" |] argv)
