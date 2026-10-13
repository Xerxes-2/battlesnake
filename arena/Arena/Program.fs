/// Local self-play: plays bot A against bot B many times in parallel with the engine's own rules plus
/// standard food spawning, and reports A's win rate. Not bit-exact with the official engine (start
/// positions and food placement are approximations), but good enough to rank versions against each other.
///
///   dotnet run -c Release --project arena/Arena -- <botA> <botB> [--mode duel|standard] [--games N] [--par P] [--seed S]
///   bots: base (1-ply paranoid)  |  search:<ms> (iterative deepening with a fixed per-move budget)
module Snake.Arena.Program

open System
open System.Threading
open System.Threading.Tasks
open Snake.Engine

type Bot = Board -> int -> Dir

let makeBot (geo: Geometry) (maxSnakes: int) (spec: string) : Bot =
    match spec.Split(':') with
    | [| "base" |] ->
        let p = Policy(geo, maxSnakes)
        fun b s -> p.Decide(b, s)
    | [| "search"; ms |] ->
        let ms = float ms
        let sr = Searcher(geo, maxSnakes)
        fun b s -> sr.SearchFor(b, s, ms).Move
    | _ -> failwith $"unknown bot {spec}"

let starts = [| (1, 1); (9, 9); (1, 9); (9, 1); (1, 5); (9, 5); (5, 1); (5, 9) |]

let spawnFood (b: Board) (rng: Random) =
    let geo = b.Geometry
    let mutable nFood = 0

    for c in 0 .. geo.Cells - 1 do
        if b.IsFood c then nFood <- nFood + 1

    // Standard ruleset: keep at least minimumFood=1, otherwise 15% chance per turn to add one
    if nFood < 1 || rng.Next 100 < 15 then
        let free = [| for c in 0 .. geo.Cells - 1 do if b.Occupancy c = 0 && not (b.IsFood c) then c |]
        if free.Length > 0 then b.SetFood(free.[rng.Next free.Length], true)

/// Returns the winning seat, or -1 for a draw (everyone died on the same turn, or the turn cap).
let play (specs: string[]) (seed: int) =
    let rng = Random(seed)
    let geo = Geometry(11, 11)
    let n = specs.Length
    let b = Board(geo, n)
    // Duels use opposite corners like the official layout; standard games shuffle the corners
    let pos = if n = 2 then (if rng.Next 2 = 0 then [| starts.[0]; starts.[1] |] else [| starts.[2]; starts.[3] |])
              else starts.[0..3] |> Array.sortBy (fun _ -> rng.Next())

    for s in 0 .. n - 1 do
        let x, y = pos.[s]
        let c = geo.Cell(x, y)
        b.AddSnake(100, [| c; c; c |]) |> ignore
        // One food diagonally next to each snake, on the side facing the centre
        let fx = x + (if x < 5 then 1 else -1)
        let fy = y + (if y < 5 then 1 else -1)
        b.SetFood(geo.Cell(fx, fy), true)

    b.SetFood(geo.Cell(5, 5), true)
    let bots = specs |> Array.map (makeBot geo n)
    let moves = Array.zeroCreate<Dir> n
    let mutable last = -1

    while b.AliveCount > 1 && b.Turn < 1000 do
        for s in 0 .. n - 1 do
            if b.Alive s then moves.[s] <- bots.[s] b s

        b.Step moves
        spawnFood b rng

    if b.AliveCount = 1 then
        for s in 0 .. n - 1 do
            if b.Alive s then last <- s

    last, b.Turn

[<EntryPoint>]
let main argv =
    let opt name dflt =
        match Array.tryFindIndex ((=) name) argv with
        | Some i when i + 1 < argv.Length -> argv.[i + 1]
        | _ -> dflt

    let a, bSpec = argv.[0], argv.[1]
    let mode = opt "--mode" "duel"
    let games = int (opt "--games" "100")
    let par = int (opt "--par" (string (max 1 (Environment.ProcessorCount / 2))))
    let seed0 = int (opt "--seed" "1")

    // Seats alternate so neither side always gets the same corner; standard = A B A B
    let lineup g =
        let ab = if g % 2 = 0 then [| a; bSpec |] else [| bSpec; a |]
        if mode = "duel" then ab else Array.append ab ab

    let mutable winsA = 0
    let mutable winsB = 0
    let mutable draws = 0
    let mutable turns = 0L
    let sw = Diagnostics.Stopwatch.StartNew()

    Parallel.For(0, games, ParallelOptions(MaxDegreeOfParallelism = par), fun g ->
        let specs = lineup g
        let w, t = play specs (seed0 * 100_003 + g)
        Interlocked.Add(&turns, int64 t) |> ignore
        if w < 0 then Interlocked.Increment(&draws) |> ignore
        elif specs.[w] = a && a <> bSpec then Interlocked.Increment(&winsA) |> ignore
        elif specs.[w] = bSpec && a <> bSpec then Interlocked.Increment(&winsB) |> ignore
        else Interlocked.Increment(&draws) |> ignore)
    |> ignore

    let played = float games
    // Score counts a draw as half a win; the 95% interval is the normal approximation
    let score = (float winsA + 0.5 * float draws) / played
    let ci = 1.96 * sqrt (score * (1.0 - score) / played)
    printfn "%s vs %s (%s, %d games, %.0fs): A %d / B %d / draw %d  -> A score %.1f%% ± %.1f%%, avg %.0f turns"
        a bSpec mode games sw.Elapsed.TotalSeconds winsA winsB draws (100.0 * score) (100.0 * ci) (float turns / played)
    0
