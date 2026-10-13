/// Engine throughput benchmark. Compare runtimes with:
///   dotnet run -c Release -f net10.0 --project bench/Bench
///   dotnet run -c Release -f net11.0 --project bench/Bench
/// Uses its own xorshift with a fixed seed, so the workload is identical on both runtimes.
module Snake.Bench.Program

open System
open System.Diagnostics
open System.Runtime.InteropServices
open Snake.Engine

let geo = Geometry(11, 11)

/// Standard 4-player opening: corner-ish start positions with the body stacked 3 deep, plus centre food and one food near each snake
let start () =
    let b = Board(geo, 4)

    for x, y in [ (1, 1); (9, 9); (1, 9); (9, 1) ] do
        let c = geo.Cell(x, y)
        b.AddSnake(100, [| c; c; c |]) |> ignore

    for x, y in [ (5, 5); (2, 0); (8, 10); (0, 8); (10, 2) ] do
        b.SetFood(geo.Cell(x, y), true)

    b

let mutable rng = 0x9E3779B97F4A7C15UL

let inline next () =
    rng <- rng ^^^ (rng <<< 13)
    rng <- rng ^^^ (rng >>> 7)
    rng <- rng ^^^ (rng <<< 17)
    rng

/// Random but not blatantly suicidal moves (stay on the board, don't turn back onto the neck), plus random food spawns
let randomMoves (b: Board) (moves: Dir[]) =
    for s in 0 .. b.NumSnakes - 1 do
        if b.Alive s then
            let h = b.Head s
            let neck = b.Body(s, 1)
            let mutable d = enum<Dir> (int (next () % 4UL))
            let mutable tries = 0

            while tries < 4 && (let nb = geo.Neighbor(h, d) in nb < 0 || nb = neck) do
                d <- enum<Dir> ((int d + 1) % 4)
                tries <- tries + 1

            moves.[s] <- d

    if next () % 100UL < 15UL then
        let c = int (next () % uint64 geo.Cells)
        if b.Occupancy c = 0 then b.SetFood(c, true)

/// Collect a few mid-game positions (turn >= 20 with at least 2 snakes alive) for the policy benchmark
let samplePositions count =
    let init = start ()
    let b = Board(geo, 4)
    let moves = Array.zeroCreate<Dir> 4
    let res = ResizeArray()

    while res.Count < count do
        b.CopyFrom init

        while b.AliveCount >= 2 && b.Turn < 60 do
            randomMoves b moves
            b.Step moves

            if b.Turn >= 20 && b.Turn % 10 = 0 && b.AliveCount >= 2 && res.Count < count then
                res.Add(b.Clone())

    res.ToArray()

let measure name (seconds: float) (work: unit -> int) =
    // Warm up so tiered compilation finishes promoting the hot paths
    let w = Stopwatch.StartNew()
    while w.Elapsed.TotalSeconds < 1.5 do work () |> ignore

    let rounds = 5
    let rates =
        [| for _ in 1..rounds do
               let sw = Stopwatch.StartNew()
               let mutable ops = 0L
               while sw.Elapsed.TotalSeconds < seconds do
                   ops <- ops + int64 (work ())
               float ops / sw.Elapsed.TotalSeconds |]

    Array.sortInPlace rates
    printfn "%-28s median %12.0f /s   (min %.0f, max %.0f)" name rates.[rounds / 2] rates.[0] rates.[rounds - 1]

/// Fixed-time search on mid-game positions: nodes/s and the average completed depth (what actually matters)
let searchBench (positions: Board[]) (ms: float) (rounds: int) =
    let sr = Searcher(geo, 4, Weights.Standard)
    let mutable nodes = 0L
    let mutable depth = 0L
    let mutable n = 0
    let sw = Stopwatch.StartNew()

    for _ in 1..rounds do
        for p in positions do
            let me = (seq { 0 .. p.NumSnakes - 1 } |> Seq.find p.Alive)
            let r = sr.SearchFor(p, me, ms)
            nodes <- nodes + r.Nodes
            depth <- depth + int64 r.Depth
            n <- n + 1

    printfn "%-28s %12.0f nodes/s   avg depth %.2f over %d searches" $"search {ms}ms" (float nodes / sw.Elapsed.TotalSeconds) (float depth / float n) n

[<EntryPoint>]
let main argv =
    if argv |> Array.contains "search" then
        let positions = samplePositions 32
        searchBench positions 50.0 1 // warm-up
        searchBench positions 100.0 (if argv |> Array.contains "long" then 10 else 2)
        exit 0

    printfn "%s | %s | %s | cores=%d" RuntimeInformation.FrameworkDescription (string RuntimeInformation.OSArchitecture) RuntimeInformation.RuntimeIdentifier Environment.ProcessorCount

    let init = start ()
    let b = Board(geo, 4)
    let moves = Array.zeroCreate<Dir> 4

    measure "step (random playout)" 1.0 (fun () ->
        b.CopyFrom init
        let mutable n = 0

        while b.AliveCount >= 2 && b.Turn < 200 do
            randomMoves b moves
            b.Step moves
            n <- n + 1

        n)

    let positions = samplePositions 32
    let an = Analyzer(geo, 4)
    let mutable i = 0

    measure "evaluate (voronoi)" 1.0 (fun () ->
        i <- (i + 1) % positions.Length
        an.Evaluate(positions.[i], 0) |> ignore
        1)

    measure "voronoi scalar" 1.0 (fun () ->
        i <- (i + 1) % positions.Length
        an.VoronoiScalar positions.[i]
        1)

    measure "voronoi bits" 1.0 (fun () ->
        i <- (i + 1) % positions.Length
        an.VoronoiBits positions.[i]
        1)

    let policy = Policy(geo, 4)

    measure "decide (1-ply paranoid)" 1.0 (fun () ->
        i <- (i + 1) % positions.Length
        let p = positions.[i]
        let me = (seq { 0 .. p.NumSnakes - 1 } |> Seq.find p.Alive)
        policy.Decide(p, me) |> ignore
        1)

    0
