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
            // Rounds repeat the same positions: start cold so the table cannot carry answers over
            sr.Clear()
            let r = sr.SearchFor(p, me, ms)
            nodes <- nodes + r.Nodes
            depth <- depth + int64 r.Depth
            n <- n + 1

    printfn "%-28s %12.0f nodes/s   avg depth %.2f over %d searches" $"search {ms}ms" (float nodes / sw.Elapsed.TotalSeconds) (float depth / float n) n

/// Mid-game positions with n snakes from semi-random play.
let mixedPositions (n: int) (count: int) =
    let rng = Random(9)
    let res = ResizeArray<Board>()
    let starts = [| (1, 1); (9, 9); (1, 9); (9, 1) |]

    while res.Count < count do
        let b = Board(geo, n)

        for i in 0 .. n - 1 do
            let c = geo.Cell starts.[i]
            b.AddSnake(100, [| c; c; c |]) |> ignore

        b.SetFood(geo.Cell(5, 5), true)
        let moves = Array.zeroCreate<Dir> n

        while b.AliveCount >= 2 && b.Turn < 90 && res.Count < count do
            for s in 0 .. n - 1 do
                moves.[s] <- enum<Dir> (rng.Next 4)

                for _ in 1..4 do
                    let nb = geo.Neighbor(b.Head s, moves.[s])
                    if nb < 0 || b.Occupancy nb > 0 then moves.[s] <- enum<Dir> (rng.Next 4)

            b.Step moves
            if rng.Next 100 < 20 then b.SetFood(rng.Next geo.Cells, true)
            if b.Turn >= 15 && b.Turn % 12 = 0 && b.Alive 0 && b.AliveCount >= 2 then res.Add(b.Clone())

    res.ToArray()

/// Machine-independent: total nodes to complete each depth, plain fixed order vs move ordering.
let orderBench () =
    for n, label, depths in [ 2, "duel", [ 5; 7; 9 ]; 4, "4 snakes", [ 4; 5; 6 ] ] do
        let ps = mixedPositions n 40

        for d in depths do
            let total o sc =
                ps
                |> Array.sumBy (fun b ->
                    let sr = Searcher(geo, n, Weights.Standard)
                    sr.Ordering <- o
                    sr.UseScores <- sc
                    sr.MaxDepth <- d
                    (sr.SearchFor(b, 0, 600_000.0)).Nodes)

            let a, h, t = total false false, total true false, total true true
            printfn "%-9s depth %d: plain %11d  hints %11d (%.2fx)  +scores %11d (%.2fx)" label d a h (float a / float h) t (float a / float t)

/// Lazy SMP at a fixed time per position: nodes/s and average completed depth for 1..threads threads.
let smpBench (positions: Board[]) (ms: float) (threads: int) =
    for t in 1..threads do
        let ps = ParallelSearcher(geo, 4, Weights.Standard, t)
        ps.Diversity <- (match Environment.GetEnvironmentVariable "SMP_DIV" with | null -> 0 | v -> int v)
        let mutable nodes = 0L
        let mutable depth = 0L
        let sw = Stopwatch.StartNew()

        for p in positions do
            ps.Clear()
            let me = (seq { 0 .. p.NumSnakes - 1 } |> Seq.find p.Alive)
            let stopAt = Stopwatch.GetTimestamp() + int64 (ms * float Stopwatch.Frequency / 1000.0)
            let r = ps.Search(p, me, stopAt, t - 1)
            nodes <- nodes + r.Nodes
            depth <- depth + int64 r.Depth

        printfn "smp %d thread(s) %4.0fms   %10.0f nodes/s   avg depth %.2f" t ms (float nodes / sw.Elapsed.TotalSeconds) (float depth / float positions.Length)

[<EntryPoint>]
let main argv =
    if argv |> Array.contains "smp" then
        let positions = samplePositions 32
        smpBench positions 50.0 2 // warm-up
        let ms = argv |> Array.tryPick (fun a -> match Double.TryParse a with | true, v -> Some v | _ -> None) |> Option.defaultValue 250.0
        let tmax = (match Environment.GetEnvironmentVariable "SMP_T" with | null -> 4 | v -> int v)
        if tmax = 1 then
            for m in [ ms; ms * 2.0; ms * 4.0 ] do smpBench positions m 1
        else smpBench positions ms tmax
        exit 0

    if argv |> Array.contains "order" then
        orderBench ()
        exit 0

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
