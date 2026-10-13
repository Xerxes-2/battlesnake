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

[<EntryPoint>]
let main _ =
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

    let policy = Policy(geo, 4)

    measure "decide (1-ply paranoid)" 1.0 (fun () ->
        i <- (i + 1) % positions.Length
        let p = positions.[i]
        let me = (seq { 0 .. p.NumSnakes - 1 } |> Seq.find p.Alive)
        policy.Decide(p, me) |> ignore
        1)

    0
