module Snake.Engine.Tests.SearchTests

open Xunit
open Snake.Engine
open Snake.Engine.Tests.RulesTests

/// Mid-game positions from semi-random play (avoids instant suicide so bodies get long and twisted).
let positions (seed: int) (count: int) =
    let rng = System.Random(seed)
    let res = ResizeArray<Board>()

    while res.Count < count do
        let b = Board(geo, 4)

        for x, y in [ (1, 1); (9, 9); (1, 9); (9, 1) ] do
            let c = geo.Cell(x, y)
            b.AddSnake(100, [| c; c; c |]) |> ignore

        b.SetFood(geo.Cell(5, 5), true)
        let moves = Array.zeroCreate<Dir> 4

        while b.AliveCount >= 2 && b.Turn < 80 && res.Count < count do
            for s in 0..3 do
                moves[s] <- enum<Dir> (rng.Next 4)

                for _ in 1..4 do
                    let nb = geo.Neighbor(b.Head s, moves[s])
                    if nb < 0 || b.Occupancy nb > 0 then moves[s] <- enum<Dir> (rng.Next 4)

            b.Step moves
            if rng.Next 100 < 25 then b.SetFood(rng.Next geo.Cells, true)
            if b.Turn % 15 = 0 && b.Alive 0 && b.AliveCount >= 2 then res.Add(b.Clone())

    res.ToArray()

let fixedDepth (ordering: bool) (depth: int) (b: Board) =
    let cfg = { SearchConfig.make Weights.standard with Ordering = ordering; ExactDepthOnly = true }
    Searcher(geo, 4, cfg).Search(b, 0, Limits.depth depth)

[<Fact>]
let ``move ordering does not change the fixed-depth result`` () =
    for b in positions 3 40 do
        for depth in [ 1; 2; 3; 4 ] do
            let plain = fixedDepth false depth b
            let ordered = fixedDepth true depth b
            Assert.Equal(plain.Depth, ordered.Depth)
            Assert.Equal(plain.Score, ordered.Score)

[<Fact>]
let ``move ordering cuts the tree`` () =
    let ps = positions 4 30
    let total o = ps |> Array.sumBy (fun b -> (fixedDepth o 5 b).Nodes)
    let plain, ordered = total false, total true
    Assert.True(ordered < plain, $"ordered {ordered} vs plain {plain}")

[<Fact>]
let ``reusing a persistent table keeps the result at least as deep`` () =
    // Search once, then again on the same searcher: the second run must reach at least the same depth on far
    // fewer nodes, and agree on the score at the depth both completed (deeper entries may only refine it).
    for b in positions 5 15 do
        let s = Searcher(geo, 4, SearchConfig.make Weights.standard)
        let first = s.Search(b, 0, Limits.depth 5)
        let second = s.Search(b, 0, Limits.depth 5)
        Assert.Equal(first.Depth, second.Depth)
        Assert.Equal(first.Score, second.Score)
        Assert.True(second.Nodes * 3L <= first.Nodes + 50L, $"second {second.Nodes} vs first {first.Nodes}")

/// Lazy SMP: legal moves, stops at the deadline, and a forced move (main thread done at once, before the helpers
/// even started) does not leave helpers running until the deadline.
[<Fact>]
let ``parallel searcher stops on time and plays legal moves`` () =
    let ps = ParallelSearcher(geo, 4, SearchConfig.make Weights.standard, 4)

    for b in positions 21 20 do
        let t0 = Time.now ()
        let r = ps.Search(b, 0, Limits.forMs 30.0<ms>, 3)
        Assert.True(Time.since t0 < 80.0<ms>, $"took {Time.since t0} ms")
        Assert.True(geo.Neighbor(b.Head 0, r.Move) >= 0)

    // Snake in the corner with its neck blocking one way: a single legal move
    let b = Board(geo, 4)
    b.AddSnake(100, [| geo.Cell(0, 0); geo.Cell(1, 0); geo.Cell(2, 0) |]) |> ignore
    b.AddSnake(100, [| geo.Cell(9, 9); geo.Cell(9, 8); geo.Cell(9, 7) |]) |> ignore
    let t0 = Time.now ()
    let r = ps.Search(b, 0, Limits.forMs 2000.0<ms>, 3)
    Assert.Equal(Dir.Up, r.Move)
    Assert.True(Time.since t0 < 500.0<ms>, $"forced move took {Time.since t0} ms")
