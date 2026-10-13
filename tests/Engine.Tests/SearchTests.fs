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
                moves.[s] <- enum<Dir> (rng.Next 4)

                for _ in 1..4 do
                    let nb = geo.Neighbor(b.Head s, moves.[s])
                    if nb < 0 || b.Occupancy nb > 0 then moves.[s] <- enum<Dir> (rng.Next 4)

            b.Step moves
            if rng.Next 100 < 25 then b.SetFood(rng.Next geo.Cells, true)
            if b.Turn % 15 = 0 && b.Alive 0 && b.AliveCount >= 2 then res.Add(b.Clone())

    res.ToArray()

let fixedDepth (ordering: bool) (depth: int) (b: Board) =
    let s = Searcher(geo, 4, Weights.Standard)
    s.Ordering <- ordering
    s.ExactDepthOnly <- true
    s.MaxDepth <- depth
    s.SearchFor(b, 0, 600_000.0)

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
        let s = Searcher(geo, 4, Weights.Standard)
        s.MaxDepth <- 5
        let first = s.SearchFor(b, 0, 600_000.0)
        let second = s.SearchFor(b, 0, 600_000.0)
        Assert.Equal(first.Depth, second.Depth)
        Assert.Equal(first.Score, second.Score)
        Assert.True(second.Nodes * 3L <= first.Nodes + 50L, $"second {second.Nodes} vs first {first.Nodes}")
