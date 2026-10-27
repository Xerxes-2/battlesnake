module Snake.Engine.Tests.PolicyTests

open Xunit
open Snake.Engine
open Snake.Engine.Tests.RulesTests

let decide (b: Board) = Policy.create geo 4 b 0

[<Fact>]
let ``voronoi splits an empty board symmetrically`` () =
    let b = board [ 100, [ (1, 5); (0, 5); (0, 5) ]; 100, [ (9, 5); (10, 5); (10, 5) ] ] []
    let a = Territory(geo, 4)
    a.Voronoi b
    Assert.Equal(a.Area 0, a.Area 1)
    Assert.True(a.Area 0 > 40)

[<Fact>]
let ``bitboard voronoi matches the scalar one`` () =
    let rng = System.Random(5)
    let a = Territory(geo, 4)
    let mutable checkedPositions = 0

    for _ in 1..300 do
        let b = Board(geo, 4)

        for x, y in [ (1, 1); (9, 9); (1, 9); (9, 1) ] do
            let c = geo.Cell(x, y)
            b.AddSnake(100, [| c; c; c |]) |> ignore

        b.SetFood(geo.Cell(5, 5), true)
        let moves = Array.zeroCreate<Dir> 4

        while b.AliveCount >= 2 && b.Turn < 150 do
            for s in 0..3 do
                moves[s] <- enum<Dir> (rng.Next 4)
                // Mostly avoid instant suicide so games get long and bodies get twisted
                for _ in 1..3 do
                    let nb = geo.Neighbor(b.Head s, moves[s])
                    if nb < 0 || (b.Occupancy nb > 0 && b.Alive s) then moves[s] <- enum<Dir> (rng.Next 4)

            b.Step moves
            if rng.Next 100 < 20 then b.SetFood(rng.Next geo.Cells, true)

            a.VoronoiScalar b
            let ref = [| for s in 0..3 -> a.Area s, a.NearestFood s |]
            a.VoronoiBits b
            let got = [| for s in 0..3 -> a.Area s, a.NearestFood s |]
            Assert.Equal<(int * int)[]>(ref, got)
            checkedPositions <- checkedPositions + 1

    Assert.True(checkedPositions > 3000)

[<Fact>]
let ``does not walk into a wall`` () =
    let b = board [ 100, [ (0, 10); (1, 10); (2, 10) ] ] []
    Assert.Equal(Dir.Down, decide b)

[<Fact>]
let ``avoids head to head with a longer snake`` () =
    // Moving right might hit the longer snake head-on at (5,5)
    let b = board [ 100, [ (4, 5); (3, 5); (2, 5) ]; 100, [ (6, 5); (7, 5); (8, 5); (9, 5) ] ] []
    Assert.NotEqual(Dir.Right, decide b)

[<Fact>]
let ``avoids a dead-end pocket`` () =
    // Left of the head, (4,5) and (3,5) form a 2-cell pocket enclosed by the body; the body is long enough that it won't vacate in time
    let b =
        board
            [ 100,
              [ (5, 5)
                (5, 4)
                (4, 4)
                (3, 4)
                (2, 4)
                (2, 5)
                (2, 6)
                (3, 6)
                (4, 6)
                (4, 7)
                (4, 8)
                (4, 9)
                (4, 10) ] ]
            []

    Assert.NotEqual(Dir.Left, decide b)

[<Fact>]
let ``goes for adjacent food when starving`` () =
    let b = board [ 2, [ (5, 5); (5, 4); (5, 3) ] ] [ (4, 5) ]
    Assert.Equal(Dir.Left, decide b)

// ---- Searcher (iterative-deepening paranoid alpha-beta) ----

let search (b: Board) = Searcher(geo, 4, SearchConfig.make Weights.defaults).Search(b, 0, Limits.forMs 50.0<ms>)

[<Fact>]
let ``search does not walk into a wall`` () =
    let b = board [ 100, [ (0, 10); (1, 10); (2, 10) ] ] []
    Assert.Equal(Dir.Down, (search b).Move)

[<Fact>]
let ``search avoids head to head with a longer snake`` () =
    let b = board [ 100, [ (4, 5); (3, 5); (2, 5) ]; 100, [ (6, 5); (7, 5); (8, 5); (9, 5) ] ] []
    Assert.NotEqual(Dir.Right, (search b).Move)

[<Fact>]
let ``search avoids a dead-end pocket`` () =
    let b =
        board
            [ 100,
              [ (5, 5)
                (5, 4)
                (4, 4)
                (3, 4)
                (2, 4)
                (2, 5)
                (2, 6)
                (3, 6)
                (4, 6)
                (4, 7)
                (4, 8)
                (4, 9)
                (4, 10) ] ]
            []

    Assert.NotEqual(Dir.Left, (search b).Move)

[<Fact>]
let ``search takes a head to head it wins`` () =
    // Opponent is shorter and cornered at (0,0): its only moves are up to (0,1) or right to (1,0).
    // Moving to (1,1) does not kill it yet, but the duel is decided either way; we must at least not lose.
    let b = board [ 100, [ (2, 0); (3, 0); (4, 0); (5, 0) ]; 100, [ (0, 0); (0, 1); (0, 2) ] ] []
    // Moving left to (1,0) means: opponent moves right into us (dies, shorter) or up into its own neck (dies).
    let r = search b
    Assert.Equal(Dir.Left, r.Move)
    Assert.True(r.Score > 1e5)

[<Fact>]
let ``search completes several plies in a duel`` () =
    let b = board [ 100, [ (1, 1); (1, 1); (1, 1) ]; 100, [ (9, 9); (9, 9); (9, 9) ] ] [ (5, 5) ]
    let r = search b
    Assert.True(r.Depth >= 3, $"depth {r.Depth}, nodes {r.Nodes}")

[<Fact>]
let ``search returns a move with no time left`` () =
    let b = board [ 100, [ (5, 5); (5, 4); (5, 3) ]; 100, [ (1, 1); (1, 2); (1, 3) ] ] []
    let r = Searcher(geo, 4, SearchConfig.make Weights.defaults).Search(b, 0, Limits.until 0L<tick>)
    Assert.Contains(r.Move, [ Dir.Up; Dir.Left; Dir.Right ])
