module Snake.Engine.Tests.PolicyTests

open Xunit
open Snake.Engine
open Snake.Engine.Tests.RulesTests

let decide (b: Board) = Policy(geo, 4).Decide(b, 0)

[<Fact>]
let ``voronoi splits an empty board symmetrically`` () =
    let b = board [ 100, [ (1, 5); (0, 5); (0, 5) ]; 100, [ (9, 5); (10, 5); (10, 5) ] ] []
    let a = Analyzer(geo, 4)
    a.Voronoi b
    Assert.Equal(a.Area 0, a.Area 1)
    Assert.True(a.Area 0 > 40)

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
              [ (5, 5); (5, 4); (4, 4); (3, 4); (2, 4); (2, 5); (2, 6); (3, 6); (4, 6); (4, 7); (4, 8); (4, 9); (4, 10) ] ]
            []

    Assert.NotEqual(Dir.Left, decide b)

[<Fact>]
let ``goes for adjacent food when starving`` () =
    let b = board [ 2, [ (5, 5); (5, 4); (5, 3) ] ] [ (4, 5) ]
    Assert.Equal(Dir.Left, decide b)
