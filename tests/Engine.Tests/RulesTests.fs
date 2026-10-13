module Snake.Engine.Tests.RulesTests

open Xunit
open Snake.Engine

let geo = Geometry(11, 11)
let c (x, y) = geo.Cell(x, y)

/// Snakes are given as (health, [head; ...; tail]) in xy coordinates
let board (snakes: (int * (int * int) list) list) (food: (int * int) list) =
    let b = Board(geo, 4)

    for hp, body in snakes do
        b.AddSnake(hp, body |> List.map c |> Array.ofList) |> ignore

    for f in food do
        b.SetFood(c f, true)

    b

let body (b: Board) s =
    [ for k in 0 .. b.Length s - 1 -> let cell = b.Body(s, k) in geo.X cell, geo.Y cell ]

let step (b: Board) (moves: Dir list) = b.Step(Array.ofList moves)

let totalOcc (b: Board) = Seq.sum (seq { for i in 0 .. geo.Cells - 1 -> b.Occupancy i })

let aliveLen (b: Board) =
    Seq.sum (seq { for s in 0 .. b.NumSnakes - 1 do if b.Alive s then b.Length s })

[<Fact>]
let ``move shifts body and costs one health`` () =
    let b = board [ 90, [ (5, 5); (5, 4); (5, 3) ] ] []
    step b [ Dir.Up ]
    Assert.Equal<(int * int) list>([ (5, 6); (5, 5); (5, 4) ], body b 0)
    Assert.Equal(89, b.Health 0)
    Assert.Equal(1, b.Turn)
    Assert.Equal(aliveLen b, totalOcc b)

[<Fact>]
let ``stacked start body unstacks`` () =
    let b = board [ 100, [ (1, 1); (1, 1); (1, 1) ] ] []
    step b [ Dir.Right ]
    Assert.Equal<(int * int) list>([ (2, 1); (1, 1); (1, 1) ], body b 0)
    Assert.True(b.Alive 0)

[<Fact>]
let ``eating restores health, grows by duplicating tail, removes food`` () =
    let b = board [ 10, [ (5, 5); (5, 4); (5, 3) ] ] [ (5, 6) ]
    step b [ Dir.Up ]
    Assert.Equal(100, b.Health 0)
    Assert.Equal<(int * int) list>([ (5, 6); (5, 5); (5, 4); (5, 4) ], body b 0)
    Assert.False(b.IsFood(c (5, 6)))
    Assert.Equal(aliveLen b, totalOcc b)

[<Fact>]
let ``moving off the board eliminates`` () =
    let b = board [ 100, [ (0, 5); (1, 5); (2, 5) ] ] []
    step b [ Dir.Left ]
    Assert.False(b.Alive 0)
    Assert.Equal(0, totalOcc b)

[<Fact>]
let ``starvation at zero health, unless eating on that turn`` () =
    let starve = board [ 1, [ (5, 5); (5, 4); (5, 3) ] ] []
    step starve [ Dir.Up ]
    Assert.False(starve.Alive 0)
    let saved = board [ 1, [ (5, 5); (5, 4); (5, 3) ] ] [ (5, 6) ]
    step saved [ Dir.Up ]
    Assert.True(saved.Alive 0)

[<Fact>]
let ``moving into own neck is self collision`` () =
    let b = board [ 100, [ (5, 5); (5, 4); (5, 3) ] ] []
    step b [ Dir.Down ]
    Assert.False(b.Alive 0)

[<Fact>]
let ``chasing own tail is allowed`` () =
    let b = board [ 100, [ (5, 5); (6, 5); (6, 4); (5, 4) ] ] []
    step b [ Dir.Down ]
    Assert.True(b.Alive 0)

[<Fact>]
let ``tail does not move the turn after eating`` () =
    // Ate last turn so the tail is duplicated; following that tail now is a collision
    let b = board [ 100, [ (5, 5); (6, 5); (6, 4); (5, 4); (5, 4) ] ] []
    step b [ Dir.Down ]
    Assert.False(b.Alive 0)

[<Fact>]
let ``head into other snake body is elimination for the mover only`` () =
    let b = board [ 100, [ (5, 5); (4, 5); (3, 5) ]; 100, [ (7, 6); (6, 6); (5, 6); (4, 6) ] ] []
    step b [ Dir.Up; Dir.Right ]
    Assert.False(b.Alive 0)
    Assert.True(b.Alive 1)

[<Fact>]
let ``following another snake's tail is allowed`` () =
    let b = board [ 100, [ (5, 5); (4, 5); (3, 5) ]; 100, [ (6, 7); (6, 6); (5, 6) ] ] []
    step b [ Dir.Up; Dir.Up ]
    Assert.True(b.Alive 0)
    Assert.True(b.Alive 1)

[<Fact>]
let ``head to head: shorter loses`` () =
    let b = board [ 100, [ (4, 5); (3, 5); (2, 5) ]; 100, [ (6, 5); (7, 5); (8, 5); (9, 5) ] ] []
    step b [ Dir.Right; Dir.Left ]
    Assert.False(b.Alive 0)
    Assert.True(b.Alive 1)
    Assert.Equal(aliveLen b, totalOcc b)

[<Fact>]
let ``head to head: equal length both die`` () =
    let b = board [ 100, [ (4, 5); (3, 5); (2, 5) ]; 100, [ (6, 5); (7, 5); (8, 5) ] ] []
    step b [ Dir.Right; Dir.Left ]
    Assert.False(b.Alive 0)
    Assert.False(b.Alive 1)

[<Fact>]
let ``head to head length counts food eaten this turn`` () =
    let b = board [ 100, [ (4, 5); (3, 5); (2, 5) ]; 100, [ (6, 5); (7, 5); (8, 5) ] ] [ (5, 5) ]
    step b [ Dir.Right; Dir.Left ]
    // Both eat and both grow to length 4, so it's still equal length and both die
    Assert.False(b.Alive 0)
    Assert.False(b.Alive 1)

[<Fact>]
let ``body of a snake eliminated out of bounds is not an obstacle`` () =
    // Snake 1 runs off the board; snake 0's head enters the cell occupied by snake 1's body
    let b = board [ 100, [ (1, 4); (2, 4); (3, 4) ]; 100, [ (0, 6); (0, 5); (1, 5); (2, 5) ] ] []
    step b [ Dir.Up; Dir.Left ]
    Assert.False(b.Alive 1)
    Assert.True(b.Alive 0)

[<Fact>]
let ``two snakes can share a food`` () =
    let b = board [ 50, [ (4, 5); (3, 5); (2, 5) ]; 50, [ (6, 6); (7, 6); (8, 6) ] ] [ (5, 5) ]
    // Different cells: each snake eats only the food under its own head; here only snake 0 eats
    step b [ Dir.Right; Dir.Left ]
    Assert.Equal(100, b.Health 0)
    Assert.Equal(49, b.Health 1)

[<Fact>]
let ``hazard damage applies unless standing on food`` () =
    let b = board [ 100, [ (5, 5); (5, 4); (5, 3) ] ] []
    b.HazardDamage <- 14
    b.AddHazard(c (5, 6))
    step b [ Dir.Up ]
    Assert.Equal(85, b.Health 0)

[<Fact>]
let ``copy is independent`` () =
    let b = board [ 100, [ (5, 5); (5, 4); (5, 3) ] ] [ (5, 6) ]
    let k = b.Clone()
    step k [ Dir.Up ]
    Assert.Equal(3, b.Length 0)
    Assert.True(b.IsFood(c (5, 6)))
    Assert.Equal(4, k.Length 0)
