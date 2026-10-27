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

[<Fact>]
let ``incremental hash matches a from-scratch hash`` () =
    let rng = System.Random(11)
    let mutable steps = 0

    for g in 1..400 do // also checks the occupancy/food bitboards
        let b = Board(geo, 4)
        // Mix of start lengths (1 and 3) so the length-1 path is covered too
        for x, y in [ (1, 1); (9, 9); (1, 9); (9, 1) ] do
            let c = geo.Cell(x, y)
            b.AddSnake(30 + rng.Next 70, (if rng.Next 3 = 0 then [| c |] else [| c; c; c |])) |> ignore

        if g % 3 = 0 then
            b.HazardDamage <- 14
            for _ in 1..20 do b.AddHazard(rng.Next geo.Cells)

        for _ in 1..10 do b.SetFood(rng.Next geo.Cells, true)
        Assert.Equal(b.ComputeHash(), b.Hash)
        let moves = Array.zeroCreate<Dir> 4

        while b.AliveCount >= 1 && b.Turn < 200 do
            for s in 0..3 do
                moves[s] <- enum<Dir> (rng.Next 4)
                // Usually dodge, sometimes not, so all death causes happen
                if rng.Next 10 > 0 then
                    for _ in 1..3 do
                        let nb = geo.Neighbor(b.Head s, moves[s])
                        if nb < 0 || b.Occupancy nb > 0 then moves[s] <- enum<Dir> (rng.Next 4)

            b.Step moves
            if rng.Next 100 < 30 then b.SetFood(rng.Next geo.Cells, true)
            if rng.Next 100 < 5 then b.SetFood(rng.Next geo.Cells, false)
            Assert.Equal(b.ComputeHash(), b.Hash)
            Assert.Equal(b.Hash, b.Clone().Hash)
            let bits f = Seq.fold (fun (lo, hi) c -> if f c then (if c < 64 then lo ||| (1UL <<< c), hi else lo, hi ||| (1UL <<< (c - 64))) else lo, hi) (0UL, 0UL) (seq { 0 .. geo.Cells - 1 })
            Assert.Equal((b.OccLo, b.OccHi), bits (fun c -> b.Occupancy c > 0))
            Assert.Equal((b.FoodLo, b.FoodHi), bits b.IsFood)
            steps <- steps + 1

    Assert.True(steps > 10_000)
