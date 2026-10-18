module ReplayTests

open System.IO
open System.Text.Json
open Xunit
open Snake.Engine

/// Loads a position saved from the engine API (fixtures/*.json); "me" goes to index 0.
let load (name: string) =
    let doc = JsonDocument.Parse(File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "fixtures", name)))
    let r = doc.RootElement
    let geo = Geometry(11, 11)
    let b = Board(geo, 4)
    let cell (e: JsonElement) = geo.Cell(e.[0].GetInt32(), e.[1].GetInt32())

    let snakes =
        [ for s in r.GetProperty("snakes").EnumerateArray() ->
              s.GetProperty("me").GetBoolean(),
              s.GetProperty("hp").GetInt32(),
              [| for c in s.GetProperty("body").EnumerateArray() -> cell c |] ]
        |> List.sortBy (fun (me, _, _) -> not me)

    for _, hp, body in snakes do
        b.AddSnake(hp, body) |> ignore

    for f in r.GetProperty("food").EnumerateArray() do
        b.SetFood(cell f, true)

    for h in r.GetProperty("hazards").EnumerateArray() do
        b.AddHazard(cell h)

    b.Turn <- r.GetProperty("turn").GetInt32()
    geo, b

/// Ladder game 60b3aa33 (lost at turn 501): from here 0.6.0 claimed a forced win in 19 plies and stopped
/// deepening, because the opponent, far from our head, was played by the one-step Default policy and
/// walked into a dead end. A proven score must only come from positions where every opponent was searched.
[<Fact>]
let ``no fake proven win when the duel opponent is far away`` () =
    let geo, b = load "duel-fake-win-t479.json"
    let sr = Searcher(geo, 4, Weights.Duel)
    sr.NodeLimit <- 2_000_000L
    let r = sr.SearchFor(b, 0, 60_000.0)
    Assert.True(r.Score < 500_000.0, $"claimed a proven win: score {r.Score} at depth {r.Depth}")
