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
    let cell (e: JsonElement) = geo.Cell(e[0].GetInt32(), e[1].GetInt32())

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
    let r = Searcher(geo, 4, SearchConfig.make Weights.duel).Search(b, 0, Limits.nodes 2_000_000L)
    Assert.True(r.Score < 500_000.0, $"claimed a proven win: score {r.Score} at depth {r.Depth}")

/// Ladder game 7a8e3ab9 (died turn 88): the paranoid search saw only losses, the relaxed re-search kept just the
/// "nearest" opponent (alexander at distance 2, picked by tie order) and moved right into (9,5), where Nathan, also
/// at distance 2 and longer, moved too. Every opponent that can reach our next cell must stay adversarial.
/// RelaxNear fixes it but is off by default: it cost far more on the ladder than it saved (see SearchConfig).
[<Fact>]
let ``relaxed search with RelaxNear keeps every opponent two steps away adversarial`` () =
    let geo, b = load "std-relaxed-tie-t87.json"
    let cfg = { SearchConfig.make Weights.standard with RelaxNear = true }
    let r = Searcher(geo, 4, cfg).Search(b, 0, Limits.nodes 400_000L)
    Assert.NotEqual(Dir.Right, r.Move)

/// Ladder games e82c20e4 (turn 17) and baffa219 (turn 33), 0.7.8: every move was a proven loss, and the timeout
/// guard's 2 points on the edge move cancelled its 2 extra plies before dying, so the search tied it with a
/// head-on into a longer snake and took that. 29 ladder deaths like this in 2 days. Proven scores carry no guard.
[<Theory>]
[<InlineData("std-guard-headon-t17.json", Dir.Left)>]
[<InlineData("std-guard-headon-t33.json", Dir.Right)>]
let ``the timeout guard does not trade a later loss for a head-on`` (name: string, headOn: Dir) =
    let geo, b = load name
    let search cfg = Searcher(geo, 4, cfg).Search(b, 0, Limits.nodes 400_000L).Move
    let cfg = SearchConfig.make Weights.standard
    Assert.NotEqual(headOn, search cfg)
    Assert.Equal(headOn, search { cfg with GuardProven = true }) // the arena's guard078 bot still plays 0.7.8

/// Ladder game 681b749b (turn 113), 0.7.8: Left kills the shorter opponent head-on now, but points at the wall, so
/// the guard made "win next turn" score higher and the snake ran up beside it for 9 turns. Same cause as above.
/// Needs the server's Lazy SMP: a single thread proves the win at depth 1 and stops; the deepest helper decides.
[<Fact>]
let ``the timeout guard does not postpone a proven win`` () =
    let geo, b = load "duel-guard-stall-t113.json"
    let search cfg = ParallelSearcher(geo, 4, cfg, 4).Search(b, 0, Limits.forMs 50.0<ms>, 3).Move
    let cfg = SearchConfig.make Weights.duel
    Assert.Equal(Dir.Left, search cfg)
    Assert.Equal(Dir.Up, search { cfg with GuardProven = true })

/// Ladder game ebd19d06 (turn 312, lost at 528): Lazy SMP claimed a win in 2 plies at depth 4 and stopped deepening.
/// At depth 4 coreyja, 9 steps away, moved by Default at the root; a helper had proven, deeper and with coreyja in
/// range, that the position it walked into was lost for it. That proof says nothing about coreyja's other moves.
/// Timing-dependent: without the fix about 5 in 8 runs claimed the win, so 4 runs catch it 98% of the time.
[<Fact>]
let ``a win proven below a Default move is not a proof`` () =
    for _ in 1..4 do
        let ps = ParallelSearcher(Geometry(11, 11), 4, SearchConfig.make Weights.standardPrev, 4)

        for t in 309..312 do
            let _, b = load $"std-fake-win-t{t}.json"
            let r = ps.Search(b, 0, Limits.forMs 100.0<ms>, 3)
            Assert.True(r.Score < 500_000.0, $"turn {t}: claimed a proven win, score {r.Score} at depth {r.Depth}")
