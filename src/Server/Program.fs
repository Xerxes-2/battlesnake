module Snake.Server.Program

open System
open System.Diagnostics
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Snake.Engine
open Snake.Server.Api

/// Searches until `stopAt` (a Stopwatch timestamp). A Searcher is a few KB of buffers, so one per request is fine.
let decide (gs: GameState) (stopAt: int64) =
    let b, me = toBoard gs
    Searcher(b.Geometry, b.MaxSnakes).Search(b, me, stopAt)

let msToTicks (ms: float) = int64 (ms * float Stopwatch.Frequency / 1000.0)

/// Startup warmup: run the decision path a few hundred times on a fake board so the JIT promotes the hot paths to optimized code (tier 1).
let warmup () =
    let mk id (x, y) =
        { Id = id; Name = id; Health = 90; Body = [| for i in 0..2 -> { X = x; Y = y - i } |]
          Head = { X = x; Y = y }; Length = 3; Latency = "0"; Shout = ""; Squad = "" }

    let snakes = [| mk "a" (1, 3); mk "b" (9, 3); mk "c" (1, 9); mk "d" (9, 9) |]

    let gs =
        { Game = { Id = "warmup"; Ruleset = { Name = "standard"; Version = ""; Settings = { FoodSpawnChance = 15; MinimumFood = 1; HazardDamagePerTurn = 0 } }
                   Map = "standard"; Timeout = 500; Source = "" }
          Turn = 5
          Board = { Height = 11; Width = 11; Food = [| { X = 5; Y = 5 } |]; Hazards = [||]; Snakes = snakes }
          You = snakes.[0] }

    // Many short searches: tiering promotes a method after ~30 calls, then OSR/PGO need loops to run
    let sw = Stopwatch.StartNew()
    for _ in 1..60 do decide gs (Stopwatch.GetTimestamp() + msToTicks 5.0) |> ignore
    sw.Elapsed.TotalMilliseconds

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateSlimBuilder(args)
    // In code rather than appsettings.json: the Nix package runs without a content root next to the binary,
    // and per-request framework logs would double the journal for no information.
    builder.Logging.AddFilter("Microsoft", LogLevel.Warning) |> ignore
    let app = builder.Build()
    let log = app.Logger
    log.LogInformation("runtime {rt}, warmup {ms:F0} ms", Runtime.InteropServices.RuntimeInformation.FrameworkDescription, warmup ())

    app.MapGet("/", Func<obj>(fun () -> box info)) |> ignore

    app.MapPost("/start", Func<GameState, IResult>(fun gs ->
        log.LogInformation("start {game} {ruleset} {w}x{h} snakes={n}", gs.Game.Id, gs.Game.Ruleset.Name, gs.Board.Width, gs.Board.Height, gs.Board.Snakes.Length)
        Results.Ok())) |> ignore

    app.MapPost("/move", Func<GameState, obj>(fun gs ->
        let t0 = Stopwatch.GetTimestamp()
        let budget, net = Clock.budget gs.Game.Id gs.Game.Timeout gs.You.Latency
        let r = decide gs (t0 + msToTicks budget)
        let ms = Stopwatch.GetElapsedTime(t0).TotalMilliseconds
        Clock.record gs.Game.Id ms
        log.LogInformation(
            "move {game} t={turn} -> {dir} d={depth} score={score:F0} nodes={nodes} ({ms:F1}/{budget:F0} ms, net~{net:F0}, lat={lat})",
            gs.Game.Id, gs.Turn, Dir.toApi r.Move, r.Depth, r.Score, r.Nodes, ms, budget, net, gs.You.Latency)
        box {| move = Dir.toApi r.Move; shout = "" |})) |> ignore

    app.MapPost("/end", Func<GameState, IResult>(fun gs ->
        Clock.finish gs.Game.Id
        let won = gs.Board.Snakes |> Array.exists (fun s -> s.Id = gs.You.Id)
        log.LogInformation("end {game} turn={turn} {result}", gs.Game.Id, gs.Turn, (if won then "WIN/ALIVE" else "LOSS"))
        Results.Ok())) |> ignore

    let env name dflt = Environment.GetEnvironmentVariable name |> Option.ofObj |> Option.defaultValue dflt
    let host, port = env "HOST" "0.0.0.0", env "PORT" "8000"
    app.Run($"http://{host}:{port}")
    0
