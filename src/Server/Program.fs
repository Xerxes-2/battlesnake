module Snake.Server.Program

open System
open System.Diagnostics
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Snake.Engine
open Snake.Server.Api

let decide (gs: GameState) =
    let b, me = toBoard gs
    Policy(b.Geometry, b.MaxSnakes).Decide(b, me)

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

    let sw = Stopwatch.StartNew()
    for _ in 1..500 do decide gs |> ignore
    sw.Elapsed.TotalMilliseconds

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateSlimBuilder(args)
    let app = builder.Build()
    let log = app.Logger
    log.LogInformation("runtime {rt}, warmup {ms:F0} ms", Runtime.InteropServices.RuntimeInformation.FrameworkDescription, warmup ())

    app.MapGet("/", Func<obj>(fun () -> box info)) |> ignore

    app.MapPost("/start", Func<GameState, IResult>(fun gs ->
        log.LogInformation("start {game} {ruleset} {w}x{h} snakes={n}", gs.Game.Id, gs.Game.Ruleset.Name, gs.Board.Width, gs.Board.Height, gs.Board.Snakes.Length)
        Results.Ok())) |> ignore

    app.MapPost("/move", Func<GameState, obj>(fun gs ->
        let t0 = Stopwatch.GetTimestamp()
        let d = decide gs
        let ms = Stopwatch.GetElapsedTime(t0).TotalMilliseconds
        log.LogInformation("move {game} t={turn} -> {dir} ({ms:F1} ms)", gs.Game.Id, gs.Turn, Dir.toApi d, ms)
        box {| move = Dir.toApi d; shout = "" |})) |> ignore

    app.MapPost("/end", Func<GameState, IResult>(fun gs ->
        let won = gs.Board.Snakes |> Array.exists (fun s -> s.Id = gs.You.Id)
        log.LogInformation("end {game} turn={turn} {result}", gs.Game.Id, gs.Turn, (if won then "WIN/ALIVE" else "LOSS"))
        Results.Ok())) |> ignore

    let env name dflt = Environment.GetEnvironmentVariable name |> Option.ofObj |> Option.defaultValue dflt
    let host, port = env "HOST" "0.0.0.0", env "PORT" "8000"
    app.Run($"http://{host}:{port}")
    0
