module Snake.Server.Program

open System
open System.Diagnostics
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Snake.Engine
open Snake.Server.Api

/// Searches until `stopAt` (a Stopwatch timestamp) with this game's persistent searcher.
let decide (gs: GameState) (stopAt: int64<tick>) =
    let b, me = toBoard gs
    Sessions.search gs.Game.Id b me stopAt

/// Startup warmup: run the decision path a few hundred times on a fake board so the JIT promotes the hot paths to optimized code (tier 1).
let warmup () =
    let mk id (x, y) =
        { Id = id
          Name = id
          Health = 90
          Body = [| for i in 0..2 -> { X = x; Y = y - i } |]
          Head = { X = x; Y = y }
          Length = 3
          Latency = "0"
          Shout = ""
          Squad = "" }

    let snakes = [| mk "a" (1, 3); mk "b" (9, 3); mk "c" (1, 9); mk "d" (9, 9) |]

    let gs =
        { Game =
            { Id = "warmup"
              Ruleset =
                { Name = "standard"
                  Version = ""
                  Settings = { FoodSpawnChance = 15; MinimumFood = 1; HazardDamagePerTurn = 0 } }
              Map = "standard"
              Timeout = 500
              Source = "" }
          Turn = 5
          Board =
            { Height = 11
              Width = 11
              Food = [| { X = 5; Y = 5 } |]
              Hazards = [||]
              Snakes = snakes }
          You = snakes[0] }

    // Many short searches: tiering promotes a method after ~30 calls, then OSR/PGO need loops to run
    let sw = Stopwatch.StartNew()

    for _ in 1..60 do
        decide gs (Time.after 5.0<ms>) |> ignore

    Sessions.finish "warmup"
    sw.Elapsed.TotalMilliseconds

[<EntryPoint>]
let main args =
    // Each /move blocks its thread for the whole search (~260 ms). The pool starts with one thread per core and
    // adds more only slowly, so with more concurrent games than cores a request waited for a thread before its
    // clock started (measured on the 4-core A1: 6+ concurrent games gave 516-766 ms responses). With threads
    // available up front every request starts at once; the OS shares the cores and each search still stops on time.
    Threading.ThreadPool.GetMinThreads() |> fun (_, io) -> Threading.ThreadPool.SetMinThreads(64, io) |> ignore
    let builder = WebApplication.CreateSlimBuilder(args)
    // In code rather than appsettings.json: the Nix package runs without a content root next to the binary,
    // and per-request framework logs would double the journal for no information.
    builder.Logging.AddFilter("Microsoft", LogLevel.Warning) |> ignore
    let app = builder.Build()
    let log = app.Logger

    log.LogInformation(
        "snake-server {v}, runtime {rt}, threads {th}, warmup {ms:F0} ms",
        version,
        Runtime.InteropServices.RuntimeInformation.FrameworkDescription,
        Sessions.threads,
        warmup ()
    )

    app.MapGet("/", Func<_>(fun () -> info)) |> ignore

    app.MapPost(
        "/start",
        Func<GameState, IResult>(fun gs ->
            log.LogInformation(
                "start {game} {ruleset} {w}x{h} snakes={n} arm={arm}",
                gs.Game.Id,
                rulesetName gs.Game,
                gs.Board.Width,
                gs.Board.Height,
                (orEmpty gs.Board.Snakes).Length,
                (if Sessions.legacyArm gs.Game.Id then "legacy" else "new")
            )

            Results.Ok())
    )
    |> ignore

    app.MapPost(
        "/move",
        Func<GameState, _>(fun gs ->
            let t0 = Time.now ()
            let budget, net = Clock.budget gs.Game.Id gs.Game.Timeout gs.You.Latency
            let r = decide gs (t0 + Time.ticks budget)
            let took = Time.since t0
            Clock.record gs.Game.Id took

            log.LogInformation(
                "move {game} t={turn} -> {dir} d={depth}{relaxed} score={score:F0} nodes={nodes} ({ms:F1}/{budget:F0} ms, net~{net:F0}, lat={lat})",
                gs.Game.Id,
                gs.Turn,
                Dir.toApi r.Move,
                r.Depth,
                (if r.Relaxed then "R" else ""),
                r.Score,
                r.Nodes,
                float took,
                float budget,
                float net,
                gs.You.Latency
            )

            {| move = Dir.toApi r.Move; shout = "" |})
    )
    |> ignore

    app.MapPost(
        "/end",
        Func<GameState, IResult>(fun gs ->
            Clock.finish gs.Game.Id
            Sessions.finish gs.Game.Id
            let won = orEmpty gs.Board.Snakes |> Array.exists (fun s -> s.Id = gs.You.Id)

            log.LogInformation(
                "end {game} turn={turn} {result}",
                gs.Game.Id,
                gs.Turn,
                (if won then "WIN/ALIVE" else "LOSS")
            )

            Results.Ok())
    )
    |> ignore

    let env name dflt = Environment.GetEnvironmentVariable name |> Option.ofObj |> Option.defaultValue dflt
    let host, port = env "HOST" "0.0.0.0", env "PORT" "8000"
    app.Run($"http://{host}:{port}")
    0
