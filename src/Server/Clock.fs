/// Per-turn time budget. The engine's timeout (usually 500ms) covers the network round trip too, so the search
/// gets: timeout - estimated network time - safety margin.
///
/// The network time is learned per game from `you.latency`, which is the engine-measured response time of our
/// previous turn: latency - our compute time last turn = network. Spikes are adopted at once, improvements decay in.
module Snake.Server.Clock

open System
open System.Collections.Concurrent

type private GameClock =
    { mutable Net: float
      mutable LastCompute: float }

let private games = ConcurrentDictionary<string, GameClock>()

let private envFloat name (dflt: float) =
    match Environment.GetEnvironmentVariable name with
    | null -> dflt
    | v ->
        match Double.TryParse v with
        | true, x -> x
        | _ -> dflt

/// Network estimate before any measurement: a fresh TCP connection Sydney <-> Oregon is ~2 x 165ms.
let firstTurnNet = envFloat "SNAKE_FIRST_NET_MS" 330.0
/// Covers GC, scheduling jitter and the time between our clock stopping and the bytes leaving the box.
let margin = envFloat "SNAKE_MARGIN_MS" 80.0

/// Search time in milliseconds for this turn.
let budget (gameId: string) (timeout: int) (latency: string) =
    if games.Count > 10_000 then games.Clear() // games whose /end never arrived

    let c = games.GetOrAdd(gameId, fun _ -> { Net = nan; LastCompute = nan })

    match Int32.TryParse latency with
    | true, l when l > 0 && not (Double.IsNaN c.LastCompute) ->
        let net = max 0.0 (float l - c.LastCompute)
        c.Net <- if Double.IsNaN c.Net || net > c.Net then net else 0.5 * c.Net + 0.5 * net
    | _ -> ()

    let net = if Double.IsNaN c.Net then firstTurnNet else c.Net
    let timeout = if timeout > 0 then float timeout else 500.0
    Math.Clamp(timeout - net - margin, 10.0, timeout - 50.0), net

let record (gameId: string) (computeMs: float) =
    match games.TryGetValue gameId with
    | true, c -> c.LastCompute <- computeMs
    | _ -> ()

let finish (gameId: string) = games.TryRemove gameId |> ignore
