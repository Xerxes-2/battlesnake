/// Per-turn time budget. The engine's timeout (usually 500ms) covers the network round trip too, so the search
/// gets: timeout - estimated network time - safety margin.
///
/// The network time is learned per game from `you.latency`, which is the engine-measured response time of our
/// previous turn: latency - our compute time last turn = network. Spikes are adopted at once, improvements decay in.
module Snake.Server.Clock

open System
open System.Collections.Concurrent

open Snake.Engine

/// Per game: network estimate and our compute time last turn (NaN = not known yet).
type private GameClock = { Net: float<ms>; LastCompute: float<ms> }

let private unknown: float<ms> = LanguagePrimitives.FloatWithMeasure nan
let private known (t: float<ms>) = not (Double.IsNaN(float t))

let private games = ConcurrentDictionary<string, GameClock>()

let private envMs name (dflt: float<ms>) =
    match Double.TryParse(Environment.GetEnvironmentVariable name) with
    | true, x -> x * 1.0<ms>
    | _ -> dflt

/// Network estimate before any measurement: a fresh TCP connection Sydney <-> Oregon is ~2 x 165ms.
let firstTurnNet = envMs "SNAKE_FIRST_NET_MS" 330.0<ms>
/// Covers GC, scheduling jitter and the time between our clock stopping and the bytes leaving the box.
let margin = envMs "SNAKE_MARGIN_MS" 80.0<ms>

/// The network estimate after the engine reported `latency` (ms, as sent) for a turn we computed for
/// `lastCompute` ms: spikes are adopted at once, improvements decay in.
let updateNet (net: float<ms>) (lastCompute: float<ms>) (latency: string) =
    match Int32.TryParse latency with
    | true, l when l > 0 && known lastCompute ->
        let measured = max 0.0<ms> (float l * 1.0<ms> - lastCompute)
        if not (known net) || measured > net then measured else 0.5 * net + 0.5 * measured
    | _ -> net

/// Search time for an estimated network time (NaN = unknown) and the engine's timeout in ms (0 = default 500).
let budgetFor (net: float<ms>) (timeout: int) =
    let net = if known net then net else firstTurnNet
    let timeout = if timeout > 0 then float timeout * 1.0<ms> else 500.0<ms>
    min (timeout - 50.0<ms>) (max 10.0<ms> (timeout - net - margin)), net

/// Turns before this keep at least `firstTurnNet`: the engine sometimes opens a fresh connection on turns 1-2 as
/// well (ladder 0.7.6: net max 252/325ms on t=1/2 vs a 163ms median), and a timeout there repeats our opening move
/// from a corner into the wall — 5 dead-on-turn-2 losses in ~600 games. Opening search depth costs nothing.
let coldTurns = 3

/// Search time for this turn, and the network estimate it assumed.
let budget (gameId: string) (turn: int) (timeout: int) (latency: string) =
    let budgetFor net timeout = budgetFor (if turn < coldTurns && known net then max net firstTurnNet else net) timeout

    if games.Count > 10_000 then games.Clear() // games whose /end never arrived

    let c = games.GetOrAdd(gameId, fun _ -> { Net = unknown; LastCompute = unknown })
    let c = { c with Net = updateNet c.Net c.LastCompute latency }
    games[gameId] <- c
    budgetFor c.Net timeout

let record (gameId: string) (computeMs: float<ms>) =
    match games.TryGetValue gameId with
    | true, c -> games[gameId] <- { c with LastCompute = computeMs }
    | _ -> ()

let finish (gameId: string) = games.TryRemove gameId |> ignore
