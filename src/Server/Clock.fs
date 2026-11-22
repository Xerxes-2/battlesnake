/// Per-turn time budget. The engine's timeout (usually 500ms) covers the network round trip too, so the search
/// gets: timeout - estimated network time - safety margin.
///
/// The network time is learned per game from `you.latency`, which is the engine-measured response time of our
/// previous turn: latency - our compute time last turn = network. The estimate is the worst of the last `window`
/// measurements: once a game's network turns jittery it stays jittery (ladder 11-17..22: 162 ms flat in most games,
/// 170-480 ms in about a quarter), and a mean lets the next spike through.
module Snake.Server.Clock

open System
open System.Collections.Concurrent

open Snake.Engine

/// Per game: the last `window` network measurements (newest last) and our compute time last turn (NaN = not known yet).
type private GameClock = { Recent: float<ms> list; LastCompute: float<ms> }

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

/// Network measurements kept per game. Replaying the ladder nets, the max of the last 20 halved the timeouts that
/// start a run (0.64% -> 0.28% of moves) for 7 ms less average search than the old spike-then-halve estimate.
let window = 20

/// Adds the network time measured from the engine's `latency` (ms, as sent) for a turn we computed for
/// `lastCompute` ms. A timed-out turn reports the timeout itself, so that sample is only a lower bound.
let addSample (recent: float<ms> list) (lastCompute: float<ms>) (latency: string) =
    match Int32.TryParse latency with
    | true, l when l > 0 && known lastCompute ->
        let measured = max 0.0<ms> (float l * 1.0<ms> - lastCompute)
        let recent = recent @ [ measured ]
        if recent.Length > window then List.tail recent else recent
    | _ -> recent

/// The network estimate: the worst recent measurement, NaN before the first one.
let estimate (recent: float<ms> list) =
    if recent.IsEmpty then unknown else List.max recent

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

    let c = games.GetOrAdd(gameId, fun _ -> { Recent = []; LastCompute = unknown })
    // `latency` is for turn - 1; cold-connection turns would hold the budget down for `window` turns
    let c = if turn > coldTurns then { c with Recent = addSample c.Recent c.LastCompute latency } else c
    games[gameId] <- c
    budgetFor (estimate c.Recent) timeout

let record (gameId: string) (computeMs: float<ms>) =
    match games.TryGetValue gameId with
    | true, c -> games[gameId] <- { c with LastCompute = computeMs }
    | _ -> ()

let finish (gameId: string) = games.TryRemove gameId |> ignore
