/// Per-game Searchers. The Searcher's table and move history carry over from one turn to the next (measured:
/// about +0.35 ply in Duels at equal nodes), so each game keeps its own instead of a fresh one per request.
///
/// A game uses one Searcher per snake count: when a snake dies the board is rebuilt with fewer slots and the
/// indices shift, so old entries would not match anyway, and the weights switch to the Duel set at 2 snakes.
module Snake.Server.Sessions

open System
open System.Collections.Concurrent
open System.Threading
open Snake.Engine

type private Session =
    { Searchers: ConcurrentDictionary<struct (int * int * int), Searcher>
      Lock: obj
      mutable LastUsed: int64 }

let private sessions = ConcurrentDictionary<string, Session>()

/// Games whose /end never arrived (engine hiccup, restart in between) get dropped after this long.
let private idleTicks = TimeSpan.FromMinutes(10.0).Ticks

let private sweep () =
    let now = DateTime.UtcNow.Ticks

    for kv in sessions do
        if now - Volatile.Read(&kv.Value.LastUsed) > idleTicks then sessions.TryRemove kv.Key |> ignore

/// Runs `f` with this game's Searcher for the board's size and snake count. Requests for one game never overlap
/// in practice; if they ever do, the late one gets a throwaway Searcher rather than waiting.
let withSearcher (gameId: string) (b: Board) (w: Weights) (f: Searcher -> 'T) : 'T =
    if sessions.Count > 64 then sweep ()

    let s =
        sessions.GetOrAdd(gameId, fun _ -> { Searchers = ConcurrentDictionary(); Lock = obj (); LastUsed = 0L })

    Volatile.Write(&s.LastUsed, DateTime.UtcNow.Ticks)
    let geo = b.Geometry
    let key = struct (geo.Width, geo.Height, b.MaxSnakes)

    if Monitor.TryEnter s.Lock then
        try
            f (s.Searchers.GetOrAdd(key, fun _ -> Searcher(geo, b.MaxSnakes, w)))
        finally
            Monitor.Exit s.Lock
    else
        f (Searcher(geo, b.MaxSnakes, w))

let finish (gameId: string) = sessions.TryRemove gameId |> ignore
let count () = sessions.Count
