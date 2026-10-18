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
    { Searchers: ConcurrentDictionary<struct (int * int * int), ParallelSearcher>
      Lock: obj
      mutable LastUsed: int64 }

let private sessions = ConcurrentDictionary<string, Session>()

/// Threads per search when this is the only game running (SNAKE_THREADS, default: all cores). Measured on the
/// 4-core A1: Lazy SMP with 4 threads reaches about the depth of a single thread with 1.9x the time.
let threads =
    match Int32.TryParse(Environment.GetEnvironmentVariable "SNAKE_THREADS") with
    | true, n when n >= 1 -> n
    | _ -> Environment.ProcessorCount

/// Searches running right now. Nearly always 1 on the ladder (>99% of moves), but games that overlap share the
/// cores instead of each starting `threads` threads.
let mutable private active = 0

/// Diversity mode for the helpers (see Searcher.Diversity), switchable for experiments.
let private diversity =
    match Int32.TryParse(Environment.GetEnvironmentVariable "SNAKE_SMP_DIVERSITY") with
    | true, n -> n
    | _ -> 2

/// Games whose /end never arrived (engine hiccup, restart in between) get dropped after this long.
let private idleTicks = TimeSpan.FromMinutes(10.0).Ticks

let private sweep () =
    let now = DateTime.UtcNow.Ticks

    for kv in sessions do
        if now - Volatile.Read(&kv.Value.LastUsed) > idleTicks then sessions.TryRemove kv.Key |> ignore

let private make (geo: Geometry) (maxSnakes: int) (w: Weights) (n: int) =
    let p = ParallelSearcher(geo, maxSnakes, w, n)
    p.Diversity <- diversity
    p

/// Searches with this game's ParallelSearcher for the board's size and snake count, using the cores not taken by
/// other running searches. Requests for one game never overlap in practice; if they ever do, the late one gets a
/// throwaway single-threaded Searcher rather than waiting.
let search (gameId: string) (b: Board) (me: int) (w: Weights) (stopAt: int64) : SearchInfo =
    if sessions.Count > 64 then sweep ()

    let s =
        sessions.GetOrAdd(gameId, fun _ -> { Searchers = ConcurrentDictionary(); Lock = obj (); LastUsed = 0L })

    Volatile.Write(&s.LastUsed, DateTime.UtcNow.Ticks)
    let geo = b.Geometry
    let key = struct (geo.Width, geo.Height, b.MaxSnakes)

    let running = Interlocked.Increment(&active)

    try
        let helpers = max 0 (threads / running - 1)

        if Monitor.TryEnter s.Lock then
            try
                let p = s.Searchers.GetOrAdd(key, fun _ -> make geo b.MaxSnakes w threads)
                p.Search(b, me, stopAt, helpers)
            finally
                Monitor.Exit s.Lock
        else
            (make geo b.MaxSnakes w 1).Search(b, me, stopAt, 0)
    finally
        Interlocked.Decrement(&active) |> ignore

let finish (gameId: string) = sessions.TryRemove gameId |> ignore
let count () = sessions.Count
