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
      /// A/B arm: the 0.6.0 setup (one thread, pre-0.6.1 proofs) for this game
      Legacy: bool
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

/// Ladder A/B (SNAKE_AB, default on): Standard games whose id starts with an even hex digit run the 0.6.0 setup,
/// so both arms meet the same opponents at the same time. Over periods the two can't be told apart: matchmaking
/// moved to stronger opponents just as 0.6.1/0.7 went live.
let private abTest = Environment.GetEnvironmentVariable "SNAKE_AB" <> "0"

/// Whether a game plays the legacy arm, from its id alone so the logs and game ids can be split afterwards.
let legacyArm (gameId: string) (snakes: int) =
    abTest && snakes > 2 && gameId.Length > 0 && Uri.IsHexDigit gameId.[0] && Convert.ToInt32(string gameId.[0], 16) % 2 = 0

/// Games whose /end never arrived (engine hiccup, restart in between) get dropped after this long.
let private idleTicks = TimeSpan.FromMinutes(10.0).Ticks

let private sweep () =
    let now = DateTime.UtcNow.Ticks

    for kv in sessions do
        if now - Volatile.Read(&kv.Value.LastUsed) > idleTicks then sessions.TryRemove kv.Key |> ignore

let private make (geo: Geometry) (maxSnakes: int) (w: Weights) (n: int) (legacy: bool) =
    let p = ParallelSearcher(geo, maxSnakes, w, (if legacy then 1 else n))
    p.Diversity <- diversity
    p.SoundProofs <- not legacy
    p

/// Searches with this game's ParallelSearcher for the board's size and snake count, using the cores not taken by
/// other running searches. Requests for one game never overlap in practice; if they ever do, the late one gets a
/// throwaway single-threaded Searcher rather than waiting.
let search (gameId: string) (b: Board) (me: int) (w: Weights) (stopAt: int64) : SearchInfo =
    if sessions.Count > 64 then sweep ()

    let s =
        sessions.GetOrAdd(
            gameId,
            fun _ ->
                { Searchers = ConcurrentDictionary()
                  Lock = obj ()
                  Legacy = legacyArm gameId b.MaxSnakes
                  LastUsed = 0L }
        )

    Volatile.Write(&s.LastUsed, DateTime.UtcNow.Ticks)
    let geo = b.Geometry
    let key = struct (geo.Width, geo.Height, b.MaxSnakes)

    let running = Interlocked.Increment(&active)

    try
        let helpers = if s.Legacy then 0 else max 0 (threads / running - 1)

        if Monitor.TryEnter s.Lock then
            try
                let p = s.Searchers.GetOrAdd(key, fun _ -> make geo b.MaxSnakes w threads s.Legacy)
                p.Search(b, me, stopAt, helpers)
            finally
                Monitor.Exit s.Lock
        else
            (make geo b.MaxSnakes w 1 s.Legacy).Search(b, me, stopAt, 0)
    finally
        Interlocked.Decrement(&active) |> ignore

let finish (gameId: string) = sessions.TryRemove gameId |> ignore
let count () = sessions.Count
