/// Local self-play on the official standard map rules (BattlesnakeOfficial/rules: PlaceSnakesFixed,
/// PlaceFoodFixed, standard food spawning). Game rules come from our own engine, which the tests pin to the official one.
///
///   Arena match <botA> <botB> [--mode duel|standard] [--games MAX] [--sprt elo0,elo1] [--fixed] [--par P] [--seed S]
///     SPRT by default (H0 elo<=0 vs H1 elo>=20, alpha=beta=0.05), stops when decided; --fixed plays exactly --games
///   Arena tune  [--mode duel|standard] [--nodes N] [--iters N] [--batch B] [--from weights] [--par P]
///
///   bot:  base | search:<ms> | nodes:<n> | plain:<n> (no ordering) | hint:<n> (no score reuse)
///         | legacy:<n> (pre-0.6.1 proofs) | relaxnear:<n> (0.7.1 re-search) | ponder:<n>:<p> (p more nodes
///         after each move) | smp:<ms>:<threads>:<diversity>   optionally followed by @<weights>
///   weights: "name=value,..." or a path to a file in that format (see Weights.names)
module Snake.Arena.Program

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Snake.Engine

type Bot = Board -> int -> Dir

/// Throughput accounting across all games (for judging the arena itself, not the bots).
let mutable totalNodes = 0L

let count (r: SearchInfo) =
    Interlocked.Add(&totalNodes, r.Nodes) |> ignore
    r.Move

let stats' (sw: Diagnostics.Stopwatch) =
    let s = sw.Elapsed.TotalSeconds

    sprintf
        "%.1fM nodes/s, GC pause %.1fs, gen2 %d, alloc %.1f GB"
        (float totalNodes / s / 1e6)
        (GC.GetTotalPauseDuration().TotalSeconds)
        (GC.CollectionCount 2)
        (float (GC.GetTotalAllocatedBytes false) / 1e9)

let private readText (s: string) = if File.Exists s then File.ReadAllText s else s

/// A weights spec (name=value pairs or a file) over the defaults.
let parseWeights (s: string) = Weights.parse (readText s)

/// A bot that searches with `config` within the limits `limits ()` gives for each move.
let private searcher geo maxSnakes config (limits: unit -> Limits) : Bot =
    let sr = Searcher(geo, maxSnakes, config)
    fun b s -> count (sr.Search(b, s, limits ()))

let makeBotW (geo: Geometry) (maxSnakes: int) (spec: string) (w: Weights) : Bot =
    let cfg = SearchConfig.make w
    let nodes n = fun () -> Limits.nodes (int64 n)

    match spec.Split(':') with
    | [| "base" |] -> Policy.create geo maxSnakes
    | [| "search"; ms |] -> searcher geo maxSnakes cfg (fun () -> Limits.forMs (float ms * 1.0<ms>))
    | [| "nodes"; n |] -> searcher geo maxSnakes cfg (nodes n)
    // move ordering hints only, no score reuse
    | [| "hint"; n |] -> searcher geo maxSnakes { cfg with UseScores = false } (nodes n)
    // nodes:<n> with the pre-0.6.1 unsound proofs, for A/B
    | [| "legacy"; n |] -> searcher geo maxSnakes { cfg with SoundProofs = false } (nodes n)
    // nodes:<n> without move ordering, for A/B
    | [| "plain"; n |] -> searcher geo maxSnakes { cfg with Ordering = false } (nodes n)
    // nodes:<n> with 0.7.1's relaxed re-search that keeps opponents 2 steps away adversarial, for A/B
    | [| "relaxnear"; n |] -> searcher geo maxSnakes { cfg with RelaxNear = true } (nodes n)
    | [| "ponder"; n; p |] ->
        // After answering, keep searching the same position for p more nodes (the server does this while the
        // response travels and the other snakes think); the table carries it into the next turn.
        let sr = Searcher(geo, maxSnakes, cfg)

        fun b s ->
            let m = count (sr.Search(b, s, Limits.nodes (int64 n)))
            count (sr.Search(b, s, Limits.nodes (int64 p))) |> ignore
            m
    | [| "smp"; ms; t; div |] ->
        // Lazy SMP with t threads for ms per move
        let ps = ParallelSearcher(geo, maxSnakes, { cfg with Diversity = int div }, int t)
        fun b s -> count (ps.Search(b, s, Limits.forMs (float ms * 1.0<ms>), int t - 1))
    | _ -> failwith $"unknown bot {spec}"

/// `@...` overrides single weights on top of the mode's shipped ones (before 0.7.4 bots without @ played the
/// default weights, so every A/B that didn't spell out weights compared those, not what the server plays).
let makeBot geo maxSnakes (spec: string) =
    let basis = Weights.forSnakes maxSnakes

    match spec.IndexOf '@' with
    | -1 -> makeBotW geo maxSnakes spec basis
    | i -> makeBotW geo maxSnakes spec[.. i - 1] (Weights.parseOver basis (readText spec[i + 1 ..]))

let shuffle (rng: Random) (a: 'T[]) =
    for i in a.Length - 1 .. -1 .. 1 do
        let j = rng.Next(i + 1)
        let t = a[i]
        a[i] <- a[j]
        a[j] <- t

/// Official PlaceSnakesFixed + PlaceFoodFixed for an 11x11 board.
let setup (rng: Random) (n: int) =
    let geo = Geometry(11, 11)
    let b = Board(geo, n)
    let mn, md, mx = 1, 5, 9
    let corners = [| (mn, mn); (mn, mx); (mx, mn); (mx, mx) |]
    let cardinals = [| (mn, md); (md, mn); (md, mx); (mx, md) |]
    shuffle rng corners
    shuffle rng cardinals
    let starts = if rng.Next 2 = 0 then Array.append corners cardinals else Array.append cardinals corners

    for s in 0 .. n - 1 do
        let c = geo.Cell starts[s]
        b.AddSnake(100, [| c; c; c |]) |> ignore

    // One food diagonal to each head, away from the centre on at least one axis, never centre or corner
    let cx, cy = 5, 5

    for s in 0 .. n - 1 do
        let hx, hy = starts[s]

        let options =
            [| (hx - 1, hy - 1); (hx - 1, hy + 1); (hx + 1, hy - 1); (hx + 1, hy + 1) |]
            |> Array.filter (fun (px, py) ->
                (px, py) <> (cx, cy)
                && not (b.IsFood(geo.Cell(px, py)))
                && ((px < hx && hx < cx) || (cx < hx && hx < px) || (py < hy && hy < cy) || (cy < hy && hy < py))
                && not ((px = 0 || px = 10) && (py = 0 || py = 10)))

        if options.Length > 0 then b.SetFood(geo.Cell options[rng.Next options.Length], true)

    b.SetFood(geo.Cell(cx, cy), true)
    b

/// Standard map food: top up to minimumFood=1, else spawn one with foodSpawnChance=15. Like the official
/// GetUnoccupiedPoints(includePossibleMoves=false), cells next to a living head are excluded.
let spawnFood (b: Board) (rng: Random) =
    let geo = b.Geometry
    let mutable nFood = 0

    for c in 0 .. geo.Cells - 1 do
        if b.IsFood c then nFood <- nFood + 1

    if nFood < 1 || (100 - rng.Next 100) < 15 then
        let blocked = Array.zeroCreate<bool> geo.Cells

        for s in 0 .. b.NumSnakes - 1 do
            if b.Alive s then
                for d in Dir.all do
                    let nb = geo.Neighbor(b.Head s, d)
                    if nb >= 0 then blocked[nb] <- true

        let free =
            [| for c in 0 .. geo.Cells - 1 do
                   if b.Occupancy c = 0 && not (b.IsFood c) && not blocked[c] then c |]

        if free.Length > 0 then b.SetFood(free[rng.Next free.Length], true)

/// Plays one game; returns the winning seat (-1 = draw) and the number of turns.
let playBots (bots: Bot[]) (seed: int) =
    let rng = Random(seed)
    let n = bots.Length
    let b = setup rng n
    let moves = Array.zeroCreate<Dir> n

    while b.AliveCount > 1 && b.Turn < 1000 do
        for s in 0 .. n - 1 do
            if b.Alive s then moves[s] <- (bots[s]) b s

        b.Step moves
        spawnFood b rng

    let mutable winner = -1

    if b.AliveCount = 1 then
        for s in 0 .. n - 1 do
            if b.Alive s then winner <- s

    winner, b.Turn

let opt (argv: string[]) name dflt =
    match Array.tryFindIndex ((=) name) argv with
    | Some i when i + 1 < argv.Length -> argv[i + 1]
    | _ -> dflt

/// Seat layout: duel = A B, standard = A B A B; `flip` swaps A and B so both sides get both start orders.
let seats mode flip (a: 'T) (b: 'T) =
    let ab = if flip then [| b; a |] else [| a; b |]
    if mode = "duel" then ab else Array.append ab ab

let geo11 = Geometry(11, 11)

/// Elo <-> expected score (logistic, 400 scale)
let eloToScore (e: float) = 1.0 / (1.0 + Math.Pow(10.0, -e / 400.0))
let scoreToElo (s: float) = -400.0 * Math.Log10(1.0 / Math.Clamp(s, 1e-6, 1.0 - 1e-6) - 1.0)

/// Results of seat-swapped game pairs. A pair scores 0, 1/4, ..., 1 for A (the pentanomial model used by
/// fishtest): start-position luck mostly cancels inside a pair, so the variance is lower than per game.
type PairStats() =
    let counts = Array.zeroCreate<int> 5 // pair points for A in half-points: 0..4

    member _.Add(halfPoints: int) = counts[halfPoints] <- counts[halfPoints] + 1
    member _.Pairs = Array.sum counts
    member _.Counts = counts

    /// Mean and variance of the pair score with 0.5 pseudo-pairs per outcome: without them a short run of
    /// identical pairs has zero variance and the LLR explodes. Pulls towards 50%, i.e. errs on the safe side.
    member this.MeanVar() =
        let c k = float counts[k] + 0.5
        let n = Seq.sum (Seq.init 5 c)
        let mean = (Seq.sum (Seq.init 5 (fun k -> c k * float k / 4.0))) / n
        let var = (Seq.sum (Seq.init 5 (fun k -> c k * (float k / 4.0 - mean) ** 2.0))) / n
        mean, var

    /// GSPRT log-likelihood ratio of H1 (elo1) vs H0 (elo0), normal approximation (Van den Bergh).
    member this.Llr(elo0: float, elo1: float) =
        if this.Pairs < 2 then
            0.0
        else
            let mean, var = this.MeanVar()
            let s0, s1 = eloToScore elo0, eloToScore elo1
            float this.Pairs * (s1 - s0) * (2.0 * mean - s0 - s1) / (2.0 * var)

    /// Elo estimate and 95% interval
    member this.Elo() =
        let mean, var = this.MeanVar()
        let se = sqrt (var / float this.Pairs)
        scoreToElo mean, scoreToElo (mean - 1.96 * se), scoreToElo (mean + 1.96 * se)

/// A vs B. Games come in seat-swapped pairs with the same seed. By default runs an SPRT for H0: elo <= elo0
/// vs H1: elo >= elo1 (alpha = beta = 0.05) and stops as soon as it decides; --games is the cap.
/// --fixed plays exactly --games games.
let runMatch (argv: string[]) =
    let a, bSpec = argv[1], argv[2]
    let mode = opt argv "--mode" "duel"
    let fixedN = argv |> Array.contains "--fixed"
    let games = (int (opt argv "--games" (if fixedN then "100" else "2000")) + 1) / 2 * 2
    let par = int (opt argv "--par" (string Environment.ProcessorCount))
    let seed0 = int (opt argv "--seed" "1")
    let bounds = (opt argv "--sprt" "0,20").Split(',') |> Array.map float
    let elo0, elo1 = bounds[0], bounds[1]
    let lower, upper = Math.Log(0.05 / 0.95), Math.Log(0.95 / 0.05)
    let stats = PairStats()
    let results = Array.create games -1 // per game: half-points for A (0, 1, 2), -1 = not finished
    let mutable winsA = 0
    let mutable winsB = 0
    let mutable draws = 0
    let mutable turns = 0L
    let mutable played = 0
    let mutable verdict = ""
    let mutable stop = false
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable lastReport = 0.0

    let line () =
        let elo, lo, hi = stats.Elo()

        sprintf
            "%d pairs (%d games run), A %d / B %d / draw %d, elo %+.0f [%+.0f, %+.0f], LLR %.2f (%.2f, %.2f), pairs %A, %.0fs"
            stats.Pairs
            played
            winsA
            winsB
            draws
            elo
            lo
            hi
            (stats.Llr(elo0, elo1))
            lower
            upper
            stats.Counts
            sw.Elapsed.TotalSeconds

    // Games are handed out strictly in order (Parallel.For would give each thread a contiguous chunk, so a pair's
    // two games ran one after the other and pairs completed late), so both games of a pair run at the same time.
    let mutable next = -1

    let play g =
        let flip = g % 2 = 1
        let specs = seats mode flip a bSpec
        let isA = seats mode flip true false
        let bots = specs |> Array.map (makeBot geo11 specs.Length)
        let w, t = playBots bots (seed0 * 100_003 + g / 2)

        let r =
            if w < 0 || a = bSpec then 1
            elif isA[w] then 2
            else 0

        lock stats (fun () ->
            turns <- turns + int64 t
            played <- played + 1
            results[g] <- r

            if r = 2 then winsA <- winsA + 1
            elif r = 0 then winsB <- winsB + 1
            else draws <- draws + 1

            let other = results[g ^^^ 1]

            // Once decided, later finishers are not counted, so the report matches the decision
            if other >= 0 && not stop then
                stats.Add(r + other)
                let llr = stats.Llr(elo0, elo1)

                if not fixedN && stats.Pairs >= 10 && (llr <= lower || llr >= upper) then
                    verdict <-
                        if llr >= upper then
                            sprintf "H1 accepted (A >= %+.0f elo)" elo1
                        else
                            sprintf "H0 accepted (A <= %+.0f elo)" elo0

                    Volatile.Write(&stop, true)

                if sw.Elapsed.TotalSeconds - lastReport > 30.0 then
                    lastReport <- sw.Elapsed.TotalSeconds
                    eprintfn "  .. %s" (line ()))

    let worker () =
        let mutable g = Interlocked.Increment(&next)

        while g < games && not (Volatile.Read(&stop)) do
            play g
            g <- Interlocked.Increment(&next)

    Array.init par (fun _ -> Task.Factory.StartNew(worker, TaskCreationOptions.LongRunning)) |> Task.WaitAll

    if verdict = "" then verdict <- if fixedN then "fixed" else "inconclusive (hit --games)"
    printfn "%s vs %s (%s): %s" a bSpec mode verdict
    printfn "  %s, avg %.0f turns" (line ()) (float turns / float played)
    printfn "  arena: %s" (stats' sw)
    0

/// SPSA (as in chess engine tuning): each sample perturbs every weight by ±c_i at random, plays θ+ against θ-
/// on a seat-swapped game pair, and moves θ along the perturbation in proportion to the result.
let runTune (argv: string[]) =
    let mode = opt argv "--mode" "duel"
    let nodes = opt argv "--nodes" "3000"
    let iters = int (opt argv "--iters" "200")
    let batch = int (opt argv "--batch" "64")
    let par = int (opt argv "--par" (string Environment.ProcessorCount))
    let out = opt argv "--out" $"weights-{mode}.txt"

    let theta =
        (match opt argv "--from" "" with
         | "" -> Weights.defaults
         | s -> parseWeights s)
        |> Weights.toArray

    let k = theta.Length
    // Perturbation ~15% of the starting value (at least 0.5); integer-like weights still move since we round only for lenCap
    let c = theta |> Array.map (fun x -> max 0.5 (0.15 * abs x))
    // Some weights must stay in sensible ranges
    let lo = Array.create k 0.0
    let hi = theta |> Array.map (fun x -> max 10.0 (4.0 * x))
    let lr = float (opt argv "--lr" "0.4")
    let rngMaster = Random(int (opt argv "--seed" "7"))
    let sw = Diagnostics.Stopwatch.StartNew()

    printfn
        "tuning %s at nodes:%s, %d iters x %d pairs; start %s"
        mode
        nodes
        iters
        batch
        (Weights.format (Weights.ofArray theta))

    for it in 1..iters do
        let deltas = Array.init batch (fun _ -> Array.init k (fun _ -> if rngMaster.Next 2 = 0 then -1.0 else 1.0))
        let seeds = Array.init batch (fun _ -> rngMaster.Next())
        let results = Array.zeroCreate<float> batch
        let snapshot = Array.copy theta

        Parallel.For(
            0,
            batch * 2,
            ParallelOptions(MaxDegreeOfParallelism = par),
            fun j ->
                let i = j / 2
                let flip = j % 2 = 1

                let plus =
                    Weights.ofArray (
                        Array.init k (fun p -> Math.Clamp(snapshot[p] + c[p] * deltas[i][p], lo[p], hi[p]))
                    )

                let minus =
                    Weights.ofArray (
                        Array.init k (fun p -> Math.Clamp(snapshot[p] - c[p] * deltas[i][p], lo[p], hi[p]))
                    )

                let ws = seats mode flip plus minus
                let isPlus = seats mode flip true false
                let bots = ws |> Array.map (fun w -> makeBotW geo11 ws.Length ("nodes:" + nodes) w)
                let w, _ = playBots bots seeds[i]

                let r =
                    if w < 0 then 0.0
                    elif isPlus[w] then 1.0
                    else -1.0
                // Two writers per slot (the pair); lock-free add on doubles is not available, so use a lock
                lock results (fun () -> results[i] <- results[i] + r)
        )
        |> ignore

        // Decay learning rate slowly, as usual for SPSA
        let a = lr / Math.Pow(1.0 + float it / 50.0, 0.602)

        for p in 0 .. k - 1 do
            let mutable g = 0.0

            for i in 0 .. batch - 1 do
                g <- g + results[i] * deltas[i][p]

            theta[p] <- Math.Clamp(theta[p] + a * c[p] * g / float batch * 2.0, lo[p], hi[p])

        let decisive = results |> Array.sumBy abs
        File.WriteAllText(out, (Weights.format (Weights.ofArray theta)).Replace(",", "\n") + "\n")

        if it % 5 = 0 || it = iters then
            printfn
                "[%4d %5.0fs] decisive %.0f%%  %s"
                it
                sw.Elapsed.TotalSeconds
                (100.0 * decisive / float (2 * batch))
                (Weights.format (Weights.ofArray theta))

    0

/// Diagnostic: along real games, nodes needed to complete depth D at each turn with a fresh table vs with the
/// table left by the previous turn (searched to depth D, optionally pondered to D+1).
let runReuse (argv: string[]) =
    let mode = opt argv "--mode" "duel"
    let d = int (opt argv "--depth" "7")
    let ponder = argv |> Array.contains "--ponder"
    let n = if mode = "duel" then 2 else 4
    let cfg = SearchConfig.make (Weights.forSnakes n)
    let mutable fresh = 0L
    let mutable warm = 0L
    let mutable turns = 0

    for g in 1..6 do
        let rng = Random(g)
        let b = setup rng n
        let persistent = Searcher(geo11, n, cfg)
        let mv = Array.zeroCreate<Dir> n
        let bots = Array.init n (fun _ -> makeBotW geo11 n "nodes:20000" cfg.Weights)

        while b.AliveCount > 1 && b.Turn < 300 && b.Alive 0 do
            let rf = Searcher(geo11, n, cfg).Search(b, 0, Limits.depth d)
            let rp = persistent.Search(b, 0, Limits.depth d)

            if b.Turn > 0 && rf.Depth = d then
                fresh <- fresh + rf.Nodes
                warm <- warm + rp.Nodes
                turns <- turns + 1

            if ponder then persistent.Search(b, 0, Limits.depth (d + 1)) |> ignore

            mv[0] <- rf.Move

            for s in 1 .. n - 1 do
                if b.Alive s then mv[s] <- (bots[s]) b s

            b.Step mv
            spawnFood b rng

    printfn
        "%s depth %d ponder=%b: %d turns, fresh %d nodes, warm %d nodes (%.2fx fewer)"
        mode
        d
        ponder
        turns
        fresh
        warm
        (float fresh / float warm)

    0

/// Diagnostic: average completed depth per turn at a node budget, for a fresh table, a persistent table, and a
/// persistent table with p ponder nodes after each move. The game is driven by the plain persistent searcher.
let runDepth (argv: string[]) =
    let mode = opt argv "--mode" "duel"
    let nodes = int64 (opt argv "--nodes" "30000")
    let p = int64 (opt argv "--ponder" "18000")
    let n = if mode = "duel" then 2 else 4
    let cfg = SearchConfig.make (Weights.forSnakes n)
    let sum = Array.zeroCreate<int64> 3
    let mutable turns = 0

    for g in 1..8 do
        let rng = Random(100 + g)
        let b = setup rng n
        let sr = Array.init 3 (fun _ -> Searcher(geo11, n, cfg))
        let mv = Array.zeroCreate<Dir> n
        let bots = Array.init n (fun _ -> makeBotW geo11 n ("nodes:" + string nodes) cfg.Weights)

        while b.AliveCount > 1 && b.Turn < 300 && b.Alive 0 do
            let r =
                Array.init 3 (fun i ->
                    let s = if i = 0 then Searcher(geo11, n, cfg) else sr[i]
                    s.Search(b, 0, Limits.nodes nodes))

            for i in 0..2 do
                sum[i] <- sum[i] + int64 r[i].Depth

            turns <- turns + 1
            sr[2].Search(b, 0, Limits.nodes p) |> ignore
            mv[0] <- r[1].Move

            for s in 1 .. n - 1 do
                if b.Alive s then mv[s] <- (bots[s]) b s

            b.Step mv
            spawnFood b rng

    let avg i = float sum[i] / float turns

    printfn
        "%s nodes=%d ponder=%d, %d turns: avg depth fresh %.2f, persistent %.2f, +ponder %.2f"
        mode
        nodes
        p
        turns
        (avg 0)
        (avg 1)
        (avg 2)

    0

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        eprintfn "usage: Arena match <botA> <botB> ... | Arena tune ..."
        2
    | _ when argv[0] = "match" -> runMatch argv
    | _ when argv[0] = "tune" -> runTune argv
    | _ when argv[0] = "reuse" -> runReuse argv
    | _ when argv[0] = "depth" -> runDepth argv
    | _ when argv[0] = "texel" ->
        Texel.run argv
        0
    | _ when argv[0] = "features" ->
        Texel.dump argv
        0
    | _ -> runMatch (Array.append [| "match" |] argv)
