namespace Snake.Engine

open System.Diagnostics

/// Milliseconds (budgets, latencies) and Stopwatch ticks (deadlines): units of measure keep the two from mixing,
/// at no runtime cost.
[<Measure>]
type ms

[<Measure>]
type tick

module Time =
    let inline now () : int64<tick> = LanguagePrimitives.Int64WithMeasure(Stopwatch.GetTimestamp())

    let never: int64<tick> = LanguagePrimitives.Int64WithMeasure System.Int64.MaxValue

    let ticks (t: float<ms>) : int64<tick> =
        LanguagePrimitives.Int64WithMeasure(int64 (float t * float Stopwatch.Frequency / 1000.0))

    /// The deadline `t` from now.
    let after (t: float<ms>) = now () + ticks t

    /// Time since the timestamp `t0`.
    let since (t0: int64<tick>) : float<ms> =
        LanguagePrimitives.FloatWithMeasure(float (int64 (now () - t0)) * 1000.0 / float Stopwatch.Frequency)
