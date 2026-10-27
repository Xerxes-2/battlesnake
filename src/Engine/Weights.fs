namespace Snake.Engine

open System
open System.Globalization

/// Evaluation weights. The names (see Weights.names) are the stable interface: weight files, logs, arena specs.
type Weights =
    {
        Area: float
        OppArea: float
        Len: float
        LenCap: float
        Trapped: float
        Starve: float
        NoFoodHp: float
        NoFood: float
        HungryHp: float
        FoodHungry: float
        FoodBehind: float
        FoodAhead: float
        LeadLen: float
        /// Territory below trapRatio x length counts as trapped (1 = can't even hold the body).
        TrapRatio: float
    }

module Weights =
    let names =
        [| "area"
           "oppArea"
           "len"
           "lenCap"
           "trapped"
           "starve"
           "noFoodHp"
           "noFood"
           "hungryHp"
           "foodHungry"
           "foodBehind"
           "foodAhead"
           "leadLen"
           "trapRatio" |]

    /// Flat view in `names` order, for tuners that perturb weights generically.
    let toArray (w: Weights) =
        [| w.Area
           w.OppArea
           w.Len
           w.LenCap
           w.Trapped
           w.Starve
           w.NoFoodHp
           w.NoFood
           w.HungryHp
           w.FoodHungry
           w.FoodBehind
           w.FoodAhead
           w.LeadLen
           w.TrapRatio |]

    /// Inverse of toArray. 13 values (files from before trapRatio) leave it at 1.
    let ofArray (v: float[]) =
        { Area = v[0]
          OppArea = v[1]
          Len = v[2]
          LenCap = v[3]
          Trapped = v[4]
          Starve = v[5]
          NoFoodHp = v[6]
          NoFood = v[7]
          HungryHp = v[8]
          FoodHungry = v[9]
          FoodBehind = v[10]
          FoodAhead = v[11]
          LeadLen = v[12]
          TrapRatio = if v.Length > 13 then v[13] else 1.0 }

    /// The hand-picked starting point.
    let defaults = ofArray [| 10.0; 4.0; 30.0; 4.0; 50.0; 5000.0; 30.0; 10.0; 30.0; 8.0; 4.0; 1.0; 2.0; 1.0 |]

    /// `text` ("name=value,name=value", one per line also works) applied over `basis`. Unknown names are an error.
    let parseOver (basis: Weights) (text: string) =
        let v = toArray basis

        for part in
            text.Split([| ','; '\n'; ';' |], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) do
            match part.Split('=') with
            | [| k; x |] ->
                match Array.tryFindIndex ((=) (k.Trim())) names with
                | Some i -> v[i] <- Double.Parse(x.Trim(), CultureInfo.InvariantCulture)
                | None -> failwith $"unknown weight {k}"
            | _ -> failwith $"bad weight entry {part}"

        ofArray v

    let parse = parseOver defaults

    let format (w: Weights) =
        Array.map2
            (fun (n: string) (x: float) -> n + "=" + x.ToString("0.###", CultureInfo.InvariantCulture))
            names
            (toArray w)
        |> String.concat ","

    /// Tuned by `Arena tune --mode duel --nodes 5000` (SPSA, ~19k games), then refined at `--nodes 30000` (100 iters x 32
    /// pairs, ~6.4k games; SPRT vs the 5k weights at 30k nodes: +35 elo [+12, +59]); copy of tune/weights-duel-30k.txt.
    /// Area and oppArea are nearly collinear 1v1 (they sum to roughly the free board), so their split is arbitrary.
    /// Before 0.7.5, kept for the ladder A/B.
    let duelPrev =
        parse
            "area=1.973,oppArea=1.681,len=100.778,lenCap=5.506,trapped=32.391,starve=2854.777,noFoodHp=28.094,noFood=9.887,hungryHp=49.46,foodHungry=7.999,foodBehind=3.469,foodAhead=0.118,leadLen=1.259"

    /// trapRatio 1 -> 3 (0.7.5): on 570 ladder duels, territory under twice the length was the strongest
    /// predictor of the result beyond the evaluation; self-play +159 elo vs trapRatio 1 (1.5: +38, 2: +45, 3 vs 2: +71,
    /// 4.5 vs 3: no gain).
    let duel = { duelPrev with TrapRatio = 3.0 }

    /// Tuned by `Arena tune --mode standard --nodes 5000` (SPSA, ~10k games); copy of tune/weights-standard.txt.
    /// Before 0.7.4, kept for the ladder A/B.
    let standardPrev =
        parse
            "area=2.616,oppArea=0.599,len=60.101,lenCap=5.895,trapped=1.078,starve=5854.651,noFoodHp=23.906,noFood=10.768,hungryHp=27.447,foodHungry=9.451,foodBehind=2.741,foodAhead=2.194,leadLen=3.089"

    /// trapped 1.08 -> 10: on 610 ladder games a snake with less territory than its length dies within 10 turns
    /// 4-20% of the time against 0.7% otherwise, yet the 5k-node SPSA had left the penalty near zero (+35 elo).
    let standard = { standardPrev with Trapped = 10.0 }

    /// The shipped weights for a game with this many snakes.
    let forSnakes (n: int) = if n = 2 then duel else standard
