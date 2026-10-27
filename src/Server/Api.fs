/// Data types for the Battlesnake API (https://docs.battlesnake.com/api), plus conversion to the engine's internal state.
/// Only records are used, so the default System.Text.Json (web defaults: camelCase, case-insensitive) is enough.
/// Fields the engine may leave out are `option`, like serde's Option: missing or null both read as None.
module Snake.Server.Api

open System.Collections.Concurrent
open System.Reflection
open Snake.Engine

type Coord = { X: int; Y: int }

type SnakeDto =
    { Id: string
      Name: string
      Health: int
      Body: Coord[]
      Head: Coord
      Length: int
      Latency: string
      Shout: string
      Squad: string }

type RulesetSettings = { FoodSpawnChance: int; MinimumFood: int; HazardDamagePerTurn: int }

type Ruleset = { Name: string; Version: string; Settings: RulesetSettings option }

type Game =
    { Id: string
      Ruleset: Ruleset option
      Map: string
      Timeout: int
      Source: string }

type BoardDto =
    { Height: int
      Width: int
      Food: Coord[] option
      Hazards: Coord[] option
      Snakes: SnakeDto[] option }

type GameState = { Game: Game; Turn: int; Board: BoardDto; You: SnakeDto }

/// "0.4.0+87ebf06": Version from Directory.Build.props plus the commit. Nix passes the flake's short rev as
/// SourceRevisionId; local builds get the full git sha from the SDK, shortened here to match.
let version =
    let v =
        match typeof<Coord>.Assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>() with
        | NonNull a -> a.InformationalVersion
        | Null -> "unknown"

    // Only a full 40-char sha gets shortened; "abc1234-dirty" from a dirty flake tree stays as is
    match v.IndexOf '+' with
    | i when i >= 0 && v.Length - i - 1 = 40 -> v[.. i + 7]
    | _ -> v

let info =
    {| apiversion = "1"
       author = "xerxes2"
       color = "#7b2ff7"
       head = "fang"
       tail = "bolt"
       version = version |}

let private geometries = ConcurrentDictionary<struct (int * int), Geometry>()

let geometry w h = geometries.GetOrAdd(struct (w, h), fun struct (w, h) -> Geometry(w, h))

/// An array field the engine left out counts as empty.
let orEmpty (a: 'T[] option) = Option.defaultValue [||] a

let rulesetName (g: Game) = g.Ruleset |> Option.map _.Name |> Option.defaultValue "?"

/// Converts the request into the engine state, putting "you" at index 0. Returns (board, my index).
let toBoard (gs: GameState) : Board * int =
    let bd = gs.Board
    let geo = geometry bd.Width bd.Height
    let snakes = orEmpty bd.Snakes
    let b = Board(geo, max 1 snakes.Length)
    b.Turn <- gs.Turn

    match gs.Game.Ruleset with
    | Some { Settings = Some st } -> b.HazardDamage <- st.HazardDamagePerTurn
    | _ -> ()

    let cell (p: Coord) = geo.Cell(p.X, p.Y)
    let add (s: SnakeDto) = b.AddSnake(s.Health, s.Body |> Array.map cell) |> ignore

    let you = snakes |> Array.tryFind (fun s -> s.Id = gs.You.Id)
    add (defaultArg you gs.You)

    for s in snakes do
        if s.Id <> gs.You.Id then add s

    for f in orEmpty bd.Food do
        b.SetFood(cell f, true)

    for h in orEmpty bd.Hazards do
        b.AddHazard(cell h)

    b, 0
