/// Data types for the Battlesnake API (https://docs.battlesnake.com/api), plus conversion to the engine's internal state.
/// Only records are used, so the default System.Text.Json (web defaults: camelCase, case-insensitive) is enough.
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

type RulesetSettings =
    { FoodSpawnChance: int
      MinimumFood: int
      HazardDamagePerTurn: int }

type Ruleset =
    { Name: string
      Version: string
      Settings: RulesetSettings }

type Game =
    { Id: string
      Ruleset: Ruleset
      Map: string
      Timeout: int
      Source: string }

type BoardDto =
    { Height: int
      Width: int
      Food: Coord[]
      Hazards: Coord[]
      Snakes: SnakeDto[] }

type GameState =
    { Game: Game
      Turn: int
      Board: BoardDto
      You: SnakeDto }

/// "0.4.0+87ebf06": Version from Directory.Build.props plus the commit. Nix passes the flake's short rev as
/// SourceRevisionId; local builds get the full git sha from the SDK, shortened here to match.
let version =
    let v =
        Assembly.GetEntryAssembly().GetCustomAttribute<AssemblyInformationalVersionAttribute>().InformationalVersion

    // Only a full 40-char sha gets shortened; "abc1234-dirty" from a dirty flake tree stays as is
    match v.IndexOf '+' with
    | i when i >= 0 && v.Length - i - 1 = 40 -> v.[.. i + 7]
    | _ -> v

let info =
    {| apiversion = "1"
       author = "xerxes2"
       color = "#7b2ff7"
       head = "default"
       tail = "default"
       version = version |}

let private geometries = ConcurrentDictionary<struct (int * int), Geometry>()

let geometry w h =
    geometries.GetOrAdd(struct (w, h), fun struct (w, h) -> Geometry(w, h))

/// Converts the request into the engine state, putting "you" at index 0. Returns (board, my index).
let toBoard (gs: GameState) : Board * int =
    let bd = gs.Board
    let geo = geometry bd.Width bd.Height
    let snakes = if isNull bd.Snakes then [||] else bd.Snakes
    let b = Board(geo, max 1 snakes.Length)
    b.Turn <- gs.Turn

    if not (obj.ReferenceEquals(gs.Game.Ruleset, null))
       && not (obj.ReferenceEquals(gs.Game.Ruleset.Settings, null)) then
        b.HazardDamage <- gs.Game.Ruleset.Settings.HazardDamagePerTurn

    let cell (p: Coord) = geo.Cell(p.X, p.Y)
    let add (s: SnakeDto) = b.AddSnake(s.Health, s.Body |> Array.map cell) |> ignore

    let you = snakes |> Array.tryFind (fun s -> s.Id = gs.You.Id)
    add (defaultArg you gs.You)

    for s in snakes do
        if s.Id <> gs.You.Id then add s

    if not (isNull bd.Food) then
        for f in bd.Food do
            b.SetFood(cell f, true)

    if not (isNull bd.Hazards) then
        for h in bd.Hazards do
            b.AddHazard(cell h)

    b, 0
