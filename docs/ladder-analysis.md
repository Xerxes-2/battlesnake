# 天梯分析

线上蛇的成绩只从天梯对局里看。自对弈（arena SPRT）能筛掉明显更差的改动，但**测不出天梯上的收益**：0.7.7 在 standard 自对弈 +38 elo，天梯 4 蛇反而略降。所以每个改动上线后都要这样复查一遍。

## 1. 抓日志

```sh
ssh <host> 'journalctl -u battlesnake --no-pager -o short-iso' > snake.log
```

- 必须用 `-o short-iso`，按天分组的统计靠行首时间戳。
- 服务器上的 journald 会轮转，太旧的日志（个位数天）会被删掉，想留长历史得先在服务器加保留上限或定期拉到本地。
- 服务重启（Nix 切换、OOM、systemd 超时）会打断日志，重启前的段和之后的分开统计。

## 2. 解析

```sh
python3 tune/ladder_parse.py snake.log games.pkl      # 每局 → dict(moves, res, arm, n, ...)
python3 tune/ladder_report.py games.pkl --timeouts   # 胜率 / 死因 / 超时
```

`ladder_report.py` 会去 `arena.battlesnake.com/api/games/` 拉对局元数据和帧，缓存在 `.cache/ladder/`，重跑免费。`--timeouts` 要多拉帧，慢一些。

日志行的格式（`src/Server/Program.fs`）：

```
start <id> <ruleset> <w>x<h> snakes=<n> arm=<legacy|new>
move <id> t=<turn> -> <dir> d=<depth>[R] score=<s> nodes=<n> (<ms>/<budget> ms, net~<net>, lat=<lat>)
end <id> turn=<t> <WIN/ALIVE|LOSS>
```

- `lat` 是引擎测到的**上一步**延迟；`lat >= SnakeTimeout` 基本就是超时。
- `d=0 score=0 nodes=0` 是只有一步可走、没搜索，不是 bug。
- `R` 后缀是 relaxed search 标记（`soundProofs` 且 `Score < -win/2` 时的放宽搜索）。
- `score` 接近 `±1e6` 是搜索已经证出必胜/必败，此时再谈估值没意义。

## 3. A/B 分组

`SNAKE_AB` 默认开，`Sessions.legacyArm` 用 game id 首位十六进制数决定 arm，所以同一个 id 永远分到同一组。

- `legacy` = 上一版权重（`Weights.duelPrev` / `standardPrev`），跟当前代码一起跑。
- `new` = 当前权重。

代码改动两组都有，只有权重不同。所以 **arm 差异 = 权重差异**，而和上一版的总体差异里混着代码改动和天梯环境变化（对手换版本、新蛇涌入），不能直接当成改动的收益。

判定用正态近似：`p = w/(w+l)`，两组差的标准差约 `sqrt(p(1-p)(1/n1+1/n2))`，1σ 以上才值得看，2σ 才有说服力。

## 4. 指标

- **胜率**：`end` 是 `LOSS` 算输，`WIN/ALIVE` 算赢（活到最后）。
- **死因分布**：从 last frame 的 `Death.Cause` 读。旧引擎叫 `self-collision`，新 arena 叫 `snake-self-collision`，脚本里做了合并。
- **被证出必败的距离**：第一次出现 `score < -5e5` 的回合距死亡还有几回合。0.7.6 时多数败局在死前 4 到 20 回合就已证出，说明局面在更早就输掉了——要往前找原因。
- **长度差**：`我的长度 - 最长对手长度`，按回合采样看分布。这是 4 蛇的主要弱点指标。
- **超时**：优先用新 arena 帧里的 `StatusCode`/`Error`（`A response was not received before the timeout`），这是唯一的第一手证据。旧数据只能靠"我们回的方向和帧里实际走的方向不一致"间接推断，会漏。
- **超时连续性**：按局看超时时长。串行 20 多回合的超时是断网，不是网络抖动，`Clock` 的余量和 `TimeoutGuard` 都救不了。

## 5. 坑

- `engine.battlesnake.com` 对 2026-11-09 之后的对局一律 404，分析脚本统一用 `arena.battlesnake.com/api`。
- 天梯只跑 `standard`、11x11。`Source=ladder` 是旧引擎（MaxTurns 7200），`Source=arena` 是新平台（MaxTurns 5000），两者规则阶段一样。
- 蛇名每局随机（`Snafe`、`Slytherin` 之类），认人要认 `Author`。
- 对局有保存期限，老 ID 会 404，所以统计要在局后尽快做。
- 一局 4 蛇的胜率天然低于 2 蛇（1/4 对 1/2 的生存概率），两列不能直接比。
