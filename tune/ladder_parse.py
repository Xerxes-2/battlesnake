#!/usr/bin/env python3
"""Parse a snake-server journal dump into a per-game pickle for ladder analysis.

Get the dump with (see docs/ladder-analysis.md):
    ssh <host> 'journalctl -u battlesnake --no-pager -o short-iso' > snake.log

Writes {id: {ts, rs, w, h, n, arm, moves[], turns, res}}, where each move is
{t, dir, d, R, s, ms, bud, lat}. `lat` is the engine-reported latency of our
previous reply, so lat >= timeout is a strong timeout signal.

usage: ladder_parse.py snake.log games.pkl
"""

import pickle
import re
import sys

TS = re.compile(r'^(\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d)')
START = re.compile(r'start (\S+) (\S+) (\d+)x(\d+) snakes=(\d+) arm=(\w+)')
MOVE = re.compile(
    r'move (\S+) t=(\d+) -> (\w+) d=(\d+)(R?) score=(\S+) nodes=(\d+)'
    r' \((\S+)/(\S+) ms, net~(\S+), lat=(\S*)\)'
)
END = re.compile(r'end (\S+) turn=(\d+) (\S+)')


def parse(path):
    games = {}
    ts = ''
    for line in open(path):
        line = line.strip()
        # journal output is prefixed only on the first line of a multi-line entry
        if t := TS.match(line):
            ts = t[1]
        if 'start ' in line and (m := START.search(line)):
            games[m[1]] = dict(rs=m[2], w=int(m[3]), h=int(m[4]), n=int(m[5]),
                               arm=m[6], moves=[], res=None, turns=None, ts=ts)
        elif (m := MOVE.search(line)) and m[1] in games:
            games[m[1]]['moves'].append(dict(
                t=int(m[2]), dir=m[3], d=int(m[4]), R=bool(m[5]), s=float(m[6]),
                ms=float(m[8]), bud=float(m[9]), lat=int(m[11] or 0)))
        elif (m := END.search(line)) and m[1] in games:
            games[m[1]]['res'] = m[3]
            games[m[1]]['turns'] = int(m[2])
    return games


def main_inline(games):
    """Sanity check: one line of win rate per (snakes, arm)."""
    tally = {}
    for g in games.values():
        if g['res'] is None:
            continue
        key = (g['n'], g['arm'])
        w, l = tally.get(key, (0, 0))
        tally[key] = (w + (g['res'] != 'LOSS'), l + (g['res'] == 'LOSS'))
    for key, (w, l) in sorted(tally.items()):
        print(f'{key[0]}p {key[1]:6s} W{w} L{l} {w / (w + l):.1%}')
    print(f'{len(games)} games, {sum(g["res"] is not None for g in games.values())} finished')


if __name__ == '__main__':
    g = parse(sys.argv[1])
    pickle.dump(g, open(sys.argv[2], 'wb'))
    main_inline(g)
