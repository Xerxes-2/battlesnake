#!/usr/bin/env python3
"""Report on parsed ladder games: win rate, death causes, placements, timeouts.

Reads the pickle from ladder_parse.py and fetches game metadata from the arena
API (cached, so reruns are free). The engine host engine.battlesnake.com 404s
for games played after 2026-11-09; the arena host serves both old and new ones.

usage: ladder_report.py games.pkl [--timeouts]

Each extra game costs ~1 request for meta and ~2 for --timeouts.
"""

import collections
import concurrent.futures as cf
import json
import os
import sys
import urllib.request

API = 'https://arena.battlesnake.com/api/games/'
ME = 'Xerxes-2'
CACHE = os.path.join(os.path.dirname(__file__), '..', '.cache', 'ladder')
# the old engine and the new arena spell some causes differently
ALIAS = {'self-collision': 'snake-self-collision'}


def get(gid, what=''):
    path = os.path.join(CACHE, gid + what + '.json')
    if os.path.exists(path):
        return json.load(open(path))
    try:
        with urllib.request.urlopen(f'{API}{gid}{what}', timeout=30) as r:
            d = json.load(r)
    except Exception:
        d = None  # games older than the archive are gone for good
    os.makedirs(CACHE, exist_ok=True)
    json.dump(d, open(path, 'w'))
    return d


def main():
    import pickle
    games = pickle.load(open(sys.argv[1], 'rb'))
    timeouts = '--timeouts' in sys.argv
    ids = [k for k, g in games.items() if g['res']]
    with cf.ThreadPoolExecutor(16) as ex:
        metas = dict(zip(ids, ex.map(get, ids)))
        if timeouts:
            frames = dict(zip(ids, ex.map(lambda g: get(g, '/frames?offset=0&limit=1000'), ids)))
        else:
            frames = {}

    byday = collections.defaultdict(lambda: [0, 0])
    cause = collections.Counter()
    place = collections.Counter()
    late = collections.Counter()
    for gid, g in games.items():
        if not g['res'] or g['rs'] != 'standard':
            continue
        key = (g['ts'][:10], g['n'], g['arm'])
        byday[key][0] += g['res'] != 'LOSS'
        byday[key][1] += g['res'] == 'LOSS'
        d = metas.get(gid)
        if not d:
            continue
        snakes = d['LastFrame']['Snakes']
        me = [s for s in snakes if s.get('Author') == ME]
        if not me:
            continue
        death = me[0].get('Death') or {}
        if death:
            cause[(g['n'], ALIAS.get(death.get('Cause'), death.get('Cause')))] += 1
        elif g['n'] == 4:  # survived: placement is by length then survival
            place['alive'] += 1
        for s in (frames.get(gid) or {}).get('Frames', []):
            for sn in s['Snakes']:
                if sn.get('Author') == ME and sn.get('Error'):
                    late[gid[:8]] += 1

    print('== win rate (day, players, arm)')
    for k, (w, l) in sorted(byday.items()):
        print(f'{k[0]} {k[1]}p {k[2]:6s} W{w} L{l} {w / (w + l):.1%}')
    print('\n== death cause (players, cause)')
    for k, v in cause.most_common(12):
        print(f'{k[1]:18s} {k[0]}p {v}')
    if place:
        print('\n== 4p survived to the end:', place['alive'])
    if timeouts:
        print('\n== timeouts (game, count)')
        for k, v in late.most_common(10):
            print(f'{k} {v}')
        print('games with a timeout:', len(late), f'/ {len(ids)}')


if __name__ == '__main__':
    main()
