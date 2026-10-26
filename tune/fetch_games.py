#!/usr/bin/env python3
"""Download ladder games from the Battlesnake engine API for Texel tuning.

Usage: fetch_games.py IDS_FILE OUT_DIR
Writes OUT_DIR/<id>.json = {"id", "w", "h", "frames": [{"t", "food", "haz", "snakes": [{"a", "hp", "body", "dt"}]}]}
where "a" is the author, "body" a flat [x0,y0,x1,y1,...] list and "dt" the death turn (absent if alive at the end).
Non-ladder games are recorded as an empty marker so they are not fetched again.
"""
import json, os, sys, urllib.request, concurrent.futures as cf

API = "https://engine.battlesnake.com/games/"

def get(url):
    for _ in range(4):
        try:
            with urllib.request.urlopen(url, timeout=30) as r:
                return json.load(r)
        except Exception:
            pass
    raise RuntimeError(url)

def fetch(gid, out):
    path = os.path.join(out, gid + ".json")
    if os.path.exists(path):
        return "cached"
    g = get(API + gid)
    if g["Game"].get("Source") != "ladder":
        open(path, "w").write("{}")
        return "skip"
    last = g["LastFrame"]["Turn"]
    deaths = {s["ID"]: s["Death"]["Turn"] for s in g["LastFrame"]["Snakes"] if s.get("Death")}
    frames, off = [], 0
    while off <= last:
        page = get(f"{API}{gid}/frames?offset={off}&limit=100")["Frames"]
        if not page:
            break
        for f in page:
            snakes = []
            for s in f["Snakes"]:
                if s.get("Death"):
                    continue
                body = [v for c in s["Body"] for v in (c["X"], c["Y"])]
                e = {"a": s["Author"], "id": s["ID"], "hp": s["Health"], "body": body}
                if s["ID"] in deaths:
                    e["dt"] = deaths[s["ID"]]
                snakes.append(e)
            frames.append({"t": f["Turn"], "food": [v for c in f["Food"] for v in (c["X"], c["Y"])],
                           "haz": [v for c in (f.get("Hazards") or []) for v in (c["X"], c["Y"])], "snakes": snakes})
        off += len(page)
    doc = {"id": gid, "w": g["Game"]["Width"], "h": g["Game"]["Height"], "ruleset": g["Game"]["Ruleset"]["name"],
           "n": len(g["LastFrame"]["Snakes"]), "last": last, "frames": frames}
    tmp = path + ".tmp"
    with open(tmp, "w") as fh:
        json.dump(doc, fh, separators=(",", ":"))
    os.replace(tmp, path)
    return "ok"

if __name__ == "__main__":
    ids = [l.strip() for l in open(sys.argv[1]) if l.strip()]
    out = sys.argv[2]
    os.makedirs(out, exist_ok=True)
    stats = {}
    with cf.ThreadPoolExecutor(int(os.environ.get("THREADS", "8"))) as ex:
        def safe(g):
            try:
                return fetch(g, out)
            except Exception:
                return "error"

        for i, r in enumerate(ex.map(safe, ids)):
            stats[r] = stats.get(r, 0) + 1
            if i % 100 == 0:
                print(i, stats, flush=True)
    print("done", stats)
