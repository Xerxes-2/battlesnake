#!/usr/bin/env python3
"""Screen candidate evaluation features on ladder frames (dumped by `Arena features`).

For each target (dies within 10 turns, final placement) fit a logistic model on the current evaluation's inputs,
then add one candidate at a time and report how much the validation log-loss drops. A feature only matters for the
engine if it adds information the existing terms don't already carry.

usage: screen.py tune/texel/feat-standard.bin
"""

import sys

import numpy as np

path = sys.argv[1]
cols = open(path + ".cols").read().split(",")
X = np.fromfile(path, dtype=np.float32).reshape(-1, len(cols)).astype(np.float64)
c = {n: X[:, i] for i, n in enumerate(cols)}

valid = c["valid"] > 0
lenDiff = np.clip(c["len"] - c["oppLen"], -6, 6)
base = {
    "eval": c["eval"] / 1000,
    "area": c["area"],
    "oppArea": c["oppArea"],
    "lenDiff": lenDiff,
    "trapped": np.maximum(0, c["len"] - c["area"]),
    "hp": c["hp"],
    "nf": c["nf"],
    "others": c["others"],
}
L = c["len"]
cand = {
    "deg": c["deg"],
    "deg<=1": (c["deg"] <= 1) * 1.0,
    "headThreat": c["headThreat"],
    "oppHeadDist<=2": (c["oppHeadDist"] <= 2) * 1.0,
    "oppHeadDist": np.minimum(c["oppHeadDist"], 10),
    "solo": c["solo"],
    "solo<len": np.maximum(0, L - c["solo"]),
    "soloMinusArea": c["soloMinusArea"],
    "frontier": c["frontier"],
    "near3": c["near3"],
    "tailDist": np.minimum(c["tailDist"], 30),
    "tailUnreach": (c["tailDist"] >= 99) * 1.0,
    "wallDist": c["wallDist"],
    "contested": c["contested"],
    "minArea": c["minArea2"],
    "log area": np.log1p(c["area"]),
    "area/len": np.minimum(c["area"] / L, 5),
    "area<2len": np.maximum(0, 2 * L - c["area"]),
    "turn": c["turn"] / 100,
}


def fit(F, y, w=None, iters=25):
    """Logistic regression by Newton's method (labels may be fractional); returns weights, with the bias first."""
    F = np.column_stack([np.ones(len(F))] + [F])
    mu, sd = F[:, 1:].mean(0), F[:, 1:].std(0) + 1e-9
    F[:, 1:] = (F[:, 1:] - mu) / sd
    b = np.zeros(F.shape[1])
    for _ in range(iters):
        p = 1 / (1 + np.exp(-np.clip(F @ b, -30, 30)))
        g = F.T @ (p - y)
        H = (F * (p * (1 - p))[:, None]).T @ F + 1e-6 * np.eye(len(b))
        b -= np.linalg.solve(H, g)
    return b, mu, sd


def logloss(F, y, model):
    b, mu, sd = model
    F = np.column_stack([np.ones(len(F))] + [(F - mu) / sd])
    p = np.clip(1 / (1 + np.exp(-F @ b)), 1e-9, 1 - 1e-9)
    return -np.mean(y * np.log(p) + (1 - y) * np.log(1 - p))


def screen(target, mask=None):
    y = c[target]
    m = np.ones(len(y), bool) if mask is None else mask
    tr, va = m & ~valid, m & valid
    print(f"\n== target {target}  (train {tr.sum()}, valid {va.sum()}, mean {y[m].mean():.3f})")
    for name, feats in [("eval only", ["eval"]), ("all base", list(base))]:
        B = np.column_stack([base[f] for f in feats])
        ref = logloss(B[va], y[va], fit(B[tr], y[tr]))
        print(f"  baseline {name:10s} valid logloss {ref:.5f}")
        gains = []
        for n, v in cand.items():
            F = np.column_stack([B, v])
            model = fit(F[tr], y[tr])
            gains.append((ref - logloss(F[va], y[va], model), n, model[0][-1]))
        for g, n, coef in sorted(gains, reverse=True)[:8]:
            print(f"    {n:16s} gain {g * 1000:7.2f} mlogloss   coef {coef:+.3f}")


screen("die10")
screen("place")
screen("die10", c["me"] > 0)
