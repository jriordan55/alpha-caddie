#!/usr/bin/env python3
"""
Leakage-safe OOS: predict Birdies / Score / GIR / Fairways from strokes-gained
components only (OTT/APP/ARG/PUTT/TOTAL). Nested rolling-window selection.
No book blend. Does not change live projections.

  python scripts/sg-markets-oos-roi.py
  → data/sg_markets_oos_roi.json
"""
from __future__ import annotations

import importlib.util
import json
import time
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.linear_model import BayesianRidge
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

WEB = Path(__file__).resolve().parents[1]
REPO = WEB.parent
HIST = REPO / "data" / "historical_rounds_all.csv"
BASE_SCRIPT = WEB / "scripts" / "ml-oos-roi.py"
OUT = WEB / "data" / "sg_markets_oos_roi.json"

WINDOWS = (12, 24, 36, 48)
SG_COLS = ("sg_total", "sg_ott", "sg_app", "sg_arg", "sg_putt")
MARKET_Y = {
    "Birdies": "birdies",
    "Total score": "round_score",
    "GIR": "gir_count",
    "Fairways hit": "fw_count",
}


def load_base():
    spec = importlib.util.spec_from_file_location("ml_oos_base", BASE_SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader
    spec.loader.exec_module(mod)
    return mod


def load_history() -> pd.DataFrame:
    cols = [
        "year",
        "event_name",
        "event_completed",
        "dg_id",
        "round_num",
        "course_name",
        "round_score",
        "birdies",
        "gir",
        "driving_acc",
        "sg_total",
        "sg_ott",
        "sg_app",
        "sg_arg",
        "sg_putt",
    ]
    h = pd.read_csv(HIST, usecols=cols, low_memory=False)
    for c in cols:
        if c in ("event_name", "event_completed", "course_name"):
            continue
        h[c] = pd.to_numeric(h[c], errors="coerce")
    h = h[h["dg_id"].notna() & (h["year"] >= 2023)].copy()
    h["completed"] = pd.to_datetime(h["event_completed"], errors="coerce")
    h = h[h["completed"].notna()].copy()
    h["completed_ms"] = (h["completed"].astype("int64") // 10**6).astype(np.int64)
    h["gir_count"] = h["gir"] * 18.0
    h["fw_count"] = h["driving_acc"] * 14.0
    h = h.sort_values(["dg_id", "completed_ms", "round_num"]).reset_index(drop=True)
    h["n_prior"] = h.groupby("dg_id").cumcount()
    g = h.groupby("dg_id", sort=False)
    for window in WINDOWS:
        min_p = max(4, window // 4)
        for col in SG_COLS:
            h[f"w{window}_{col}"] = (
                g[col]
                .shift(1)
                .rolling(window, min_periods=min_p)
                .mean()
                .reset_index(level=0, drop=True)
            )
        # SG volatility (form noise)
        h[f"w{window}_sg_total_std"] = (
            g["sg_total"]
            .shift(1)
            .rolling(window, min_periods=min_p)
            .std()
            .reset_index(level=0, drop=True)
        )
    return h


def feature_cols(window: int) -> list[str]:
    return [
        "round_num",
        "n_prior",
        *[f"w{window}_{c}" for c in SG_COLS],
        f"w{window}_sg_total_std",
        # structural interactions matching how live counts are built
        f"w{window}_sg_app",  # GIR / approach birdies (duplicated ok; kept via unique below)
    ]


def unique_features(window: int) -> list[str]:
    cols = [
        "round_num",
        "n_prior",
        f"w{window}_sg_total",
        f"w{window}_sg_ott",
        f"w{window}_sg_app",
        f"w{window}_sg_arg",
        f"w{window}_sg_putt",
        f"w{window}_sg_total_std",
        # approach+putt birdie proxy, ott fairway proxy (explicit composites)
        f"w{window}_sg_app_plus_putt",
        f"w{window}_sg_ott_minus_app",
    ]
    return cols


def add_composites(df: pd.DataFrame, window: int) -> None:
    df[f"w{window}_sg_app_plus_putt"] = df[f"w{window}_sg_app"] + df[f"w{window}_sg_putt"]
    df[f"w{window}_sg_ott_minus_app"] = df[f"w{window}_sg_ott"] - df[f"w{window}_sg_app"]


def matrix(train: pd.DataFrame, test: pd.DataFrame, cols: list[str]):
    a = train[cols].to_numpy(dtype=float)
    b = test[cols].to_numpy(dtype=float)
    med = np.nanmedian(a, axis=0)
    med = np.where(np.isfinite(med), med, 0.0)
    return np.where(np.isfinite(a), a, med), np.where(np.isfinite(b), b, med)


def model():
    return Pipeline(
        [
            ("scale", StandardScaler()),
            ("bayes", BayesianRidge(compute_score=True)),
        ]
    )


def choose_window(train: pd.DataFrame, market: str) -> tuple[int, dict]:
    ycol = MARKET_Y[market]
    dates = np.sort(train["completed_ms"].dropna().unique())
    if len(dates) < 20:
        return 36, {}
    split = dates[max(1, int(len(dates) * 0.8)) - 1]
    fit = train[train["completed_ms"] < split].copy()
    val = train[train["completed_ms"] >= split].copy()
    scores = {}
    for window in WINDOWS:
        add_composites(fit, window)
        add_composites(val, window)
        cols = unique_features(window)
        needed = f"w{window}_sg_total"
        f = fit[fit[needed].notna() & fit[ycol].notna()]
        v = val[val[needed].notna() & val[ycol].notna()]
        if len(f) < 800 or len(v) < 150:
            scores[str(window)] = None
            continue
        if len(f) > 35000:
            f = f.sample(35000, random_state=window)
        x_fit, x_val = matrix(f, v, cols)
        m = model()
        m.fit(x_fit, f[ycol].to_numpy(float))
        pred = m.predict(x_val)
        scores[str(window)] = round(float(np.mean(np.abs(pred - v[ycol].to_numpy(float)))), 4)
    valid = {int(k): v for k, v in scores.items() if v is not None}
    if not valid:
        return 36, scores
    return min(valid, key=valid.get), scores


def main():
    t0 = time.time()
    base = load_base()
    panel = base.load_bet_panel()
    events = (
        panel[["event_name", "event_order", "cutoff_ms"]]
        .drop_duplicates()
        .sort_values("event_order")
    )
    print("[sg-oos] loading history…", flush=True)
    hist = load_history()
    print(f"[sg-oos] hist={len(hist)} events={len(events)}", flush=True)

    frames = []
    tuning = {}
    for market in MARKET_Y:
        print(f"\n[sg-oos] {market}", flush=True)
        ycol = MARKET_Y[market]
        mpanel = panel[panel["market"] == market].reset_index(drop=True)
        pred = np.full(len(mpanel), np.nan)
        chosen_w = np.full(len(mpanel), np.nan)

        for _, ev in events.iterrows():
            event_name = ev["event_name"]
            cutoff = int(ev["cutoff_ms"])
            mask = (mpanel["event_name"] == event_name).to_numpy()
            if not mask.any():
                continue
            outer = hist[
                (hist["completed_ms"] < cutoff)
                & hist[ycol].notna()
                & (hist["n_prior"] >= 8)
            ]
            window, scores = choose_window(outer, market)
            tuning[f"{market}|{event_name}"] = {"chosen_window": window, "inner_mae": scores}
            add_composites(outer, window)
            cols = unique_features(window)
            needed = f"w{window}_sg_total"
            fit = outer[outer[needed].notna()]
            if len(fit) > 40000:
                fit = fit.sample(40000, random_state=window)
            if len(fit) < 800:
                print(f"  skip {event_name}: train={len(fit)}", flush=True)
                continue

            # player features: latest pre-cutoff SG rolling stats
            prior = hist[(hist["completed_ms"] < cutoff)].copy()
            add_composites(prior, window)
            last = prior.groupby("dg_id", as_index=False).tail(1).set_index("dg_id")

            test_rows = mpanel.loc[mask]
            gpos = np.flatnonzero(mask)
            X_list = []
            keep = []
            for local_i, (_, row) in enumerate(test_rows.iterrows()):
                dg = int(row["dg_id"])
                if dg not in last.index:
                    continue
                fr = last.loc[dg]
                if isinstance(fr, pd.DataFrame):
                    fr = fr.iloc[-1]
                d = {c: fr[c] if c in fr.index else np.nan for c in cols}
                d["round_num"] = float(row["round"])
                X_list.append(d)
                keep.append(gpos[local_i])
            if not X_list:
                continue
            test = pd.DataFrame(X_list)
            x_fit, x_test = matrix(fit, test, cols)
            m = model()
            m.fit(x_fit, fit[ycol].to_numpy(float))
            values = m.predict(x_test)
            if market == "Birdies":
                values = np.clip(values, 0.05, None)
            if market in ("GIR", "Fairways hit"):
                values = np.clip(values, 0.5, None)
            for i, pos in enumerate(keep):
                pred[pos] = float(values[i])
                chosen_w[pos] = window
            print(f"  {event_name}: window={window} test={len(keep)}", flush=True)

        chunk = mpanel.copy()
        chunk["pred_sg"] = pred
        chunk["selected_window"] = chosen_w
        frames.append(chunk)

    full = pd.concat(frames, ignore_index=True)
    rec = base.score_predictions(full, "pred_sg", book_blend=0.0)
    payload = {
        "generated_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "hypothetical": True,
        "note": (
            "BayesianRidge maps rolling SG:OTT/APP/ARG/PUTT/TOTAL → each market. "
            "Nested window by MAE. No book blend. Live already uses SG structurally "
            "via counting-from-rates-sg; this is a pure SG→market OOS check."
        ),
        "methodology": {
            "features": "rolling SG components + composites (app+putt, ott-app) + round + n_prior",
            "candidate_windows": list(WINDOWS),
            "window_selection": "inner chronological 80/20 MAE",
            "book_blend": 0.0,
            "oos_event_count": int(len(events)),
            "events": events["event_name"].tolist(),
            "excluded_live_event": base.load_live_event(),
            "reference_baseline": "WF day+form skill36 ~6.8% ROI",
            "elapsed_sec": round(time.time() - t0, 1),
        },
        "window_tuning": tuning,
        "strategies": [
            {
                "id": "sg_components_bayesian_ridge_pure",
                "name": "SG components → markets (BayesianRidge, nested window)",
                "recommended": rec,
                "model_vs_actual": base.mae_bias(full, "pred_sg"),
            }
        ],
    }
    OUT.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    print("\n=== SG markets OOS ===", flush=True)
    print(
        f"ROI={rec['roi_pct']}% PnL={rec['units']}u bets={rec['bets']} hit={rec['hit_pct']}%",
        flush=True,
    )
    for m, s in (rec.get("by_market") or {}).items():
        print(f"  {m}: {s['units']}u / {s['bets']} bets", flush=True)
    print(f"Wrote {OUT}", flush=True)


if __name__ == "__main__":
    main()
