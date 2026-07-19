#!/usr/bin/env python3
"""
Leakage-safe Birdies OOS backtest: rolling BoB% ± course calibration.

BoB count = birdies + eagles_or_better
BoB% = BoB / 18
Base μ = 18 × rolling player BoB%

Course calibration (mirrors counting-from-rates-sg birdie spread/field mean):
  1) course_anchor  = prior mean BoB at course_used (pre-cutoff)
  2) spread_keep    = course + k*(player − course)   with k≈0.42
  3) field_calibrate = shift event-round field so mean(μ)=course_anchor

  python scripts/bob-pct-birdies-oos.py
  → data/bob_pct_birdies_oos_roi.json
"""
from __future__ import annotations

import importlib.util
import json
import re
import time
from pathlib import Path

import numpy as np
import pandas as pd

WEB = Path(__file__).resolve().parents[1]
REPO = WEB.parent
HIST = REPO / "data" / "historical_rounds_all.csv"
BASE_SCRIPT = WEB / "scripts" / "ml-oos-roi.py"
OUT = WEB / "data" / "bob_pct_birdies_oos_roi.json"

WINDOWS = (8, 12, 20, 36, 50)
HOLES = 18.0
BOOK_BLENDS = (0.0, 0.25, 0.35, 0.5)
# Same default as BIRDIE_COURSE_SPREAD_KEEP in counting-from-rates-sg.mjs
SPREAD_KEEP = 0.42
COURSE_MIN_ROUNDS = 40
PLAYER_COURSE_MIN = 4


def load_base():
    spec = importlib.util.spec_from_file_location("ml_oos_base", BASE_SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader
    spec.loader.exec_module(mod)
    return mod


def course_key(value: object) -> str:
    s = re.sub(r"[^a-z0-9]+", " ", str(value or "").lower())
    return " ".join(s.split())


def load_history() -> pd.DataFrame:
    cols = [
        "year",
        "event_name",
        "event_completed",
        "dg_id",
        "round_num",
        "course_name",
        "birdies",
        "eagles_or_better",
    ]
    h = pd.read_csv(HIST, usecols=cols, low_memory=False)
    for c in ("year", "dg_id", "round_num", "birdies", "eagles_or_better"):
        h[c] = pd.to_numeric(h[c], errors="coerce")
    h = h[h["dg_id"].notna() & (h["year"] >= 2023)].copy()
    h["completed"] = pd.to_datetime(h["event_completed"], errors="coerce")
    h = h[h["completed"].notna()].copy()
    h["completed_ms"] = (h["completed"].astype("int64") // 10**6).astype(np.int64)
    eob = h["eagles_or_better"].fillna(0.0).clip(lower=0.0)
    h["bob_count"] = h["birdies"] + eob
    h = h[h["birdies"].notna()].copy()
    h["bob_pct"] = h["bob_count"] / HOLES
    h["course_key"] = h["course_name"].map(course_key)
    h = h.sort_values(["dg_id", "completed_ms", "round_num"]).reset_index(drop=True)
    h["n_prior"] = h.groupby("dg_id").cumcount()

    g = h.groupby("dg_id", sort=False)
    for w in WINDOWS:
        min_p = max(3, w // 4)
        h[f"w{w}_bob_pct"] = (
            g["bob_pct"].shift(1).rolling(w, min_periods=min_p).mean().reset_index(level=0, drop=True)
        )
        h[f"w{w}_bob_mu"] = h[f"w{w}_bob_pct"] * HOLES
    return h


def choose_window(train: pd.DataFrame) -> tuple[int, dict]:
    dates = np.sort(train["completed_ms"].dropna().unique())
    if len(dates) < 20:
        return 20, {str(w): None for w in WINDOWS}
    split = dates[max(1, int(len(dates) * 0.8)) - 1]
    fit = train[train["completed_ms"] < split]
    val = train[train["completed_ms"] >= split]
    scores = {}
    for w in WINDOWS:
        col = f"w{w}_bob_mu"
        f = fit[fit[col].notna() & fit["bob_count"].notna()]
        v = val[val[col].notna() & val["bob_count"].notna()]
        if len(f) < 400 or len(v) < 80:
            scores[str(w)] = None
            continue
        scores[str(w)] = round(
            float(np.mean(np.abs(v[col].to_numpy(float) - v["bob_count"].to_numpy(float)))), 4
        )
    valid = {int(k): v for k, v in scores.items() if v is not None}
    if not valid:
        return 20, scores
    return min(valid, key=valid.get), scores


def player_bob_mu(history: pd.DataFrame, dg_ids, cutoff_ms: int, window: int) -> pd.Series:
    col = f"w{window}_bob_mu"
    sub = history[(history["completed_ms"] < cutoff_ms) & history["dg_id"].isin(dg_ids)]
    if sub.empty:
        return pd.Series(dtype=float)
    last = sub.groupby("dg_id", as_index=False).tail(1)
    return last.set_index("dg_id")[col]


def course_anchor_mu(history: pd.DataFrame, ck: str, cutoff_ms: int) -> tuple[float, int]:
    """Prior mean BoB count at course (all players), leakage-safe."""
    if not ck:
        return np.nan, 0
    sub = history[
        (history["completed_ms"] < cutoff_ms)
        & (history["course_key"] == ck)
        & history["bob_count"].notna()
    ]
    n = len(sub)
    if n < COURSE_MIN_ROUNDS:
        return np.nan, n
    return float(sub["bob_count"].mean()), n


def tour_anchor_mu(history: pd.DataFrame, cutoff_ms: int) -> float:
    sub = history[(history["completed_ms"] < cutoff_ms) & history["bob_count"].notna()]
    if len(sub) < 200:
        return 3.5
    # Prefer recent prior mass without current event
    recent = sub.tail(min(len(sub), 8000))
    return float(recent["bob_count"].mean())


def player_course_mu(
    history: pd.DataFrame, dg_id: float, ck: str, cutoff_ms: int
) -> tuple[float, int]:
    if not ck or not np.isfinite(dg_id):
        return np.nan, 0
    sub = history[
        (history["dg_id"] == dg_id)
        & (history["completed_ms"] < cutoff_ms)
        & (history["course_key"] == ck)
        & history["bob_count"].notna()
    ]
    n = len(sub)
    if n < PLAYER_COURSE_MIN:
        return np.nan, n
    return float(sub["bob_count"].mean()), n


def birdie_spread_keep(player_mu: float, course_mu: float, base: float = SPREAD_KEEP) -> float:
    """Mirror birdieSpreadKeepForPlayer: keep more spread for above-course players."""
    spread = base
    if np.isfinite(player_mu) and np.isfinite(course_mu) and player_mu > course_mu:
        excess = player_mu - course_mu
        spread = min(0.9, max(spread, spread + 0.4 * excess + 0.04 * max(0.0, player_mu)))
    return spread


def apply_course_spread(player_mu: float, course_mu: float, tour_mu: float) -> float:
    """μ = anchor + k*(player − anchor). Prefer course; fall back to tour."""
    anchor = course_mu if np.isfinite(course_mu) else tour_mu
    if not np.isfinite(player_mu):
        return anchor if np.isfinite(anchor) else np.nan
    if not np.isfinite(anchor):
        return player_mu
    k = birdie_spread_keep(player_mu, anchor)
    return anchor + k * (player_mu - anchor)


def apply_player_course_blend(
    player_mu: float, course_mu: float, tour_mu: float, pc_mu: float, pc_n: int
) -> float:
    """Spread-keep toward course, then blend in player-at-course history."""
    base = apply_course_spread(player_mu, course_mu, tour_mu)
    if not np.isfinite(pc_mu) or pc_n < PLAYER_COURSE_MIN:
        return base
    w = min(0.45, pc_n / (pc_n + 10.0))
    if not np.isfinite(base):
        return pc_mu
    return (1.0 - w) * base + w * pc_mu


def field_calibrate(values: np.ndarray, target: float) -> np.ndarray:
    """Shift finite values so their mean equals course/venue target (spread preserved)."""
    out = values.copy()
    m = np.isfinite(out)
    if m.sum() < 8 or not np.isfinite(target):
        return out
    gap = target - float(out[m].mean())
    if abs(gap) < 0.04:
        return out
    out[m] = np.clip(out[m] + gap, 0.15, 7.0)
    return out


def mae_bias_series(actual: np.ndarray, pred: np.ndarray) -> dict | None:
    m = np.isfinite(actual) & np.isfinite(pred)
    if m.sum() < 5:
        return None
    err = pred[m] - actual[m]
    return {
        "n": int(m.sum()),
        "bias": round(float(err.mean()), 3),
        "mae": round(float(np.abs(err).mean()), 3),
        "rmse": round(float(np.sqrt(np.mean(err ** 2))), 3),
        "corr": round(float(np.corrcoef(pred[m], actual[m])[0, 1]), 3) if m.sum() > 2 else None,
    }


def strategy_row(base, full, col, sid, predictor, window, book_blend=0.0, with_mae=True):
    return {
        "id": sid,
        "predictor": predictor,
        "window": window,
        "book_blend": book_blend,
        "recommended": base.score_predictions(full, col, book_blend=book_blend),
        "model_vs_actual": (
            {"Birdies": mae_bias_series(full["actual"].to_numpy(float), full[col].to_numpy(float))}
            if with_mae and book_blend == 0.0
            else None
        ),
    }


def main():
    started = time.time()
    base = load_base()
    panel_all = base.load_bet_panel()
    panel = panel_all[panel_all["market"] == "Birdies"].reset_index(drop=True)
    if "course_used" not in panel.columns:
        # reload course_used from vs-actual (load_bet_panel keeps it if present)
        vs = pd.read_csv(WEB / "data" / "round_projection_vs_actual.csv", low_memory=False)
        key_cols = ["event_name", "dg_id", "round"]
        vs["dg_id"] = pd.to_numeric(vs["dg_id"], errors="coerce")
        vs["round"] = pd.to_numeric(vs["round"], errors="coerce")
        merge = vs[key_cols + ["course_used"]].drop_duplicates(key_cols)
        panel = panel.merge(merge, on=key_cols, how="left")
    panel["course_key"] = panel["course_used"].map(course_key)

    events = (
        panel[["event_name", "event_order", "cutoff_ms"]]
        .drop_duplicates()
        .sort_values("event_order")
    )
    print(f"[bob-pct] Birdies OOS rows={len(panel)} events={len(events)}", flush=True)
    print("          " + " | ".join(events["event_name"].tolist()), flush=True)

    history = load_history()
    print(f"[bob-pct] hist rounds={len(history)} players={history['dg_id'].nunique()}", flush=True)

    # Prediction columns
    pred_keys = (
        [f"w{w}" for w in WINDOWS]
        + ["tuned", "tuned_spread", "tuned_field", "tuned_spread_field", "tuned_pc_spread_field"]
    )
    preds = {k: np.full(len(panel), np.nan) for k in pred_keys}
    selected_window = np.full(len(panel), np.nan)
    tuning = {}
    course_meta = {}

    for _, ev in events.iterrows():
        event_name = ev["event_name"]
        cutoff = int(ev["cutoff_ms"])
        mask = (panel["event_name"] == event_name).to_numpy()
        if not mask.any():
            continue
        train = history[
            (history["completed_ms"] < cutoff)
            & history["bob_count"].notna()
            & (history["n_prior"] >= 4)
        ]
        window, scores = choose_window(train)

        # Course from panel (usually one course per event in this export)
        event_courses = panel.loc[mask, "course_key"].dropna().astype(str)
        ck = event_courses.mode().iloc[0] if len(event_courses) else ""
        c_mu, c_n = course_anchor_mu(history, ck, cutoff)
        t_mu = tour_anchor_mu(history, cutoff)
        anchor = c_mu if np.isfinite(c_mu) else t_mu
        tuning[event_name] = {
            "chosen_window": window,
            "inner_mae": scores,
            "course_key": ck,
            "course_anchor_mu": None if not np.isfinite(c_mu) else round(c_mu, 3),
            "course_anchor_n": c_n,
            "tour_anchor_mu": round(t_mu, 3),
            "calibration_target": round(float(anchor), 3) if np.isfinite(anchor) else None,
        }
        course_meta[event_name] = tuning[event_name]

        dg_ids = panel.loc[mask, "dg_id"].unique()
        for w in WINDOWS:
            mu_map = player_bob_mu(history, dg_ids, cutoff, w)
            for i in np.flatnonzero(mask):
                dg = panel.at[i, "dg_id"]
                if dg in mu_map.index and np.isfinite(mu_map.loc[dg]):
                    val = max(0.05, float(mu_map.loc[dg]))
                    preds[f"w{w}"][i] = val
                    if w == window:
                        preds["tuned"][i] = val
                        selected_window[i] = window

        # Course-calibrated variants on nested-window player μ
        idxs = np.flatnonzero(mask)
        raw = preds["tuned"][idxs].copy()
        spread = np.full(len(idxs), np.nan)
        pc_spread = np.full(len(idxs), np.nan)
        for j, i in enumerate(idxs):
            pmu = raw[j]
            if not np.isfinite(pmu):
                continue
            row_ck = panel.at[i, "course_key"] or ck
            row_c_mu, _ = course_anchor_mu(history, row_ck, cutoff) if row_ck != ck else (c_mu, c_n)
            spread[j] = max(0.05, apply_course_spread(pmu, row_c_mu, t_mu))
            pc_mu, pc_n = player_course_mu(history, panel.at[i, "dg_id"], row_ck, cutoff)
            pc_spread[j] = max(
                0.05, apply_player_course_blend(pmu, row_c_mu, t_mu, pc_mu, pc_n)
            )

        field_raw = field_calibrate(raw, anchor)
        field_spread = field_calibrate(spread, anchor)
        field_pc = field_calibrate(pc_spread, anchor)

        for j, i in enumerate(idxs):
            preds["tuned_spread"][i] = spread[j]
            preds["tuned_field"][i] = field_raw[j]
            preds["tuned_spread_field"][i] = field_spread[j]
            preds["tuned_pc_spread_field"][i] = field_pc[j]

        covered = int(np.isfinite(preds["tuned"][mask]).sum())
        print(
            f"  {event_name}: window={window} course={ck or '?'} "
            f"anchor={anchor:.2f} (n={c_n}) covered={covered}/{int(mask.sum())}",
            flush=True,
        )

    full = panel.copy()
    for key, arr in preds.items():
        full[f"pred_{key}"] = arr
    full["selected_window"] = selected_window

    strategies = []
    for w in WINDOWS:
        strategies.append(
            strategy_row(
                base,
                full,
                f"pred_w{w}",
                f"bob_pct_l{w}_pure",
                f"rolling BoB% last {w} → μ=18×BoB%",
                w,
            )
        )

    strategies.append(
        strategy_row(
            base,
            full,
            "pred_tuned",
            "bob_pct_nested_pure",
            "nested-window rolling BoB% only",
            "nested",
        )
    )
    strategies.append(
        strategy_row(
            base,
            full,
            "pred_tuned_spread",
            "bob_pct_nested_course_spread",
            f"nested BoB% + course spread-keep (k≈{SPREAD_KEEP})",
            "nested",
        )
    )
    strategies.append(
        strategy_row(
            base,
            full,
            "pred_tuned_field",
            "bob_pct_nested_field_calibrate",
            "nested BoB% + field-mean shift to course prior BoB",
            "nested",
        )
    )
    strategies.append(
        strategy_row(
            base,
            full,
            "pred_tuned_spread_field",
            "bob_pct_nested_course_spread_field",
            "nested BoB% + course spread-keep + field-mean calibrate",
            "nested",
        )
    )
    strategies.append(
        strategy_row(
            base,
            full,
            "pred_tuned_pc_spread_field",
            "bob_pct_nested_player_course_spread_field",
            "nested BoB% + player@course blend + spread-keep + field calibrate",
            "nested",
        )
    )

    for blend in BOOK_BLENDS:
        if blend == 0.0:
            continue
        strategies.append(
            strategy_row(
                base,
                full,
                "pred_tuned_spread_field",
                f"bob_pct_course_cal_book_blend_{int(blend * 100)}",
                "course-calibrated BoB% blended with DK book (reference)",
                "nested",
                book_blend=blend,
                with_mae=False,
            )
        )

    full["pred_book"] = full["book_line"]
    strategies.append(
        strategy_row(
            base, full, "pred_book", "book_line_baseline", "DK book line as μ", None
        )
    )
    if "csv_model_line" in full.columns:
        full["pred_csv_model"] = full["csv_model_line"]
        strategies.append(
            strategy_row(
                base,
                full,
                "pred_csv_model",
                "live_model_csv_baseline",
                "captured birdies_line from projection export",
                None,
            )
        )

    payload = {
        "generated_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "hypothetical": True,
        "note": (
            "Birdies OOS: rolling BoB% with optional course calibration "
            "(spread-keep + field-mean to prior course BoB). Same DK panel/policy as ml-oos-roi. "
            "Does not change live projections."
        ),
        "methodology": {
            "market": "Birdies",
            "predictor": "rolling BoB% ± course calibration",
            "bob_definition": "birdies + eagles_or_better",
            "mu_formula_base": "μ = 18 × mean(prior BoB%)",
            "course_calibration": {
                "course_anchor": "mean prior BoB at course_used (min rounds "
                f"{COURSE_MIN_ROUNDS})",
                "spread_keep": SPREAD_KEEP,
                "spread_formula": "course + k*(player − course)",
                "field_calibrate": "shift field mean(μ) → course_anchor",
                "player_course_blend": f"up to 45% weight when ≥{PLAYER_COURSE_MIN} prior rounds at course",
            },
            "candidate_windows": list(WINDOWS),
            "window_selection": "inner chronological 80/20 MAE on BoB count inside each outer event",
            "excluded_features": ["sg_*", "gir", "fairways", "round_score", "weather", "tee wave"],
            "oos_bet_rows": int(len(panel)),
            "oos_event_count": int(len(events)),
            "events": events["event_name"].tolist(),
            "excluded_live_event": base.load_live_event(),
            "policy": base.POLICY.get("Birdies"),
            "hist_years": "2023+",
            "elapsed_sec": round(time.time() - started, 1),
        },
        "window_tuning": tuning,
        "course_meta": course_meta,
        "strategies": strategies,
    }
    OUT.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    print("\n=== BoB% ± course calibration Birdies OOS ===", flush=True)
    for s in strategies:
        r = s["recommended"]
        mva = (s.get("model_vs_actual") or {}).get("Birdies") or {}
        mae = mva.get("mae")
        bias = mva.get("bias")
        acc = f" MAE={mae} bias={bias}" if mae is not None else ""
        print(
            f"{s['id']}: ROI={r['roi_pct']}% PnL={r['units']}u bets={r['bets']}{acc}",
            flush=True,
        )
    print(f"Wrote {OUT}", flush=True)


if __name__ == "__main__":
    main()
