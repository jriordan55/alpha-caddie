#!/usr/bin/env python3
"""Leakage-safe Bayesian Ridge OOS ROI with weather, tee wave, and tuned form window."""
from __future__ import annotations

import importlib.util
import json
import re
import time
from difflib import SequenceMatcher
from pathlib import Path

import numpy as np
import pandas as pd
from sklearn.linear_model import BayesianRidge
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

WEB = Path(__file__).resolve().parents[1]
REPO = WEB.parent
HIST = REPO / "data" / "historical_rounds_all.csv"
WEATHER = WEB / "data" / "historical_round_weather.json"
BASE_SCRIPT = WEB / "scripts" / "ml-oos-roi.py"
OUT = WEB / "data" / "bayesian_ridge_oos_roi.json"

WINDOWS = (8, 12, 20, 36, 50)
BOOK_BLENDS = tuple(round(x / 10, 1) for x in range(0, 9))
MARKET_Y = {
    "Birdies": "birdies",
    "Total score": "round_score",
    "GIR": "gir_count",
    "Fairways hit": "fw_count",
}
STAT_COLS = ("sg_total", "sg_ott", "sg_app", "sg_putt", "birdies", "round_score", "gir_count", "fw_count")


def load_base():
    spec = importlib.util.spec_from_file_location("ml_oos_base", BASE_SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    assert spec.loader
    spec.loader.exec_module(mod)
    return mod


def title_key(value: object) -> str:
    s = re.sub(r"[^a-z0-9]+", " ", str(value or "").lower())
    s = re.sub(r"\b(the|championship|presented|by|workday)\b", " ", s)
    return " ".join(s.split())


def wave_from_teetime(value: object) -> tuple[float, float, float]:
    s = str(value or "").strip().lower()
    hour = None
    m = re.search(r"(?:\s|t|^)(\d{1,2}):(\d{2})\s*(am|pm)?", s)
    if m:
        hour = int(m.group(1))
        ap = m.group(3)
        if ap == "pm" and hour < 12:
            hour += 12
        elif ap == "am" and hour == 12:
            hour = 0
    if hour is None:
        return 0.0, 0.0, 1.0
    if hour < 12:
        return 1.0, 0.0, 0.0
    return 0.0, 1.0, 0.0


def weather_frame() -> pd.DataFrame:
    raw = json.loads(WEATHER.read_text(encoding="utf-8")).get("byKey", {})
    rows = []
    for snap in raw.values():
        condition = str(snap.get("condition") or "").lower()
        rows.append(
            {
                "event_id": pd.to_numeric(snap.get("event_id"), errors="coerce"),
                "year": pd.to_numeric(snap.get("year"), errors="coerce"),
                "round_num": pd.to_numeric(snap.get("round_num"), errors="coerce"),
                "weather_temp_f": pd.to_numeric(snap.get("tempF"), errors="coerce"),
                "weather_wind_mph": pd.to_numeric(snap.get("windMph"), errors="coerce"),
                "weather_humidity": pd.to_numeric(snap.get("humidityPct"), errors="coerce"),
                "weather_rain": float("rain" in condition or "storm" in condition),
                "weather_fog": float("fog" in condition or "mist" in condition),
                "weather_snow": float("snow" in condition),
            }
        )
    return pd.DataFrame(rows)


def load_history() -> pd.DataFrame:
    cols = [
        "year", "event_id", "event_name", "event_completed", "dg_id", "round_num",
        "course_name", "teetime", "round_score", "birdies", "gir", "driving_acc",
        "sg_total", "sg_ott", "sg_app", "sg_putt",
    ]
    h = pd.read_csv(HIST, usecols=cols, low_memory=False)
    for c in ("year", "event_id", "dg_id", "round_num", "round_score", "birdies", "gir",
              "driving_acc", "sg_total", "sg_ott", "sg_app", "sg_putt"):
        h[c] = pd.to_numeric(h[c], errors="coerce")
    h = h[h["dg_id"].notna() & (h["year"] >= 2023)].copy()
    h["completed"] = pd.to_datetime(h["event_completed"], errors="coerce")
    h = h[h["completed"].notna()].copy()
    h["completed_ms"] = (h["completed"].astype("int64") // 10**6).astype(np.int64)
    h["gir_count"] = h["gir"] * 18.0
    h["fw_count"] = h["driving_acc"] * 14.0
    waves = h["teetime"].map(wave_from_teetime)
    h[["wave_morning", "wave_afternoon", "wave_unknown"]] = pd.DataFrame(waves.tolist(), index=h.index)
    h = h.merge(weather_frame(), on=["event_id", "year", "round_num"], how="left")
    h["weather_missing"] = h["weather_wind_mph"].isna().astype(float)
    h["event_key"] = h["event_name"].map(title_key)
    h["course_key"] = h["course_name"].astype(str).str.lower().str.strip()
    h = h.sort_values(["dg_id", "completed_ms", "round_num"]).reset_index(drop=True)
    h["n_prior"] = h.groupby("dg_id").cumcount()

    grouped = h.groupby("dg_id", sort=False)
    for window in WINDOWS:
        for col in STAT_COLS:
            h[f"w{window}_{col}"] = (
                grouped[col].shift(1).rolling(window, min_periods=max(4, window // 4))
                .mean().reset_index(level=0, drop=True)
            )
        for market, ycol in MARKET_Y.items():
            slug = market.lower().replace(" ", "_")
            h[f"w{window}_{slug}_std"] = (
                grouped[ycol].shift(1).rolling(window, min_periods=max(4, window // 4))
                .std().reset_index(level=0, drop=True)
            )
    return h


def feature_cols(window: int, market: str) -> list[str]:
    slug = market.lower().replace(" ", "_")
    return [
        "round_num",
        f"w{window}_{MARKET_Y[market]}",
        f"w{window}_{slug}_std",
        f"w{window}_sg_total",
        f"w{window}_sg_ott",
        f"w{window}_sg_app",
        f"w{window}_sg_putt",
        f"w{window}_birdies",
        f"w{window}_round_score",
        f"w{window}_gir_count",
        f"w{window}_fw_count",
        "n_prior",
        "weather_temp_f",
        "weather_wind_mph",
        "weather_humidity",
        "weather_rain",
        "weather_fog",
        "weather_snow",
        "weather_missing",
        "wave_morning",
        "wave_afternoon",
        "wave_unknown",
    ]


def matrix(train: pd.DataFrame, test: pd.DataFrame, cols: list[str]) -> tuple[np.ndarray, np.ndarray]:
    a = train[cols].to_numpy(dtype=float)
    b = test[cols].to_numpy(dtype=float)
    med = np.nanmedian(a, axis=0)
    med = np.where(np.isfinite(med), med, 0.0)
    return np.where(np.isfinite(a), a, med), np.where(np.isfinite(b), b, med)


def model() -> Pipeline:
    return Pipeline([
        ("scale", StandardScaler()),
        ("bayes", BayesianRidge(compute_score=True)),
    ])


def choose_window(train: pd.DataFrame, market: str) -> tuple[int, dict[str, float]]:
    """Inner chronological validation; the outer event is never used to select the window."""
    dates = np.sort(train["completed_ms"].dropna().unique())
    split = dates[max(1, int(len(dates) * 0.8)) - 1]
    fit = train[train["completed_ms"] < split]
    val = train[train["completed_ms"] >= split]
    scores = {}
    ycol = MARKET_Y[market]
    for window in WINDOWS:
        cols = feature_cols(window, market)
        needed = f"w{window}_{ycol}"
        f = fit[fit[needed].notna() & fit[ycol].notna()]
        v = val[val[needed].notna() & val[ycol].notna()]
        if len(f) < 500 or len(v) < 100:
            scores[str(window)] = None
            continue
        if len(f) > 30000:
            f = f.sample(30000, random_state=window)
        x_fit, x_val = matrix(f, v, cols)
        m = model()
        m.fit(x_fit, f[ycol].to_numpy(float))
        scores[str(window)] = round(float(np.mean(np.abs(m.predict(x_val) - v[ycol].to_numpy(float)))), 4)
    valid = {int(k): v for k, v in scores.items() if v is not None}
    return min(valid, key=valid.get), scores


def match_event_rows(history: pd.DataFrame, event_name: str, cutoff_ms: int, round_num: int) -> pd.DataFrame:
    # Current-event context is allowed: tee time and archived weather, never current outcomes.
    candidates = history[(history["year"] == 2026) & (history["round_num"] == round_num)]
    keys = candidates[["event_key", "event_name"]].drop_duplicates()
    target = title_key(event_name)
    exact = keys[keys["event_key"] == target]
    if not exact.empty:
        key = exact.iloc[0]["event_key"]
    elif not keys.empty:
        key = max(keys["event_key"], key=lambda k: SequenceMatcher(None, target, k).ratio())
    else:
        return candidates.iloc[0:0]
    return candidates[candidates["event_key"] == key]


def apply_walkforward_book_blend(
    frame: pd.DataFrame,
    events: pd.DataFrame,
    min_prior_rows: int = 80,
) -> tuple[pd.DataFrame, dict[str, dict]]:
    """Select blend by prior-event MAE only, then freeze it for the next event."""
    out = frame.copy()
    out["pred_bayesian_ridge_wf_book"] = np.nan
    selections = {}
    for market in MARKET_Y:
        market_mask = out["market"] == market
        for _, event in events.iterrows():
            event_name = event["event_name"]
            event_order = int(event["event_order"])
            prior = out[
                market_mask
                & (out["event_order"] < event_order)
                & out["pred_bayesian_ridge"].notna()
                & out["book_line"].notna()
                & out["actual"].notna()
            ]
            scores = {}
            if len(prior) >= min_prior_rows:
                actual = prior["actual"].to_numpy(float)
                model_mu = prior["pred_bayesian_ridge"].to_numpy(float)
                book_mu = prior["book_line"].to_numpy(float)
                for blend in BOOK_BLENDS:
                    blended = (1.0 - blend) * model_mu + blend * book_mu
                    scores[str(blend)] = round(float(np.mean(np.abs(blended - actual))), 4)
                chosen = min(BOOK_BLENDS, key=lambda b: scores[str(b)])
                source = "prior_event_mae"
            else:
                chosen = 0.0
                source = "insufficient_prior_rows_unblended"
            current_mask = market_mask & (out["event_name"] == event_name)
            out.loc[current_mask, "pred_bayesian_ridge_wf_book"] = (
                (1.0 - chosen) * out.loc[current_mask, "pred_bayesian_ridge"]
                + chosen * out.loc[current_mask, "book_line"]
            )
            selections[f"{market}|{event_name}"] = {
                "chosen_blend": chosen,
                "selection_source": source,
                "prior_rows": int(len(prior)),
                "prior_mae": scores,
            }
    return out, selections


def main():
    started = time.time()
    base = load_base()
    panel = base.load_bet_panel()
    events = panel[["event_name", "event_order", "cutoff_ms"]].drop_duplicates().sort_values("event_order")
    history = load_history()
    frames = []
    tuning = {}
    coverage = {"test_rows": 0, "weather_rows": 0, "morning_rows": 0, "afternoon_rows": 0}

    for market in MARKET_Y:
        print(f"[bayesian-ridge] {market}", flush=True)
        ycol = MARKET_Y[market]
        market_panel = panel[panel["market"] == market].reset_index(drop=True)
        pred = np.full(len(market_panel), np.nan)
        chosen = np.full(len(market_panel), np.nan)
        for _, event in events.iterrows():
            event_name = event["event_name"]
            cutoff = int(event["cutoff_ms"])
            mask = (market_panel["event_name"] == event_name).to_numpy()
            if not mask.any():
                continue
            outer_train = history[
                (history["completed_ms"] < cutoff) & history[ycol].notna() & (history["n_prior"] >= 6)
            ]
            window, scores = choose_window(outer_train, market)
            tuning[f"{market}|{event_name}"] = {"chosen_window": window, "inner_mae": scores}
            cols = feature_cols(window, market)
            needed = f"w{window}_{ycol}"
            fit = outer_train[outer_train[needed].notna()]
            if len(fit) > 40000:
                fit = fit.sample(40000, random_state=window)

            test_panel = market_panel.loc[mask]
            global_positions = np.flatnonzero(mask)
            contexts = []
            keep = []
            for local_pos, (_, row) in enumerate(test_panel.iterrows()):
                event_rows = match_event_rows(history, event_name, cutoff, int(row["round"]))
                current = event_rows[event_rows["dg_id"].round().astype("Int64") == int(row["dg_id"])]
                prior = history[(history["dg_id"] == row["dg_id"]) & (history["completed_ms"] < cutoff)]
                if current.empty or prior.empty:
                    continue
                context = current.iloc[0].copy()
                latest = prior.iloc[-1]
                for col in cols:
                    if col.startswith(f"w{window}_") or col == "n_prior":
                        context[col] = latest.get(col, np.nan)
                contexts.append(context)
                keep.append(global_positions[local_pos])
            if not contexts:
                continue
            test = pd.DataFrame(contexts)
            x_fit, x_test = matrix(fit, test, cols)
            m = model()
            m.fit(x_fit, fit[ycol].to_numpy(float))
            values = m.predict(x_test)
            if market == "Birdies":
                values = np.clip(values, 0.05, None)
            for i, pos in enumerate(keep):
                pred[pos] = values[i]
                chosen[pos] = window
            coverage["test_rows"] += len(test)
            coverage["weather_rows"] += int((test["weather_missing"] == 0).sum())
            coverage["morning_rows"] += int(test["wave_morning"].sum())
            coverage["afternoon_rows"] += int(test["wave_afternoon"].sum())
            print(f"  {event_name}: window={window} test={len(test)}", flush=True)

        chunk = market_panel.copy()
        chunk["pred_bayesian_ridge"] = pred
        chunk["selected_window"] = chosen
        frames.append(chunk)

    full = pd.concat(frames, ignore_index=True)
    full, blend_tuning = apply_walkforward_book_blend(full, events)
    strategies = []
    for blend, tag in ((0.0, "pure"), (0.35, "book_blend_35_reference")):
        strategies.append({
            "id": f"bayesian_ridge_weather_wave_tuned_{tag}",
            "book_blend": blend,
            "recommended": base.score_predictions(full, "pred_bayesian_ridge", book_blend=blend),
            "model_vs_actual": base.mae_bias(full, "pred_bayesian_ridge") if blend == 0 else None,
        })
    strategies.append({
        "id": "bayesian_ridge_weather_wave_nested_window_and_book",
        "book_blend": "walk_forward_selected",
        "recommended": base.score_predictions(full, "pred_bayesian_ridge_wf_book", book_blend=0.0),
        "model_vs_actual": base.mae_bias(full, "pred_bayesian_ridge_wf_book"),
    })
    payload = {
        "generated_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "hypothetical": True,
        "methodology": {
            "oos_event_count": int(len(events)),
            "events": events["event_name"].tolist(),
            "excluded_live_event": base.load_live_event(),
            "candidate_windows": list(WINDOWS),
            "window_selection": "inner chronological 80/20 validation by MAE inside each outer event",
            "candidate_book_blends": list(BOOK_BLENDS),
            "book_blend_selection": (
                "per-market expanding walk-forward MAE using earlier OOS events only; "
                "unblended until 80 prior graded rows"
            ),
            "weather_source": (
                "historical_round_weather.json Open-Meteo realized archive; retrospective "
                "weather proxy, not a captured pre-round forecast"
            ),
            "tee_wave_source": "historical_rounds_all.csv teetime; morning before noon, afternoon noon or later",
            "features": "selected rolling form/SG, weather, condition, AM/PM wave, missingness indicators",
            "coverage": coverage,
            "elapsed_sec": round(time.time() - started, 1),
        },
        "window_tuning": tuning,
        "book_blend_tuning": blend_tuning,
        "strategies": strategies,
    }
    OUT.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    print("\n=== Bayesian Ridge OOS ===", flush=True)
    for s in strategies:
        r = s["recommended"]
        print(f"{s['id']}: ROI={r['roi_pct']}% PnL={r['units']}u bets={r['bets']}", flush=True)
    print(f"Wrote {OUT}", flush=True)


if __name__ == "__main__":
    main()
