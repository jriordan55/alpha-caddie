#!/usr/bin/env python3
"""
Walk-forward OOS ROI for classical ML vs the same DK pre-round panel used by
compare-live-feel-oos.mjs. Does not change live projections.

Models: Ridge, Poisson, RandomForest, XGBoost, small MLP.
Features: leakage-safe player rolling stats from historical_rounds_all.csv
          (+ optional soft blend toward the DK book line).

  python scripts/ml-oos-roi.py
  → data/ml_oos_roi.json
"""
from __future__ import annotations

import json
import math
import time
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
from scipy.stats import norm
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import PoissonRegressor, Ridge
from sklearn.neural_network import MLPRegressor
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from xgboost import XGBRegressor

warnings.filterwarnings("ignore", category=UserWarning)

WEB = Path(__file__).resolve().parents[1]
REPO = WEB.parent
HIST = REPO / "data" / "historical_rounds_all.csv"
VS = WEB / "data" / "round_projection_vs_actual.csv"
PROJ = WEB / "projections.json"
OUT = WEB / "data" / "ml_oos_roi.json"

MARKETS = {
    "Birdies": {
        "actual": "actual_birdies",
        "model": "birdies_line",
        "book": "birdies_book_line",
        "over_odds": "birdies_over_odds",
        "under_odds": "birdies_under_odds",
        "over_res": "birdies_over",
        "under_res": "birdies_under",
        "hist_y": "birdies",
        "count": True,
    },
    "Total score": {
        "actual": "actual_round_score",
        "model": "round_score_line",
        "book": "round_score_book_line",
        "over_odds": "round_score_over_odds",
        "under_odds": "round_score_under_odds",
        "over_res": "round_score_over",
        "under_res": "round_score_under",
        "hist_y": "round_score",
        "count": False,
    },
    "GIR": {
        "actual": "actual_gir",
        "model": "gir_line",
        "book": "gir_book_line",
        "over_odds": "gir_over_odds",
        "under_odds": "gir_under_odds",
        "over_res": "gir_over",
        "under_res": "gir_under",
        "hist_y": "gir",
        "count": False,
    },
    "Fairways hit": {
        "actual": "actual_fairways",
        "model": "fairways_line",
        "book": "fairways_book_line",
        "over_odds": "fairways_over_odds",
        "under_odds": "fairways_under_odds",
        "over_res": "fairways_over",
        "under_res": "fairways_under",
        "hist_y": "driving_acc",
        "count": False,
        "scale_hist": 14.0,  # driving_acc is 0-1 → approx fairways on 14 holes
    },
}

POLICY = {
    "GIR": {"min_ev": 7.5, "min_gap": 0.5, "side": "both"},
    "Birdies": {"min_ev": 25.0, "min_gap": 0.5, "side": "both"},
    "Total score": {"min_ev": 15.0, "min_gap": 0.5, "side": "both"},
    "Fairways hit": {
        "min_ev": 7.5,
        "min_gap": 1.5,
        "side": "under",
        "min_gir_minus_fw": 2.5,
        "min_course_fw_width": 30.0,
    },
}

FEATURE_COLS = [
    "round_num",
    "l12_y",
    "l36_y",
    "l12_std_y",
    "l12_sg_total",
    "l36_sg_total",
    "l12_sg_ott",
    "l12_sg_app",
    "l12_sg_putt",
    "l12_gir",
    "l12_fw",
    "l12_birdies",
    "l12_score",
    "n_prior",
    "course_l50_y",
]


def clamp(x, lo, hi):
    return max(lo, min(hi, x))


def implied(am: float) -> float:
    if not np.isfinite(am) or am == 0:
        return np.nan
    return 100.0 / (am + 100.0) if am > 0 else (-am) / (-am + 100.0)


def poisson_prob_over(lam: float, line: float) -> float:
    lam = max(0.05, float(lam))
    # P(X > line) for half-lines: sum k=ceil(line+eps)..inf
    k_min = int(math.floor(line)) + 1
    # CDF via survival of Poisson using scipy would need scipy.stats.poisson
    from scipy.stats import poisson

    return float(poisson.sf(k_min - 1, lam))


def binomial_prob_over(mu: float, n: int, line: float) -> float:
    from scipy.stats import binom

    n = int(n)
    p = clamp(mu / n, 0.02, 0.98)
    k_min = int(math.floor(line)) + 1
    return float(binom.sf(k_min - 1, n, p))


def normal_prob_over(mu: float, line: float, sigma: float) -> float:
    sigma = max(0.35, float(sigma))
    # continuity: P(X > line) ≈ 1 - Φ((line+0.5 - μ)/σ) for integer scores; use mid
    return float(1.0 - norm.cdf(line, loc=mu, scale=sigma))


def model_prob_over(market: str, mu: float, line: float) -> float:
    if not (np.isfinite(mu) and np.isfinite(line)):
        return np.nan
    if market in ("Birdies", "Bogeys"):
        return poisson_prob_over(mu, line)
    if market == "GIR":
        return binomial_prob_over(mu, 18, line)
    if market == "Fairways hit":
        return binomial_prob_over(mu, 14, line)
    # Total score — discrete-ish normal σ ≈ 2.6–3.0
    sig = clamp(math.sqrt(max(abs(mu) * 0.08, 0.2)) * 3.2, 2.2, 3.6)
    return normal_prob_over(mu, line, sig)


def cap_edges(edge_over, edge_under, mu, book):
    if not (np.isfinite(mu) and np.isfinite(book)):
        return edge_over, edge_under
    if mu >= book:
        return edge_over, min(edge_under, 0.0)
    return min(edge_over, 0.0), edge_under


def pick_side(edge_over, edge_under, min_ev, mu, book, side_policy="both"):
    if side_policy == "over":
        if edge_over >= min_ev:
            return "over", edge_over
        return None
    if side_policy == "under":
        if edge_under >= min_ev:
            return "under", edge_under
        return None
    if edge_over >= min_ev and edge_over >= edge_under:
        return "over", edge_over
    if edge_under >= min_ev and edge_under > edge_over:
        return "under", edge_under
    return None


def qualifies(market, mu, book, ctx, side):
    p = POLICY.get(market)
    if not p:
        return False
    if not (np.isfinite(mu) and np.isfinite(book)):
        return False
    if abs(mu - book) < p["min_gap"]:
        return False
    if p["side"] == "over" and not (mu > book):
        return False
    if p["side"] == "under" and not (mu < book):
        return False
    if side == "over" and p["side"] == "under":
        return False
    if side == "under" and p["side"] == "over":
        return False
    if "min_gir_minus_fw" in p:
        g = ctx.get("gir_minus_fw", np.nan)
        if not (np.isfinite(g) and g >= p["min_gir_minus_fw"]):
            return False
    if "min_course_fw_width" in p:
        w = ctx.get("course_fw_width", np.nan)
        if np.isfinite(w) and w < p["min_course_fw_width"]:
            return False
    return True


def fold_title(s: str) -> str:
    return " ".join(str(s or "").lower().replace(".", "").split())


def load_live_event() -> str:
    if not PROJ.exists():
        return ""
    try:
        j = json.loads(PROJ.read_text(encoding="utf-8"))
        return str(j.get("event_name") or (j.get("meta") or {}).get("event_name") or "").strip()
    except Exception:
        return ""


def load_bet_panel() -> pd.DataFrame:
    df = pd.read_csv(VS, low_memory=False)
    df = df[
        (df["pricing_mode"].astype(str) == "default")
        & (df["pricing_skill"].astype(str) == "default")
        & (df["book_odds_source"].astype(str) == "pre_round_audit")
    ].copy()
    df["event_name"] = df["event_name"].astype(str).str.strip()
    df["dg_id"] = pd.to_numeric(df["dg_id"], errors="coerce")
    df["round"] = pd.to_numeric(df["round"], errors="coerce")
    df["event_ms"] = pd.to_datetime(
        df["projections_updated_at"].fillna(df["exported_at"]), errors="coerce"
    ).astype("int64") // 10**6
    df = df[df["dg_id"].notna() & df["round"].notna() & (df["event_name"] != "")]
    live = load_live_event()
    if live:
        live_f = fold_title(live)
        df = df[df["event_name"].map(fold_title) != live_f]
    # expand to long market rows
    rows = []
    for market, cols in MARKETS.items():
        sub = df.copy()
        sub["market"] = market
        sub["actual"] = pd.to_numeric(sub[cols["actual"]], errors="coerce")
        sub["book_line"] = pd.to_numeric(sub[cols["book"]], errors="coerce")
        sub["csv_model_line"] = pd.to_numeric(sub[cols["model"]], errors="coerce")
        sub["over_odds"] = pd.to_numeric(sub[cols["over_odds"]], errors="coerce")
        sub["under_odds"] = pd.to_numeric(sub[cols["under_odds"]], errors="coerce")
        sub["over_res"] = sub[cols["over_res"]].astype(str).str.strip().str.upper()
        sub["under_res"] = sub[cols["under_res"]].astype(str).str.strip().str.upper()
        sub["gir_minus_fw"] = pd.to_numeric(sub.get("gir_minus_fw"), errors="coerce")
        sub["course_fw_width"] = pd.to_numeric(sub.get("course_fw_width"), errors="coerce")
        # stub-zero birdies
        if market == "Birdies":
            score = pd.to_numeric(sub["actual_round_score"], errors="coerce")
            bad = (sub["actual"] == 0) & score.notna() & (score > 0)
            sub.loc[bad, "actual"] = np.nan
        keep = sub["book_line"].notna() & np.isfinite(sub["book_line"])
        rows.append(sub.loc[keep])
    out = pd.concat(rows, ignore_index=True)
    # event order
    ev = (
        out.groupby("event_name", as_index=False)["event_ms"]
        .min()
        .sort_values("event_ms")
    )
    out["event_order"] = out["event_name"].map({e: i for i, e in enumerate(ev["event_name"])})
    out["cutoff_ms"] = out["event_name"].map(dict(zip(ev["event_name"], ev["event_ms"])))
    return out


def _roll_mean(grouped, col, window, min_periods):
    return (
        grouped[col]
        .shift(1)
        .rolling(window, min_periods=min_periods)
        .mean()
        .reset_index(level=0, drop=True)
    )


def _roll_std(grouped, col, window, min_periods):
    return (
        grouped[col]
        .shift(1)
        .rolling(window, min_periods=min_periods)
        .std()
        .reset_index(level=0, drop=True)
    )


def load_hist_features() -> pd.DataFrame:
    usecols = [
        "year",
        "event_name",
        "event_completed",
        "dg_id",
        "round_num",
        "course_name",
        "round_score",
        "birdies",
        "bogies",
        "gir",
        "driving_acc",
        "sg_total",
        "sg_ott",
        "sg_app",
        "sg_putt",
    ]
    h = pd.read_csv(HIST, usecols=usecols, low_memory=False)
    h["dg_id"] = pd.to_numeric(h["dg_id"], errors="coerce")
    h["year"] = pd.to_numeric(h["year"], errors="coerce")
    h = h[h["dg_id"].notna() & (h["year"] >= 2023)].copy()
    h["completed"] = pd.to_datetime(h["event_completed"], errors="coerce")
    h = h[h["completed"].notna()].sort_values(["dg_id", "completed", "round_num"])
    for c in [
        "round_score",
        "birdies",
        "bogies",
        "gir",
        "driving_acc",
        "sg_total",
        "sg_ott",
        "sg_app",
        "sg_putt",
        "round_num",
    ]:
        h[c] = pd.to_numeric(h[c], errors="coerce")
    # Book lines are hole-counts; hist stores GIR/FW as rates.
    h["gir_count"] = h["gir"] * 18.0
    h["fw_proxy"] = h["driving_acc"] * 14.0
    h["n_prior"] = h.groupby("dg_id").cumcount()
    h["completed_ms"] = (h["completed"].astype("int64") // 10**6).astype(np.int64)
    h["course_key"] = h["course_name"].astype(str).str.lower().str.strip()

    g = h.groupby("dg_id", sort=False)
    for src, dst in [
        ("birdies", "l12_birdies"),
        ("round_score", "l12_score"),
        ("gir_count", "l12_gir"),
        ("fw_proxy", "l12_fw"),
        ("sg_total", "l12_sg_total"),
        ("sg_ott", "l12_sg_ott"),
        ("sg_app", "l12_sg_app"),
        ("sg_putt", "l12_sg_putt"),
    ]:
        h[dst] = _roll_mean(g, src, 12, 3)
    for src, dst in [
        ("birdies", "l36_birdies"),
        ("round_score", "l36_score"),
        ("gir_count", "l36_gir"),
        ("fw_proxy", "l36_fw"),
        ("sg_total", "l36_sg_total"),
    ]:
        h[dst] = _roll_mean(g, src, 36, 6)
    return h


def attach_market_y(h: pd.DataFrame, market: str) -> pd.DataFrame:
    out = h  # mutate views carefully via assigned cols only
    if market == "Fairways hit":
        out = h.copy()
        out["y"] = out["fw_proxy"]
        out["l12_y"] = out["l12_fw"]
        out["l36_y"] = out["l36_fw"]
    elif market == "Birdies":
        out = h.copy()
        out["y"] = out["birdies"]
        out["l12_y"] = out["l12_birdies"]
        out["l36_y"] = out["l36_birdies"]
    elif market == "Total score":
        out = h.copy()
        out["y"] = out["round_score"]
        out["l12_y"] = out["l12_score"]
        out["l36_y"] = out["l36_score"]
    else:  # GIR
        out = h.copy()
        out["y"] = out["gir_count"]
        out["l12_y"] = out["l12_gir"]
        out["l36_y"] = out["l36_gir"]
    g = out.groupby("dg_id", sort=False)
    out["l12_std_y"] = _roll_std(g, "y", 12, 3)
    out = out.sort_values(["course_key", "completed_ms"])
    cg = out.groupby("course_key", sort=False)
    out["course_l50_y"] = _roll_mean(cg, "y", 50, 8)
    out = out.sort_values(["dg_id", "completed_ms", "round_num"])
    return out


def make_models(market: str, is_count: bool):
    models = {
        "ridge": Ridge(alpha=2.0),
        "random_forest": RandomForestRegressor(
            n_estimators=80,
            max_depth=7,
            min_samples_leaf=40,
            n_jobs=-1,
            random_state=42,
        ),
        "xgboost": XGBRegressor(
            n_estimators=120,
            max_depth=4,
            learning_rate=0.07,
            subsample=0.85,
            colsample_bytree=0.85,
            reg_lambda=2.0,
            objective="count:poisson" if is_count else "reg:squarederror",
            n_jobs=-1,
            random_state=42,
            verbosity=0,
        ),
        "neural_net": Pipeline(
            [
                ("scaler", StandardScaler()),
                (
                    "mlp",
                    MLPRegressor(
                        hidden_layer_sizes=(48, 24),
                        activation="relu",
                        alpha=1e-3,
                        learning_rate_init=1e-3,
                        max_iter=120,
                        early_stopping=True,
                        random_state=42,
                    ),
                ),
            ]
        ),
    }
    if is_count:
        models["poisson"] = Pipeline(
            [
                ("scaler", StandardScaler()),
                ("pois", PoissonRegressor(alpha=1.0, max_iter=300)),
            ]
        )
    else:
        models["poisson"] = Ridge(alpha=2.0)  # Poisson not natural for score/GIR; keep slot
    return models

def x_matrix(df: pd.DataFrame) -> np.ndarray:
    X = df[FEATURE_COLS].astype(float).replace([np.inf, -np.inf], np.nan)
    return X.to_numpy()


def impute_train_test(X_train, X_test):
    med = np.nanmedian(X_train, axis=0)
    med = np.where(np.isfinite(med), med, 0.0)
    Xt = np.where(np.isfinite(X_train), X_train, med)
    Xs = np.where(np.isfinite(X_test), X_test, med)
    return Xt, Xs, med


def player_feature_lookup(h_mkt: pd.DataFrame, dg_ids, cutoff_ms: int) -> pd.DataFrame:
    """Latest pre-cutoff feature row per player."""
    sub = h_mkt[h_mkt["completed_ms"] < cutoff_ms]
    if sub.empty:
        return pd.DataFrame()
    # last row per dg
    last = sub.groupby("dg_id", as_index=False).tail(1)
    last = last[last["dg_id"].isin(dg_ids)]
    return last


def score_predictions(panel: pd.DataFrame, pred_col: str, book_blend: float = 0.0):
    units = 0.0
    n = 0
    wins = 0
    by_market = {}
    for r in panel.itertuples(index=False):
        mu = getattr(r, pred_col)
        book = r.book_line
        if book_blend > 0 and np.isfinite(book) and np.isfinite(mu):
            mu = (1.0 - book_blend) * mu + book_blend * book
        if not (np.isfinite(mu) and np.isfinite(book)):
            continue
        p_over = model_prob_over(r.market, mu, book)
        if not np.isfinite(p_over):
            continue
        p_under = 1.0 - p_over
        q_o, q_u = implied(r.over_odds), implied(r.under_odds)
        if np.isfinite(q_o) and np.isfinite(q_u) and (q_o + q_u) > 0:
            fair_o, fair_u = q_o / (q_o + q_u), q_u / (q_o + q_u)
            edge_o = (p_over - fair_o) * 100
            edge_u = (p_under - fair_u) * 100
        else:
            continue
        edge_o, edge_u = cap_edges(edge_o, edge_u, mu, book)
        pol = POLICY[r.market]
        pick = pick_side(edge_o, edge_u, pol["min_ev"], mu, book, pol["side"])
        if not pick:
            continue
        side, _ = pick
        ctx = {"gir_minus_fw": r.gir_minus_fw, "course_fw_width": r.course_fw_width}
        if not qualifies(r.market, mu, book, ctx, side):
            continue
        res = r.over_res if side == "over" else r.under_res
        odds = r.over_odds if side == "over" else r.under_odds
        if res not in ("W", "L", "P"):
            continue
        pnl = (odds / 100.0 if odds > 0 else 100.0 / -odds) if res == "W" else (-1.0 if res == "L" else 0.0)
        units += pnl
        n += 1
        if res == "W":
            wins += 1
        mk = by_market.setdefault(r.market, {"units": 0.0, "bets": 0, "wins": 0})
        mk["units"] += pnl
        mk["bets"] += 1
        if res == "W":
            mk["wins"] += 1
    return {
        "units": round(units, 2),
        "bets": n,
        "wins": wins,
        "hit_pct": round(100.0 * wins / n, 1) if n else None,
        "roi_pct": round(100.0 * units / n, 1) if n else None,
        "by_market": {
            k: {"units": round(v["units"], 1), "bets": v["bets"], "wins": v["wins"]}
            for k, v in by_market.items()
        },
    }


def mae_bias(panel: pd.DataFrame, pred_col: str):
    out = {}
    for market, g in panel.groupby("market"):
        a = g["actual"].to_numpy(dtype=float)
        p = g[pred_col].to_numpy(dtype=float)
        m = np.isfinite(a) & np.isfinite(p)
        if market == "Birdies":
            # already cleaned stubs
            pass
        if m.sum() < 5:
            continue
        err = p[m] - a[m]
        out[market] = {
            "n": int(m.sum()),
            "bias": round(float(err.mean()), 2),
            "mae": round(float(np.abs(err).mean()), 2),
        }
    return out


def main():
    t0 = time.time()
    print("[ml-oos] loading bet panel…", flush=True)
    panel = load_bet_panel()
    events = (
        panel[["event_name", "event_order", "cutoff_ms"]]
        .drop_duplicates()
        .sort_values("event_order")
    )
    print(f"[ml-oos] OOS rows={len(panel)} events={len(events)}", flush=True)
    print("         " + " | ".join(events["event_name"].tolist()), flush=True)

    print("[ml-oos] loading / featurizing historical rounds…", flush=True)
    hist = load_hist_features()
    print(f"[ml-oos] hist rows={len(hist)}", flush=True)

    # predictions storage
    pred_frames = []

    model_names = ["ridge", "poisson", "random_forest", "xgboost", "neural_net", "l36_mean"]

    for market in MARKETS:
        print(f"\n[ml-oos] market={market}", flush=True)
        h_mkt = attach_market_y(hist, market)
        mpanel = panel[panel["market"] == market].reset_index(drop=True)
        preds = {n: np.full(len(mpanel), np.nan) for n in model_names}

        for _, ev in events.iterrows():
            cutoff = int(ev["cutoff_ms"])
            ev_name = ev["event_name"]
            mask_ev = (mpanel["event_name"] == ev_name).to_numpy()
            if not mask_ev.any():
                continue
            # train on hist completed before this event
            train = h_mkt[
                (h_mkt["completed_ms"] < cutoff)
                & h_mkt["y"].notna()
                & h_mkt["l12_y"].notna()
                & (h_mkt["n_prior"] >= 6)
            ]
            # subsample for speed
            if len(train) > 25000:
                train = train.sample(25000, random_state=42)
            test_rows = mpanel.loc[mask_ev]
            global_pos = np.flatnonzero(mask_ev)
            feats = player_feature_lookup(
                h_mkt, set(test_rows["dg_id"].astype(int)), cutoff
            )
            if feats.empty or len(train) < 500:
                print(f"  skip {ev_name}: train={len(train)} feat_players={len(feats)}", flush=True)
                continue
            feat_by_dg = feats.set_index("dg_id")
            # build test feature frame aligned to test_rows
            X_list = []
            keep_global = []
            for local_i, (_, row) in enumerate(test_rows.iterrows()):
                dg = int(row["dg_id"])
                if dg not in feat_by_dg.index:
                    continue
                fr = feat_by_dg.loc[dg]
                if isinstance(fr, pd.DataFrame):
                    fr = fr.iloc[-1]
                d = {c: fr[c] if c in fr.index else np.nan for c in FEATURE_COLS}
                d["round_num"] = float(row["round"])
                X_list.append(d)
                keep_global.append(global_pos[local_i])
            if not X_list:
                continue
            X_test_df = pd.DataFrame(X_list)
            X_train = x_matrix(train)
            y_train = train["y"].to_numpy(dtype=float)
            X_test = x_matrix(X_test_df)
            Xt, Xs, _ = impute_train_test(X_train, X_test)

            # L36 mean baseline
            l36 = X_test_df["l36_y"].to_numpy(dtype=float)
            for j, gpos in enumerate(keep_global):
                preds["l36_mean"][gpos] = l36[j]

            models = make_models(market, MARKETS[market]["count"])
            for name, model in models.items():
                try:
                    yt = y_train.copy()
                    if name in ("poisson", "xgboost") and MARKETS[market]["count"]:
                        yt = np.clip(yt, 0, None)
                    model.fit(Xt, yt)
                    p = model.predict(Xs)
                    if MARKETS[market]["count"]:
                        p = np.clip(p, 0.05, None)
                    for j, gpos in enumerate(keep_global):
                        preds[name][gpos] = float(p[j])
                except Exception as e:
                    print(f"  {name} failed on {ev_name}: {e}", flush=True)
            print(f"  {ev_name}: train={len(train)} test={len(keep_global)}", flush=True)

        chunk = mpanel.copy()
        for name in model_names:
            chunk[f"pred_{name}"] = preds[name]
        pred_frames.append(chunk)

    full = pd.concat(pred_frames, ignore_index=True)

    # also score CSV model lines as reference (export-time, not reconstructed WF)
    full["pred_csv_model"] = full["csv_model_line"]

    strategies = []
    for name in model_names + ["csv_model"]:
        col = f"pred_{name}"
        for blend, tag in [(0.0, "pure"), (0.35, "book_blend_35")]:
            sid = f"{name}_{tag}"
            rec = score_predictions(full, col, book_blend=blend)
            strategies.append(
                {
                    "id": sid,
                    "name": f"{name.replace('_', ' ')} ({tag})",
                    "model": name,
                    "book_blend": blend,
                    "recommended": rec,
                    "model_vs_actual": mae_bias(full, col) if blend == 0 else None,
                }
            )

    # book line as μ (should be near zero edge after vig/policy)
    full["pred_book"] = full["book_line"]
    strategies.append(
        {
            "id": "book_line_pure",
            "name": "DK book line as mu (sanity)",
            "model": "book",
            "book_blend": 0.0,
            "recommended": score_predictions(full, "pred_book", 0.0),
            "model_vs_actual": mae_bias(full, "pred_book"),
        }
    )

    strategies.sort(
        key=lambda s: (
            -(s["recommended"]["roi_pct"] or -999),
            -(s["recommended"]["units"] or -999),
        )
    )

    payload = {
        "generated_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "hypothetical": True,
        "note": (
            "Walk-forward ML OOS on pre-round DK rows. Models train only on historical "
            "rounds completed before each event. Same recommended EV/gap policy as live-feel OOS. "
            "Does not change live projections. Reference WF day+form skill36 was ~6.8% ROI."
        ),
        "methodology": {
            "oos_bet_rows": int(len(full)),
            "oos_event_count": int(events.shape[0]),
            "excluded_live_event": load_live_event(),
            "events": events["event_name"].tolist(),
            "features": FEATURE_COLS,
            "policy": "recommended per-market EV/gap filters",
            "elapsed_sec": round(time.time() - t0, 1),
            "hist_years": "2023+",
        },
        "strategies": strategies,
    }
    OUT.write_text(json.dumps(payload, indent=2), encoding="utf-8")
    print("\n=== Ranked ML OOS (recommended policy) ===\n", flush=True)
    print(f"{'strategy':<36} {'ROI%':>7} {'PnL(u)':>9} {'bets':>6} {'hit%':>6}", flush=True)
    for s in strategies:
        r = s["recommended"]
        print(
            f"{s['name']:<36} {r['roi_pct'] if r['roi_pct'] is not None else float('nan'):7.1f} "
            f"{r['units']:9.1f} {r['bets']:6d} {r['hit_pct'] if r['hit_pct'] is not None else float('nan'):6.1f}",
            flush=True,
        )
    print(f"\nWrote {OUT}", flush=True)


if __name__ == "__main__":
    main()
