/**
 * In-round μ adjustments from preds/in-play scratch fields (shared: export scripts + projection tracker).
 */

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function liveRowMatchesRound(row, meta) {
  const liveR = Math.round(num(meta?.datagolf_live_current_round ?? meta?.display_round, NaN));
  const pr = Math.round(num(row?.round, NaN));
  return Number.isFinite(liveR) && liveR >= 1 && liveR <= 4 && pr === liveR;
}

function courseParSumFirstNHoles(holePars, n) {
  const holes = Number.isFinite(n) ? Math.max(0, Math.min(18, Math.round(n))) : 0;
  if (!holes) return NaN;
  if (Array.isArray(holePars) && holePars.length >= holes) {
    let s = 0;
    for (let i = 0; i < holes; i++) {
      const p = num(holePars[i], NaN);
      if (!Number.isFinite(p)) return NaN;
      s += p;
    }
    return s;
  }
  return NaN;
}

/** Mid-round revision to total_score μ (mirrors app.js). */
export function liveCurrentRoundTotalScoreMuDelta(row, meta) {
  if (meta?.in_play_affects_round_odds !== true) return 0;
  if (!liveRowMatchesRound(row, meta)) return 0;
  const thru = Math.round(num(row.dg_live_thru, NaN));
  const today = num(row.dg_live_today, NaN);
  const baseMu = num(row.total_score, NaN);
  const par18 = num(meta?.course_par_18, NaN);
  const holePars = meta?.hole_pars;
  if (!Number.isFinite(baseMu) || !Number.isFinite(par18)) return 0;
  if (!Number.isFinite(today)) return 0;

  if (Number.isFinite(thru) && thru >= 18) {
    return clamp(par18 + today - baseMu, -14, 14);
  }
  if (!Number.isFinite(thru) || thru < 1) return 0;

  let parThru = courseParSumFirstNHoles(holePars, thru);
  if (!Number.isFinite(parThru)) parThru = (par18 / 18) * thru;
  const parRem = par18 - parThru;
  const rem = 18 - thru;
  if (rem <= 0) return 0;
  const expExcessRem = ((baseMu - par18) * rem) / 18;
  const actualStrokes = parThru + today;
  return clamp(actualStrokes + parRem + expExcessRem - baseMu, -12, 12);
}

export function livePartialRoundCountPropAdjust(market, row, meta) {
  const out = { muDelta: 0, sigmaScale: 1 };
  if (meta?.in_play_affects_round_odds !== true) return out;
  if (market !== "Birdies" && market !== "Pars" && market !== "Bogeys") return out;
  if (!liveRowMatchesRound(row, meta)) return out;
  const thru = Math.round(num(row.dg_live_thru, NaN));
  if (!Number.isFinite(thru) || thru < 1) return out;
  const rem = 18 - thru;
  if (rem < 0) return out;
  const field = market === "Birdies" ? "birdies" : market === "Pars" ? "pars" : "bogeys";
  const muFull = num(row[field], NaN);
  if (!Number.isFinite(muFull) || muFull < 0) return out;
  let b = num(row.dg_live_birdies_so_far, NaN);
  let bg = num(row.dg_live_bogeys_so_far, NaN);
  if (!Number.isFinite(b)) b = 0;
  if (!Number.isFinite(bg)) bg = 0;
  const eg = num(row.dg_live_eagles_so_far, NaN);
  const eagles = Number.isFinite(eg) && eg >= 0 ? Math.min(thru, Math.round(eg)) : 0;
  let pSo = num(row.dg_live_pars_so_far, NaN);
  if (!Number.isFinite(pSo)) pSo = Math.max(0, Math.min(thru, thru - b - bg - eagles));
  const rate = muFull / 18;
  const soFar = market === "Birdies" ? b + eagles : market === "Bogeys" ? bg : pSo;
  const muLive = clamp(soFar + rate * rem, 0, 18);
  out.muDelta = muLive - muFull;
  if (thru >= 18) out.sigmaScale = 0.26;
  else out.sigmaScale = clamp(Math.sqrt(rem / 18), 0.17, 1);
  return out;
}
