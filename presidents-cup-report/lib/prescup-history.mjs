/**
 * Presidents Cup final scores (1994–2024).
 * Sources: PGA Tour media guide, presidentscup.com/history, NBC Sports.
 */

export const PRES_CUP_RESULTS = [
  { year: 1994, usa: 20.0, int: 12.0, total: 32 },
  { year: 1996, usa: 16.5, int: 15.5, total: 32 },
  { year: 1998, usa: 11.5, int: 20.5, total: 32 },
  { year: 2000, usa: 21.5, int: 10.5, total: 32 },
  { year: 2003, usa: 17.0, int: 17.0, total: 34, tie: true },
  { year: 2005, usa: 18.5, int: 15.5, total: 34 },
  { year: 2007, usa: 19.5, int: 14.5, total: 34 },
  { year: 2009, usa: 19.5, int: 14.5, total: 34 },
  { year: 2011, usa: 19.0, int: 15.0, total: 34 },
  { year: 2013, usa: 18.5, int: 15.5, total: 34 },
  { year: 2015, usa: 15.5, int: 14.5, total: 30 },
  { year: 2017, usa: 19.0, int: 11.0, total: 30 },
  { year: 2019, usa: 16.0, int: 14.0, total: 30 },
  { year: 2022, usa: 17.5, int: 12.5, total: 30 },
  { year: 2024, usa: 18.5, int: 11.5, total: 30 },
];

/** Current format since 2015: 30 points, first to 15.5. */
export const MODERN_FORMAT = {
  total_points: 30,
  win_target: 15.5,
  tie_score: 15,
  singles_matches: 10,
  team_matches_per_session: 5,
};

export function historicalTieRate({ modernOnly = false } = {}) {
  const rows = modernOnly
    ? PRES_CUP_RESULTS.filter((r) => r.year >= 2015)
    : PRES_CUP_RESULTS;
  const ties = rows.filter((r) => r.tie || r.usa === r.int).length;
  return { rate: ties / rows.length, ties, cups: rows.length };
}

export function historicalMarginStats() {
  const margins = PRES_CUP_RESULTS.map((r) => Math.abs(r.usa - r.int));
  const onePoint = margins.filter((m) => m <= 1).length;
  return {
    avg_margin: margins.reduce((s, m) => s + m, 0) / margins.length,
    one_point_or_less: onePoint / margins.length,
  };
}

/** Fixed tie mass for cup retain + all 3-way winner markets (≈0.4%). */
export const CALIBRATED_TIE_PROB = 0.004;

/**
 * Cap tie probability and reallocate excess mass to USA / International proportionally.
 */
export function capThreeWayProb(usa, intl, tie) {
  const u = Math.max(0, usa);
  const i = Math.max(0, intl);
  const t = Math.max(0, tie);
  const sum = u + i + t;
  if (sum <= 0) {
    const half = (1 - CALIBRATED_TIE_PROB) / 2;
    return { usa: half, int: half, tie: CALIBRATED_TIE_PROB };
  }
  const nu = u / sum;
  const ni = i / sum;
  const nt = t / sum;
  const cap = Math.min(CALIBRATED_TIE_PROB, nt);
  if (nt <= cap + 1e-12) return { usa: nu, int: ni, tie: nt };
  const scale = (1 - cap) / (1 - nt);
  return { usa: nu * scale, int: ni * scale, tie: cap };
}

/** @deprecated use CALIBRATED_TIE_PROB */
export function targetTieRate() {
  return CALIBRATED_TIE_PROB;
}

export function historicalUsaWinRate() {
  const wins = PRES_CUP_RESULTS.filter((r) => r.usa > r.int).length;
  return wins / PRES_CUP_RESULTS.length;
}

/** Logistic mapping from per-player SG gap → USA outright win (excludes retain-on-tie). */
export function skillImpliedUsaWin(usaSkill, intSkill) {
  const gap = (usaSkill || 0) - (intSkill || 0);
  return 1 / (1 + Math.exp(-3.4 * (gap - 0.32)));
}

/**
 * Shrink tie mass, then blend outright winners toward skill + Presidents Cup history.
 */
export function calibrateCupProb(raw, usaSkill, intSkill) {
  const rawTie = raw.usa_retain_tie;
  const tie = CALIBRATED_TIE_PROB;
  const delta = rawTie - tie;
  const winPool = raw.usa_win + raw.int_win;
  let usaWin = winPool > 0 ? raw.usa_win + delta * (raw.usa_win / winPool) : raw.usa_win;
  let intWin = winPool > 0 ? raw.int_win + delta * (raw.int_win / winPool) : raw.int_win;

  const skillTarget = skillImpliedUsaWin(usaSkill, intSkill);
  const histTarget = historicalUsaWinRate();
  const blendTarget = 0.55 * skillTarget + 0.45 * histTarget;
  const blendWeight = 0.62;
  usaWin = (1 - blendWeight) * usaWin + blendWeight * blendTarget;
  intWin = Math.max(0.04, 1 - tie - usaWin);

  return {
    usa_win: usaWin,
    int_win: intWin,
    usa_retain_tie: tie,
    calibration: {
      raw_tie: rawTie,
      target_tie: CALIBRATED_TIE_PROB,
      calibrated_tie: tie,
      raw_usa_win: raw.usa_win,
      skill_implied_usa: skillTarget,
      historical_usa_win: histTarget,
      blend_target_usa: blendTarget,
      historical_all_time_tie: historicalTieRate().rate,
      historical_modern_tie: historicalTieRate({ modernOnly: true }).rate,
      skill_gap: (usaSkill || 0) - (intSkill || 0),
    },
  };
}
