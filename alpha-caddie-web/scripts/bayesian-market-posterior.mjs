/**
 * Bayesian-style market prior + model evidence pooling for round props.
 *
 * Prior priority:
 *   1) GamedayMath/Nostradamus fair probability fields, when supplied
 *   2) Sharp-consensus fair probability fields, when supplied
 *   3) Blended no-vig from posted two-way book odds
 *
 * The posterior is a reliability-weighted update from the market-implied mean
 * toward the independent model mean. model_fallback props are never priors.
 */

const DEFAULT_MODEL_EVIDENCE_WEIGHT = {
  // Fit from OOS model-vs-actual and no-vig-prior-vs-actual disagreement.
  // The market remains the dominant prior; the independent model moves it
  // only where historical disagreement has contained useful information.
  "Total score": 0.1,
  Birdies: 0.24,
  Bogeys: 0.2,
  Pars: 0.2,
  GIR: 0.22,
  "Fairways hit": 0.05,
};

function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function impliedProbFromAmerican(am) {
  const v = num(am, NaN);
  if (!Number.isFinite(v) || v === 0) return NaN;
  return v < 0 ? -v / (-v + 100) : 100 / (v + 100);
}

function americanFromProb(prob) {
  const p = clamp(num(prob, NaN), 1e-6, 1 - 1e-6);
  if (!Number.isFinite(p)) return NaN;
  return p >= 0.5 ? -100 * p / (1 - p) : 100 * (1 - p) / p;
}

/**
 * Public approximation of GamedayMath's described Blend method:
 * midpoint of equal-margin and proportional (normalized implied probability).
 */
export function blendedNoVigTwoWay(overOdds, underOdds) {
  const qOver = impliedProbFromAmerican(overOdds);
  const qUnder = impliedProbFromAmerican(underOdds);
  if (!Number.isFinite(qOver) || !Number.isFinite(qUnder)) {
    return { fairOver: NaN, fairUnder: NaN, overround: NaN };
  }
  const sum = qOver + qUnder;
  if (!(sum > 0)) return { fairOver: NaN, fairUnder: NaN, overround: NaN };
  const margin = sum - 1;
  const equalOver = clamp(qOver - margin / 2, 0.001, 0.999);
  const proportionalOver = clamp(qOver / sum, 0.001, 0.999);
  const fairOver = clamp((equalOver + proportionalOver) / 2, 0.001, 0.999);
  return { fairOver, fairUnder: 1 - fairOver, overround: margin };
}

export function fairPriorFromProp(prop) {
  if (!prop || typeof prop !== "object") return null;
  const source = String(prop.source || "").trim().toLowerCase();
  if (source === "model_fallback" || source === "prizepicks") return null;

  const directProb = num(
    prop.no_vig_over_prob ??
      prop.nostradamus_over_prob ??
      prop.fair_over_prob ??
      prop.sharp_over_prob,
    NaN,
  );
  if (Number.isFinite(directProb) && directProb > 0 && directProb < 1) {
    return {
      fairOver: directProb,
      fairUnder: 1 - directProb,
      source:
        source === "gamedaymath" || source === "nostradamus"
          ? "gamedaymath_nostradamus"
          : source || "sharp_consensus",
      method: "supplied_no_vig_probability",
    };
  }

  const noVigOverOdds = num(
    prop.no_vig_over_odds ?? prop.nostradamus_over_odds ?? prop.fair_over_odds,
    NaN,
  );
  if (Number.isFinite(noVigOverOdds)) {
    const fairOver = impliedProbFromAmerican(noVigOverOdds);
    if (Number.isFinite(fairOver)) {
      return {
        fairOver,
        fairUnder: 1 - fairOver,
        source:
          source === "gamedaymath" || source === "nostradamus"
            ? "gamedaymath_nostradamus"
            : source || "sharp_consensus",
        method: "supplied_no_vig_odds",
      };
    }
  }

  const overOdds = num(prop.over_odds, NaN);
  const underOdds = num(prop.under_odds, NaN);
  const fair = blendedNoVigTwoWay(overOdds, underOdds);
  if (!Number.isFinite(fair.fairOver)) return null;
  return {
    fairOver: fair.fairOver,
    fairUnder: fair.fairUnder,
    source: source || "posted_book",
    method: "blended_no_vig_equal_margin_proportional",
    overround: fair.overround,
  };
}

function poissonCdf(k, lambda) {
  const kk = Math.floor(k);
  if (kk < 0) return 0;
  const lam = Math.max(1e-8, lambda);
  let term = Math.exp(-lam);
  let sum = term;
  for (let i = 1; i <= kk; i++) {
    term *= lam / i;
    sum += term;
  }
  return clamp(sum, 0, 1);
}

function binomialCdf(k, n, p) {
  const kk = Math.min(n, Math.floor(k));
  if (kk < 0) return 0;
  const pp = clamp(p, 1e-8, 1 - 1e-8);
  let term = (1 - pp) ** n;
  let sum = term;
  for (let i = 0; i < kk; i++) {
    term *= ((n - i) / (i + 1)) * (pp / (1 - pp));
    sum += term;
  }
  return clamp(sum, 0, 1);
}

function normalCdf(z) {
  const t = 1 / (1 + 0.2316419 * Math.abs(z));
  const d = 0.3989423 * Math.exp((-z * z) / 2);
  const p =
    d * t * (0.3193815 + t * (-0.3565638 + t * (1.7814779 + t * (-1.821256 + t * 1.3302744))));
  return z >= 0 ? 1 - p : p;
}

function invNormalCdf(p) {
  const target = clamp(p, 1e-8, 1 - 1e-8);
  let lo = -8;
  let hi = 8;
  for (let i = 0; i < 70; i++) {
    const mid = (lo + hi) / 2;
    if (normalCdf(mid) < target) lo = mid;
    else hi = mid;
  }
  return (lo + hi) / 2;
}

export function probabilityOverAtMean(market, mean, line, opts = {}) {
  const mu = num(mean, NaN);
  const ln = num(line, NaN);
  if (!Number.isFinite(mu) || !Number.isFinite(ln)) return NaN;
  if (market === "Total score" || market === "Total Score") {
    const sd = Math.max(0.5, num(opts.roundSd, 3.2));
    return 1 - normalCdf((ln - mu) / sd);
  }
  const threshold = Math.floor(ln);
  if (market === "GIR" || market === "Fairways hit") {
    const holes = market === "GIR" ? 18 : Math.max(1, Math.round(num(opts.fairwayHoles, 14)));
    return 1 - binomialCdf(threshold, holes, clamp(mu / holes, 0.001, 0.999));
  }
  return 1 - poissonCdf(threshold, Math.max(0.01, mu));
}

export function meanAtOverProbability(market, fairOver, line, opts = {}) {
  const p = clamp(num(fairOver, NaN), 0.001, 0.999);
  const ln = num(line, NaN);
  if (!Number.isFinite(p) || !Number.isFinite(ln)) return NaN;
  if (market === "Total score" || market === "Total Score") {
    const sd = Math.max(0.5, num(opts.roundSd, 3.2));
    return ln + invNormalCdf(p) * sd;
  }
  const hi =
    market === "GIR" ? 17.99 : market === "Fairways hit" ? num(opts.fairwayHoles, 14) : 12;
  let lo = market === "GIR" || market === "Fairways hit" ? 0.01 : 0.01;
  let upper = Math.max(lo + 0.1, hi);
  for (let i = 0; i < 70; i++) {
    const mid = (lo + upper) / 2;
    const pm = probabilityOverAtMean(market, mid, ln, opts);
    if (pm < p) lo = mid;
    else upper = mid;
  }
  return (lo + upper) / 2;
}

export function bayesianPosteriorForProp({
  market,
  modelMean,
  prop,
  modelEvidenceWeight,
  roundSd,
  fairwayHoles,
}) {
  const modelMu = num(modelMean, NaN);
  const line = num(prop?.line, NaN);
  if (!Number.isFinite(modelMu) || !Number.isFinite(line)) return null;
  const prior = fairPriorFromProp(prop);
  if (!prior) return null;
  const distOpts = { roundSd, fairwayHoles };
  const priorMean = meanAtOverProbability(market, prior.fairOver, line, distOpts);
  if (!Number.isFinite(priorMean)) return null;
  const weight = clamp(
    num(modelEvidenceWeight, DEFAULT_MODEL_EVIDENCE_WEIGHT[market] ?? 0.3),
    0,
    1,
  );
  const posteriorMean = priorMean + weight * (modelMu - priorMean);
  const posteriorOver = probabilityOverAtMean(market, posteriorMean, line, distOpts);
  const availableOver = impliedProbFromAmerican(prop.over_odds);
  const availableUnder = impliedProbFromAmerican(prop.under_odds);
  return {
    market,
    line,
    prior_source: prior.source,
    prior_method: prior.method,
    prior_over_probability: prior.fairOver,
    prior_over_odds: americanFromProb(prior.fairOver),
    prior_mean: priorMean,
    model_mean: modelMu,
    model_evidence_weight: weight,
    posterior_mean: posteriorMean,
    posterior_over_probability: posteriorOver,
    posterior_under_probability: 1 - posteriorOver,
    posterior_over_odds: americanFromProb(posteriorOver),
    posterior_under_odds: americanFromProb(1 - posteriorOver),
    posterior_spread_over_pct:
      Number.isFinite(availableOver) ? (posteriorOver - availableOver) * 100 : NaN,
    posterior_spread_under_pct:
      Number.isFinite(availableUnder) ? (1 - posteriorOver - availableUnder) * 100 : NaN,
  };
}

export function defaultModelEvidenceWeight(market) {
  return DEFAULT_MODEL_EVIDENCE_WEIGHT[market] ?? 0.3;
}

const PROP_MARKET = {
  "Total Score": "Total score",
  "Total score": "Total score",
  Birdies: "Birdies",
  GIR: "GIR",
  "Fairways hit": "Fairways hit",
};

function priorPriority(prop) {
  const source = String(prop?.source || "").trim().toLowerCase();
  if (source === "gamedaymath" || source === "nostradamus") return 4;
  if (source === "sharp_consensus" || source === "sharp") return 3;
  if (source === "draftkings" || source === "csv") return 2;
  return 0;
}

function playerMarketMean(player, market) {
  if (market === "Total score") return num(player?.total_score, NaN);
  if (market === "Birdies") {
    const b = num(player?.birdies, NaN);
    const e = Math.max(0, num(player?.eagles, 0));
    return Number.isFinite(b) ? b + e : NaN;
  }
  if (market === "GIR") return num(player?.gir, NaN);
  if (market === "Fairways hit") return num(player?.fairways, NaN);
  return NaN;
}

function setPlayerMarketMean(player, market, mean, coursePar18) {
  const mu = num(mean, NaN);
  if (!Number.isFinite(mu)) return;
  if (market === "Total score") {
    player.total_score = Math.round(mu * 100) / 100;
    player.score_to_par = Math.round((mu - coursePar18) * 100) / 100;
    return;
  }
  if (market === "Birdies") {
    const e = Math.max(0, num(player.eagles, 0));
    player.birdies = Math.round(clamp(mu - e, 0.1, 8) * 100) / 100;
    const d = Math.max(0, num(player.doubles, 0));
    const bg = num(player.bogeys, NaN);
    if (Number.isFinite(bg)) {
      player.pars = Math.max(0.1, Math.round((18 - e - d - player.birdies - bg) * 100) / 100);
    }
    return;
  }
  if (market === "GIR") player.gir = Math.round(clamp(mu, 0, 18) * 100) / 100;
  if (market === "Fairways hit") {
    player.fairways = Math.round(clamp(mu, 0, 18) * 100) / 100;
  }
}

/**
 * Apply Bayesian market priors to player projection means wherever a real
 * sharp/book two-way prop exists. Rows without a non-circular prior stay pure model.
 */
export function applyBayesianMarketPosteriors(payload, opts = {}) {
  const players = Array.isArray(payload?.players) ? payload.players : [];
  const props = Array.isArray(payload?.props) ? payload.props : [];
  if (!players.length || !props.length) return { applied: false, rows: 0, markets: {} };

  const displayRound =
    Math.round(num(opts.displayRound ?? payload?.display_round ?? payload?.meta?.display_round, 1)) || 1;
  const coursePar18 = Math.round(num(payload?.course_par_18 ?? payload?.meta?.course_par_18, 72)) || 72;
  const fairwayHoles =
    Math.round(
      num(
        payload?.projection_course_basis?.fairway_holes_modeled ??
          payload?.meta?.projection_course_basis?.fairway_holes_modeled,
        14,
      ),
    ) || 14;

  const priorByKey = new Map();
  for (const prop of props) {
    const market = PROP_MARKET[String(prop?.market || "").trim()];
    if (!market || priorPriority(prop) <= 0 || !fairPriorFromProp(prop)) continue;
    const dg = Math.round(num(prop.dg_id, NaN));
    const round =
      Math.round(num(prop.round_num ?? prop.display_round, displayRound)) || displayRound;
    if (!Number.isFinite(dg) || round !== displayRound) continue;
    const key = `${dg}|${round}|${market}`;
    const prev = priorByKey.get(key);
    if (!prev || priorPriority(prop) > priorPriority(prev)) priorByKey.set(key, prop);
  }

  const markets = {};
  let rows = 0;
  for (const player of players) {
    const dg = Math.round(num(player?.dg_id, NaN));
    const round = Math.round(num(player?.round, displayRound)) || displayRound;
    if (!Number.isFinite(dg) || round !== displayRound) continue;
    for (const market of new Set(Object.values(PROP_MARKET))) {
      const prop = priorByKey.get(`${dg}|${round}|${market}`);
      if (!prop) continue;
      const modelMean = playerMarketMean(player, market);
      const posterior = bayesianPosteriorForProp({
        market,
        modelMean,
        prop,
        modelEvidenceWeight: opts.modelEvidenceWeights?.[market],
        roundSd: num(player.round_sd, 3.2),
        fairwayHoles,
      });
      if (!posterior) continue;
      setPlayerMarketMean(player, market, posterior.posterior_mean, coursePar18);
      if (!player.bayesian_market_posterior) player.bayesian_market_posterior = {};
      player.bayesian_market_posterior[market] = {
        line: posterior.line,
        prior_source: posterior.prior_source,
        prior_method: posterior.prior_method,
        prior_over_probability: Math.round(posterior.prior_over_probability * 10000) / 10000,
        prior_mean: Math.round(posterior.prior_mean * 1000) / 1000,
        raw_model_mean: Math.round(posterior.model_mean * 1000) / 1000,
        model_evidence_weight: posterior.model_evidence_weight,
        posterior_mean: Math.round(posterior.posterior_mean * 1000) / 1000,
        posterior_over_probability: Math.round(posterior.posterior_over_probability * 10000) / 10000,
        posterior_spread_over_pct: Math.round(posterior.posterior_spread_over_pct * 100) / 100,
        posterior_spread_under_pct: Math.round(posterior.posterior_spread_under_pct * 100) / 100,
      };
      markets[market] = (markets[market] || 0) + 1;
      rows++;
    }
  }

  if (rows > 0) {
    if (!payload.meta || typeof payload.meta !== "object") payload.meta = {};
    payload.meta.bayesian_market_calibration = {
      generated_at: new Date().toISOString(),
      method: "sharp_no_vig_prior_plus_rolling_course_model_evidence",
      display_round: displayRound,
      model_evidence_weights: { ...DEFAULT_MODEL_EVIDENCE_WEIGHT },
      markets,
    };
  }
  return { applied: rows > 0, rows, markets };
}
