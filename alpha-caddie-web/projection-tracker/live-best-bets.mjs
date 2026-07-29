/**
 * Live-week best bets: projections.json + DK props, ranked by EV and historical OOS/signals.
 */
import {
  capDirectionalPostedEdges,
  impliedProbFromAmerican,
  modelEdgePctAtLine,
  modelEdgeVsFairAtLine,
  num,
  pickBetSide,
} from "./ev-math.mjs";
import {
  isActionableMarket,
  qualifiesBet,
} from "./bet-policy.mjs";
import { ouProjectedMeanForLive } from "../scripts/projected-mean-live.mjs";
import { buildBookPropsIndex } from "../scripts/projection-book-props.mjs";
import {
  buildLiveProjectionFactorsSummary,
  courseTailoringTags,
} from "./projection-factors-panel.mjs";
import { priceSidesAgainstBook } from "./win-prob-calibration.mjs";
const PROJECTIONS_URL = "../projections.json";
const EDGE_SIGNALS_URL = "../data/edge_signal_scan.json";
const COURSE_TABLE_URL = "../data/course_table.csv";
const TOP_N = 15;

const MARKET_MODEL = {
  "Total score": (p, meta) => ouProjectedMeanForLive("Total score", p, meta),
  Birdies: (p, meta) => ouProjectedMeanForLive("Birdies", p, meta),
  Bogeys: (p, meta) => ouProjectedMeanForLive("Bogeys", p, meta),
  GIR: (p, meta) => ouProjectedMeanForLive("GIR", p, meta),
  "Fairways hit": (p, meta) => ouProjectedMeanForLive("Fairways hit", p, meta),
};

const DK_TO_MARKET = {
  "Total Score": "Total score",
  Birdies: "Birdies",
  Bogeys: "Bogeys",
  GIR: "GIR",
  "Fairways hit": "Fairways hit",
};

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function normCourseKey(raw) {
  return String(raw || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, " ")
    .trim();
}

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (const ch of line) {
    if (ch === '"') {
      q = !q;
      continue;
    }
    if (ch === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += ch;
  }
  out.push(cur);
  return out;
}

async function loadCourseTableRow(courseName) {
  try {
    const res = await fetch(`${COURSE_TABLE_URL}?t=${Date.now()}`, { cache: "no-store" });
    if (!res.ok) return null;
    const text = await res.text();
    const lines = text.split(/\r?\n/).filter(Boolean);
    if (lines.length < 2) return null;
    const header = parseCsvLine(lines[0]);
    const iCourse = header.indexOf("course");
    if (iCourse < 0) return null;
    const want = normCourseKey(courseName);
    for (let i = 1; i < lines.length; i++) {
      const cells = parseCsvLine(lines[i]);
      const ck = normCourseKey(cells[iCourse]);
      if (ck === want || ck.includes(want) || want.includes(ck)) {
        const row = {};
        for (let j = 0; j < header.length; j++) row[header[j]] = cells[j] ?? "";
        return row;
      }
    }
  } catch {
    /* optional */
  }
  return null;
}

function snapHalfLine(x) {
  const v = num(x, NaN);
  if (!Number.isFinite(v)) return NaN;
  return Math.round(v - 0.5) + 0.5;
}

/** When DK scrape fails, use model half-lines so the live tab still lists the current field. */
function supplementModelProjectionProps(dk, players, meta, round) {
  for (const [market, muFn] of Object.entries(MARKET_MODEL)) {
    for (const player of players.values()) {
      const dg = Math.round(num(player.dg_id, NaN));
      if (!Number.isFinite(dg)) continue;
      const key = `${dg}|${market}`;
      if (dk.has(key)) continue;
      const mu = muFn.length >= 2 ? muFn(player, meta) : muFn(player);
      if (!Number.isFinite(mu)) continue;
      let line = market === "Total score" ? mu : snapHalfLine(mu);
      if (market === "Total score") line = Math.min(85.5, Math.max(63.5, line));
      else line = Math.min(8.5, Math.max(0.5, line));
      dk.set(key, {
        line,
        over: -110,
        under: -110,
        player_name: player.player_name,
        source: "model_projection",
      });
    }
  }
}
function liveTargetRound(projections) {
  const meta = projections?.meta || {};
  const dr = Math.round(num(meta.display_round ?? projections.display_round, NaN));
  if (Number.isFinite(dr) && dr >= 1 && dr <= 4) return dr;
  const live = Math.round(num(meta.datagolf_field_current_round, NaN));
  if (Number.isFinite(live) && live >= 1 && live <= 4) return live;
  return 1;
}

function dkPropsForRound(projections, round) {
  const index = buildBookPropsIndex(projections, { round });
  const out = new Map();
  for (const [key, row] of index) {
    const [dgStr, , marketRaw] = key.split("|");
    const mkt = DK_TO_MARKET[marketRaw] || marketRaw;
    if (!MARKET_MODEL[mkt]) continue;
    out.set(`${dgStr}|${mkt}`, {
      line: row.line,
      over: row.over,
      under: row.under,
      player_name: row.player_name,
      source: row.source,
    });
  }
  return out;
}

function playersForRound(projections, round) {
  const byDg = new Map();
  for (const p of Array.isArray(projections?.players) ? projections.players : []) {
    const pr = Math.round(num(p.round, NaN));
    if (pr !== round) continue;
    const dg = Math.round(num(p.dg_id, NaN));
    if (!Number.isFinite(dg)) continue;
    byDg.set(dg, p);
  }
  return byDg;
}

function pinSheetActive(projections, round) {
  const ps = projections?.meta?.pin_sheet;
  if (!ps || typeof ps !== "object") return false;
  return Math.round(num(ps.round, NaN)) === round;
}

function projectionCourseBasis(projections) {
  return projections?.meta?.projection_course_basis || projections?.projection_course_basis || {};
}

function projectionFactorsNote(projections) {
  const meta = projections?.meta || projections || {};
  const parts = [];
  if (meta.projection_counts_weather_baked) {
    const rnd = Math.round(num(meta.projection_counts_weather_baked_round, NaN));
    parts.push(
      Number.isFinite(rnd) && rnd >= 1
        ? `weather baked into R${rnd} counts`
        : "weather baked into counts",
    );
  } else {
    parts.push("weather applied at pricing time");
  }
  if (meta.pin_sheet && typeof meta.pin_sheet === "object") {
    parts.push("pin sheet active");
  }
  if (meta.in_play_affects_round_odds === true) {
    parts.push("in-round scratch on");
  }
  const sgImp = projectionCourseBasis(projections).course_sg_importance;
  if (sgImp?.dominant_sg) {
    const label = String(sgImp.dominant_sg).replace("sg_", "").toUpperCase();
    parts.push(`course weights ${label}`);
  }
  const unified = meta.projection_unified_factors;
  if (meta.projection_round_adjustments?.unified_factors_applied) {
    parts.push("unified factors baked");
  }
  const teeDelta = num(unified?.tee_wave_bias?.deltaAfternoonMinusMorning, NaN);
  if (Number.isFinite(teeDelta) && Math.abs(teeDelta) >= 0.04) {
    parts.push(`tee wave ${teeDelta >= 0 ? "+" : ""}${teeDelta.toFixed(2)} stp PM`);
  }
  return parts.join(" · ");
}

function venueAnchorNote(projections) {
  const b = projectionCourseBasis(projections);
  const years = Array.isArray(b.venue_scoring_years) ? b.venue_scoring_years : [];
  const stp = num(b.venue_avg_score_to_par, NaN);
  const bird = num(b.venue_avg_birdies, NaN);
  const parts = [];
  if (years.length) parts.push(`venue markets anchored to ${years.join(", ")}`);
  if (Number.isFinite(stp)) parts.push(`${stp >= 0 ? "+" : ""}${stp.toFixed(2)} vs par`);
  if (Number.isFinite(bird) && bird >= 1.85) parts.push(`~${bird.toFixed(1)} birdies/rd`);
  return parts.join(" · ");
}

function contextLabels(market, side, edgePct, player, courseRow, pinActive, projections) {
  const labels = [];
  const wind = num(player?.weather_wind_mph, NaN);
  const cond = String(player?.weather_condition || "").toLowerCase();
  const rainy = cond.includes("rain") || cond.includes("drizzle") || cond.includes("storm");
  const tee = String(player?.dg_tee_wave || "").trim().toLowerCase();
  const sgOtt = num(player?.sg_ott, NaN);
  const gir = num(player?.gir, NaN);
  const fw = num(player?.fairways, NaN);
  const fwWidth = courseRow ? num(courseRow.fw_width, NaN) : NaN;
  const adjAcc = courseRow ? num(courseRow.adj_driving_accuracy, NaN) : NaN;
  const basis = projectionCourseBasis(projections);
  const venueStp = num(basis.venue_avg_score_to_par, NaN);
  const scoreAdj = Number.isFinite(venueStp)
    ? venueStp
    : courseRow
      ? num(courseRow.adj_score_to_par, NaN)
      : NaN;
  const girMinusFw = Number.isFinite(gir) && Number.isFinite(fw) ? gir - fw : NaN;

  if (Number.isFinite(wind)) {
    if (wind <= 8) labels.push("Calm wind (≤8 mph)");
    else if (wind <= 12) labels.push("Moderate (8–12 mph)");
    else if (wind <= 18) labels.push("Windy (12–18 mph)");
    else labels.push("Very windy (>18 mph)");
  }
  if (rainy) labels.push("Rain / soft");
  else if (Number.isFinite(wind)) labels.push("Dry");
  if (tee === "morning") labels.push("Morning wave");
  if (tee === "afternoon") labels.push("Afternoon wave");
  if (pinActive) labels.push("Pin sheet active");
  if (Number.isFinite(fwWidth)) {
    if (fwWidth < 30) labels.push("Narrow FW (<30 yd avg)");
    else if (fwWidth < 34) labels.push("Medium FW (30–34 yd)");
    else labels.push("Wide FW (≥34 yd)");
  }
  if (Number.isFinite(adjAcc)) {
    if (adjAcc < 0.55) labels.push("Hard driving course (adj acc <55%)");
    else if (adjAcc < 0.62) labels.push("Average driving course");
    else labels.push("Easy driving course (adj acc ≥62%)");
  }
  if (Number.isFinite(scoreAdj)) {
    if (scoreAdj > 1.2) labels.push("Hard vs par (adj >+1.2)");
    else if (scoreAdj < 0.5) labels.push("Easy vs par (adj <+0.5)");
    else labels.push("Neutral scoring course");
  }
  if (market === "Fairways hit" && Number.isFinite(girMinusFw)) {
    if (girMinusFw >= 3.5) labels.push("High GIR−FW (approach > driving)");
    else if (girMinusFw <= 2.5) labels.push("Low GIR−FW (driving profile)");
    else labels.push("Balanced GIR−FW");
  }
  if (Number.isFinite(sgOtt)) {
    if (sgOtt >= 0.35) labels.push("Strong OTT (≥0.35)");
    else if (sgOtt <= -0.15) labels.push("Weak OTT (≤−0.15)");
    else labels.push("Average OTT");
  }
  for (const tag of courseTailoringTags(player)) labels.push(tag);
  if (edgePct >= 15) labels.push("Edge ≥15%");
  else if (edgePct >= 10) labels.push("Edge 10–15%");
  else labels.push("Edge 5–10%");
  labels.push(`${market} ${side}`);
  return labels;
}

function signalBoost(market, labels, signals) {
  const scan = signals?.minEv5?.[market];
  let boost = 1;
  /** @type {string[]} */
  const tags = [];
  const pools = scan
    ? [
        scan.byWind,
        scan.byRain,
        scan.byTeeWave,
        scan.byPinSheet,
        scan.bySgOtt,
        scan.byCourseFwWidth,
        scan.byCourseDrivingDifficulty,
        scan.byCourseScoring,
        scan.byGirFwSkillGap,
        scan.byGirMinusFw,
        scan.byEdgeTier,
        scan.bySide,
      ]
    : [];
  for (const pool of pools) {
    if (!Array.isArray(pool)) continue;
    for (const lab of labels) {
      const hit = pool.find((r) => r.label === lab);
      if (!hit || hit.n < 8 || !Number.isFinite(hit.roi)) continue;
      boost *= 1 + clamp(hit.roi / 200, -0.22, 0.32);
      if (hit.roi >= 8) tags.push(`+${hit.roi.toFixed(0)}% ${lab}`);
      else if (hit.roi <= -5) tags.push(`${hit.roi.toFixed(0)}% ${lab}`);
    }
  }
  for (const t of signals?.actionable || []) {
    if (t.market !== market && t.market !== "all") continue;
    const parts = String(t.signal || "").split(": ");
    const sigLabel = parts.length > 1 ? parts.slice(1).join(": ") : parts[0];
    if (!labels.includes(sigLabel)) continue;
    if (String(t.action || "").includes("fade")) {
      boost *= 0.78;
      tags.push(`fade ${sigLabel}`);
    } else {
      boost *= 1.1;
      tags.push(`lean ${sigLabel}`);
    }
  }
  return { boost: clamp(boost, 0.55, 1.55), tags: [...new Set(tags)].slice(0, 4) };
}

function courseFwWidth(courseRow) {
  if (!courseRow) return NaN;
  return num(courseRow.fw_width, num(courseRow.avg_fairway_width, num(courseRow.fairway_width, NaN)));
}

function marketFactor(market, oos) {
  const row = (oos?.by_market_at_5pct || []).find((m) => m.market === market);
  const roi = num(row?.roi_pct, NaN);
  if (!Number.isFinite(roi)) return 1;
  if (roi < 0) return 0.72;
  return clamp(0.85 + roi / 60, 0.88, 1.45);
}

/**
 * @param {object} opts
 * @param {object} opts.projections
 * @param {object|null} opts.oos
 * @param {object|null} opts.signals
 * @param {object|null} opts.courseRow
 * @param {number} opts.minEvPct
 * @param {string} [opts.marketFilter]
 */
export function buildLiveBestBets({ projections, oos, signals, courseRow, minEvPct, marketFilter = "" }) {
  const round = liveTargetRound(projections);
  const dk = dkPropsForRound(projections, round);
  const players = playersForRound(projections, round);
  supplementModelProjectionProps(dk, players, projections?.meta || projections || {}, round);
  const eventName = String(projections?.event_name || projections?.meta?.event_name || "");
  const pinActive = pinSheetActive(projections, round);
  const minEdge = minEvPct;
  const meta = projections?.meta || projections || {};
  const fairwayHoles =
    Math.round(num(meta?.projection_course_basis?.fairway_holes_modeled, NaN)) || 14;

  /** @type {object[]} */
  const candidates = [];
  for (const [key, prop] of dk.entries()) {
    const [dgStr, market] = key.split("|");
    const dg = Number(dgStr);
    const player = players.get(dg);
    if (!player) continue;
    const muFn = MARKET_MODEL[market];
    if (!muFn) continue;
    const mu = muFn.length >= 2 ? muFn(player, meta) : muFn(player);
    if (!Number.isFinite(mu)) continue;
    const girMinusFw = num(player.gir, NaN) - num(player.fairways, NaN);
    const betContext = {
      gir_minus_fw: girMinusFw,
      course_fw_width: courseFwWidth(courseRow),
      round,
    };
    if (!isActionableMarket(market)) continue;
    if (
      !qualifiesBet({
        market,
        modelLine: mu,
        bookLine: prop.line,
        context: betContext,
        eventName,
      })
    ) {
      continue;
    }
    const fair = modelEdgeVsFairAtLine(market, mu, prop.line, prop.over, prop.under, 1, fairwayHoles);
    const priced = priceSidesAgainstBook({
      market,
      pRawOver: fair.pOver,
      fairOver: fair.fairOver,
      fairUnder: fair.fairUnder,
      postedOver: impliedProbFromAmerican(prop.over),
      postedUnder: impliedProbFromAmerican(prop.under),
    });
    let edgeOver = priced.confEdgeOver;
    let edgeUnder = priced.confEdgeUnder;
    if (!Number.isFinite(edgeOver) || !Number.isFinite(edgeUnder)) {
      edgeOver = fair.edgeFairOver;
      edgeUnder = fair.edgeFairUnder;
    }
    if (!Number.isFinite(edgeOver) || !Number.isFinite(edgeUnder)) {
      const posted = modelEdgePctAtLine(market, mu, prop.line, prop.over, prop.under, 1, fairwayHoles);
      edgeOver = posted.edgeOver;
      edgeUnder = posted.edgeUnder;
    }
    ({ edgeOver, edgeUnder } = capDirectionalPostedEdges(edgeOver, edgeUnder, mu, prop.line));
    const pick = pickBetSide(edgeOver, edgeUnder, minEdge, mu, prop.line);
    if (!pick) continue;
    if (
      !qualifiesBet({
        market,
        modelLine: mu,
        bookLine: prop.line,
        context: betContext,
        eventName,
        side: pick.side,
      })
    ) {
      continue;
    }
    const side = pick.side;
    const edgePct = pick.edge;
    const confP = side === "over" ? priced.pCalOver : priced.pCalUnder;
    const fairP = side === "over" ? priced.fairOver : priced.fairUnder;
    const labels = contextLabels(market, side, edgePct, player, courseRow, pinActive, projections);
    const { boost, tags } = signalBoost(market, labels, signals);
    const tailoringTags = courseTailoringTags(player);
    const mFac = marketFactor(market, oos);
    const score = edgePct * mFac * boost;
    const mktHist = (oos?.by_market_at_5pct || []).find((m) => m.market === market);
    const gap = Number.isFinite(mu) && Number.isFinite(prop.line) ? mu - prop.line : NaN;
    candidates.push({
      player_name: player.player_name || prop.player_name,
      dg_id: dg,
      round,
      market,
      side,
      mu,
      gap,
      line: prop.line,
      odds: side === "over" ? prop.over : prop.under,
      edgePct,
      confP,
      fairP,
      score,
      histRoi: num(mktHist?.roi_pct, NaN),
      histBets: Math.round(num(mktHist?.bets, NaN)) || 0,
      contextTags: [...tailoringTags, ...tags],
      tailoringTags,
    });
  }

  const byKey = new Map();
  for (const c of candidates) {
    const k = `${c.dg_id}|${c.market}`;
    const prev = byKey.get(k);
    if (!prev || c.edgePct > prev.edgePct) byKey.set(k, c);
  }

  let picks = [...byKey.values()];
  const mkt = String(marketFilter || "").trim();
  if (mkt) {
    picks = picks.filter((p) => p.market === mkt);
    picks = picks.sort((a, b) => b.score - a.score || b.edgePct - a.edgePct).slice(0, TOP_N);
  } else {
    /** @type {Map<string, object[]>} */
    const byMarket = new Map();
    for (const p of picks) {
      if (!byMarket.has(p.market)) byMarket.set(p.market, []);
      byMarket.get(p.market).push(p);
    }
    const slots = Math.max(2, Math.ceil(TOP_N / Math.max(1, byMarket.size)));
    picks = [...byMarket.entries()]
      .flatMap(([, arr]) => arr.sort((a, b) => b.score - a.score || b.edgePct - a.edgePct).slice(0, slots))
      .sort((a, b) => b.score - a.score || b.edgePct - a.edgePct)
      .slice(0, TOP_N);
  }

  return {
    round,
    roundLabel: String(projections?.meta?.display_round_label || projections?.display_round_label || "").trim() || `R${round}`,
    eventName: String(projections?.event_name || projections?.meta?.event_name || "").trim(),
    updatedAt: String(projections?.updated_at || projections?.meta?.updated_at || "").trim(),
    venueNote: venueAnchorNote(projections),
    factorsNote: projectionFactorsNote(projections),
    factorsSummary: buildLiveProjectionFactorsSummary(projections),
    modelLinesOnly: ![...dk.values()].some((p) => String(p.source || "").toLowerCase() === "draftkings"),
    picks,
  };
}

const JOURNAL_MARKET_ORDER = [
  "Total score",
  "Birdies",
  "Bogeys",
  "GIR",
  "Fairways hit",
];

function journalMarketSortKey(m) {
  const i = JOURNAL_MARKET_ORDER.indexOf(m);
  return i >= 0 ? i : 99;
}

/**
 * All DraftKings round O/U sides for the live week (no EV filter).
 * @param {object} projections
 */
export function buildAllLiveDkBetOptions(projections) {
  const round = liveTargetRound(projections);
  const dk = dkPropsForRound(projections, round);
  const players = playersForRound(projections, round);
  const eventName = String(projections?.event_name || projections?.meta?.event_name || "").trim();
  const roundLabel =
    String(projections?.meta?.display_round_label || projections?.display_round_label || "").trim() ||
    `R${round}`;
  const modelLinesOnly = ![...dk.values()].some((p) => String(p.source || "").toLowerCase() === "draftkings");

  /** @type {object[]} */
  const options = [];

  for (const [key, prop] of dk.entries()) {
    const [dgStr, market] = key.split("|");
    const dg = Number(dgStr);
    const player = players.get(dg);
    if (!player) continue;
    if (!isActionableMarket(market)) continue;
    if (!Number.isFinite(prop.line)) continue;
    const playerName = String(player.player_name || prop.player_name || "").trim();

    for (const side of ["over", "under"]) {
      const odds = side === "over" ? prop.over : prop.under;
      if (!Number.isFinite(odds)) continue;
      options.push({
        lineKey: `${dg}|${market}|${side}`,
        eventName,
        round,
        dg_id: dg,
        playerName,
        opponentName: "",
        market,
        side,
        line: prop.line,
        odds: Math.round(odds),
        pickType: "ou",
      });
    }
  }

  options.sort((a, b) => {
    const mk = journalMarketSortKey(a.market) - journalMarketSortKey(b.market);
    if (mk) return mk;
    const pn = a.playerName.localeCompare(b.playerName);
    if (pn) return pn;
    return String(a.side).localeCompare(String(b.side));
  });

  return { round, roundLabel, eventName, options, modelLinesOnly };
}

let _ctx = null;
let _ctxLoad = null;

export async function loadLiveBestBetsContext() {
  if (_ctx) return _ctx;
  if (!_ctxLoad) {
    _ctxLoad = (async () => {
      const [projRes, sigRes, oosRes] = await Promise.all([
        fetch(`${PROJECTIONS_URL}?t=${Date.now()}`, { cache: "no-store" }),
        fetch(`${EDGE_SIGNALS_URL}?t=${Date.now()}`, { cache: "no-store" }),
        fetch(`../data/walkforward_oos_roi.json?t=${Date.now()}`, { cache: "no-store" }),
      ]);
      const projections = projRes.ok ? await projRes.json() : null;
      const signals = sigRes.ok ? await sigRes.json() : null;
      const oos = oosRes.ok ? await oosRes.json() : null;
      const courseName = String(projections?.course_used || projections?.meta?.course_used || "").trim();
      const courseRow = courseName ? await loadCourseTableRow(courseName) : null;
      _ctx = { projections, signals, oos, courseRow };
      return _ctx;
    })();
  }
  return _ctxLoad;
}

export function invalidateLiveBestBetsCache() {
  _ctx = null;
  _ctxLoad = null;
}
