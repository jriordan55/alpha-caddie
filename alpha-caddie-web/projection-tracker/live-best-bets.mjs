/**
 * Live-week best bets: projections.json + DK props, ranked by EV and historical OOS/signals.
 */
import {
  capDirectionalPostedEdges,
  modelEdgePctAtLine,
  num,
  pickBetSide,
} from "./ev-math.mjs";

const PROJECTIONS_URL = "../projections.json";
const EDGE_SIGNALS_URL = "../data/edge_signal_scan.json";
const COURSE_TABLE_URL = "../data/course_table.csv";
const TOP_N = 15;

const MARKET_MODEL = {
  "Total score": (p) => num(p.total_score, NaN),
  Birdies: (p) => {
    const b = num(p.birdies, NaN);
    const e = num(p.eagles, 0);
    return Number.isFinite(b) ? b + (Number.isFinite(e) ? e : 0) : NaN;
  },
  GIR: (p) => num(p.gir, NaN),
  "Fairways hit": (p) => num(p.fairways, NaN),
};

const DK_TO_MARKET = {
  "Total Score": "Total score",
  Birdies: "Birdies",
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

function liveTargetRound(projections) {
  const meta = projections?.meta || {};
  const dr = Math.round(num(meta.display_round ?? projections.display_round, NaN));
  if (Number.isFinite(dr) && dr >= 1 && dr <= 4) return dr;
  const live = Math.round(num(meta.datagolf_field_current_round, NaN));
  if (Number.isFinite(live) && live >= 1 && live <= 4) return live;
  return 1;
}

function dkPropsForRound(projections, round) {
  const out = new Map();
  for (const r of Array.isArray(projections?.props) ? projections.props : []) {
    if (String(r.source || "").trim().toLowerCase() !== "draftkings") continue;
    const mkt = DK_TO_MARKET[String(r.market || "").trim()];
    if (!mkt) continue;
    const rnd = Math.round(num(r.round_num, NaN));
    if (Number.isFinite(rnd) && rnd >= 1 && rnd <= 4 && rnd !== round) continue;
    const line = num(r.line, NaN);
    const over = num(r.over_odds, NaN);
    const under = num(r.under_odds, NaN);
    const dg = Math.round(num(r.dg_id, NaN));
    if (!Number.isFinite(dg) || !Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) continue;
    out.set(`${dg}|${mkt}`, { line, over, under, player_name: r.player_name });
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

function venueAnchorNote(projections) {
  const b = projectionCourseBasis(projections);
  const years = Array.isArray(b.venue_scoring_years) ? b.venue_scoring_years : [];
  const stp = num(b.venue_avg_score_to_par, NaN);
  const bird = num(b.venue_avg_birdies, NaN);
  const parts = [];
  if (years.length) parts.push(`venue markets anchored to ${years.join(", ")}`);
  if (Number.isFinite(stp)) parts.push(`${stp >= 0 ? "+" : ""}${stp.toFixed(2)} vs par`);
  if (Number.isFinite(bird)) parts.push(`~${bird.toFixed(1)} birdies/rd`);
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

function marketFactor(market, oos) {
  const row = (oos?.by_market_at_5pct || []).find((m) => m.market === market);
  const roi = num(row?.roi_pct, NaN);
  if (!Number.isFinite(roi)) return 1;
  return clamp(0.8 + roi / 75, 0.88, 1.42);
}

/**
 * @param {object} opts
 * @param {object} opts.projections
 * @param {object|null} opts.oos
 * @param {object|null} opts.signals
 * @param {object|null} opts.courseRow
 * @param {number} opts.minEvPct
 */
export function buildLiveBestBets({ projections, oos, signals, courseRow, minEvPct }) {
  const round = liveTargetRound(projections);
  const dk = dkPropsForRound(projections, round);
  const players = playersForRound(projections, round);
  const pinActive = pinSheetActive(projections, round);
  const minEdge = minEvPct;

  /** @type {object[]} */
  const candidates = [];
  for (const [key, prop] of dk.entries()) {
    const [dgStr, market] = key.split("|");
    const dg = Number(dgStr);
    const player = players.get(dg);
    if (!player) continue;
    const muFn = MARKET_MODEL[market];
    if (!muFn) continue;
    const mu = muFn(player);
    if (!Number.isFinite(mu)) continue;
    let { edgeOver, edgeUnder } = modelEdgePctAtLine(market, mu, prop.line, prop.over, prop.under);
    ({ edgeOver, edgeUnder } = capDirectionalPostedEdges(edgeOver, edgeUnder, mu, prop.line));
    const pick = pickBetSide(edgeOver, edgeUnder, minEdge, mu, prop.line);
    if (!pick) continue;
    const side = pick.side;
    const edgePct = pick.edge;
    const labels = contextLabels(market, side, edgePct, player, courseRow, pinActive, projections);
    const { boost, tags } = signalBoost(market, labels, signals);
    const mFac = marketFactor(market, oos);
    const score = edgePct * mFac * boost;
    const mktHist = (oos?.by_market_at_5pct || []).find((m) => m.market === market);
    candidates.push({
      player_name: player.player_name || prop.player_name,
      dg_id: dg,
      round,
      market,
      side,
      mu,
      line: prop.line,
      odds: side === "over" ? prop.over : prop.under,
      edgePct,
      score,
      histRoi: num(mktHist?.roi_pct, NaN),
      histBets: Math.round(num(mktHist?.bets, NaN)) || 0,
      contextTags: tags,
    });
  }

  const byKey = new Map();
  for (const c of candidates) {
    const k = `${c.dg_id}|${c.market}`;
    const prev = byKey.get(k);
    if (!prev || c.edgePct > prev.edgePct) byKey.set(k, c);
  }

  return {
    round,
    roundLabel: String(projections?.meta?.display_round_label || projections?.display_round_label || "").trim() || `R${round}`,
    eventName: String(projections?.event_name || projections?.meta?.event_name || "").trim(),
    updatedAt: String(projections?.updated_at || projections?.meta?.updated_at || "").trim(),
    venueNote: venueAnchorNote(projections),
    picks: [...byKey.values()].sort((a, b) => b.score - a.score || b.edgePct - a.edgePct).slice(0, TOP_N),
  };
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
