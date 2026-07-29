/**
 * Backtest context-signal insights from detail CSV columns.
 */
import { num } from "./ev-math.mjs";

export const SIGNAL_COLS = [
  "weather_wind_mph",
  "weather_temp_f",
  "weather_condition",
  "weather_difficulty",
  "sg_ott",
  "sg_app",
  "tee_wave",
  "pin_sheet_active",
  "gir_minus_fw",
  "course_fw_width",
];

function roiGroup(bets, labelFn, minN = 10) {
  const g = new Map();
  for (const b of bets) {
    const lab = labelFn(b);
    if (!lab) continue;
    let a = g.get(lab);
    if (!a) a = { label: lab, n: 0, units: 0, wins: 0, losses: 0 };
    a.n++;
    a.units += b.pnl;
    const res = String(b.betRes || "").trim().toUpperCase();
    if (res === "W") a.wins++;
    else if (res === "L") a.losses++;
    g.set(lab, a);
  }
  return [...g.values()]
    .filter((a) => a.n >= minN)
    .map((a) => ({
      ...a,
      roi: a.n ? Math.round((a.units / a.n) * 1000) / 10 : NaN,
      hit: a.wins + a.losses ? Math.round((a.wins / (a.wins + a.losses)) * 1000) / 10 : NaN,
      units: Math.round(a.units * 100) / 100,
    }))
    .sort((a, b) => b.roi - a.roi);
}

function attachSignals(bet, row) {
  const wind = num(row.weather_wind_mph, NaN);
  const cond = String(row.weather_condition || "").toLowerCase();
  const pin = String(row.pin_sheet_active || "").trim();
  return {
    ...bet,
    weather_wind_mph: wind,
    weather_difficulty: num(row.weather_difficulty, NaN),
    weather_condition: cond,
    sg_ott: num(row.sg_ott, NaN),
    sg_app: num(row.sg_app, NaN),
    tee_wave: String(row.tee_wave || "").toLowerCase(),
    pin_sheet_active: pin === "1" ? 1 : pin === "0" ? 0 : NaN,
    gir_minus_fw: num(row.gir_minus_fw, NaN),
    course_fw_width: num(row.course_fw_width, NaN),
    rainy: cond.includes("rain") || cond.includes("drizzle") || cond.includes("storm"),
  };
}

function hasSignalCoverage(bets) {
  const n = bets.length;
  if (!n) return { weather: 0, sg: 0, tee: 0, pin: 0, course: 0 };
  let weather = 0;
  let sg = 0;
  let tee = 0;
  let pin = 0;
  let course = 0;
  for (const b of bets) {
    if (Number.isFinite(b.weather_wind_mph)) weather++;
    if (Number.isFinite(b.sg_ott) || Number.isFinite(b.sg_app)) sg++;
    if (b.tee_wave) tee++;
    if (b.pin_sheet_active === 0 || b.pin_sheet_active === 1) pin++;
    if (Number.isFinite(b.course_fw_width)) course++;
  }
  return { weather, sg, tee, pin, course, total: n };
}

function fmtPct(v) {
  if (!Number.isFinite(v)) return "—";
  return `${v >= 0 ? "+" : ""}${v.toFixed(1)}%`;
}

function insightFromGroup(label, rows, opts = {}) {
  const minRoi = opts.minRoi ?? 10;
  const maxRoi = opts.maxRoi ?? -5;
  const minN = opts.minN ?? 10;
  if (!rows?.length) return null;
  const best = rows[0];
  const worst = rows[rows.length - 1];
  if (best && best.roi >= minRoi && best.n >= minN) {
    return { tone: "", text: `${label}: ${best.label} — ${fmtPct(best.roi)} ROI on ${best.n} bets (+${best.units.toFixed(1)}u).` };
  }
  if (worst && worst.roi <= maxRoi && worst.n >= minN) {
    return { tone: "bad", text: `${label}: ${worst.label} — ${fmtPct(worst.roi)} ROI on ${worst.n} bets (fade or raise min EV).` };
  }
  return null;
}

/**
 * @param {object[]} qualifiedBets — explodeDetailToBets rows with qualified=true
 * @param {Record<string, string>[]} detailRows — raw CSV rows keyed by player-round
 */
export function buildEdgeSignalInsights(qualifiedBets, detailRows) {
  const byKey = new Map();
  for (const row of detailRows) {
    if (row.pricing_mode !== "default" || row.book_odds_source !== "pre_round_audit") continue;
    const k = `${row.event_name}\x1f${row.dg_id}|${row.round}`;
    byKey.set(k, row);
  }

  const bets = [];
  for (const b of qualifiedBets) {
    if (!b.qualified) continue;
    const k = `${b.event_name}\x1f${b.dg_id ?? ""}|${b.round}`;
    const row = byKey.get(k) || byKey.get(`${b.event_name}\x1f|${b.round}`);
    if (!row) {
      const alt = detailRows.find(
        (r) =>
          r.event_name === b.event_name &&
          String(r.round) === String(b.round) &&
          String(r.player_name) === String(b.player_name) &&
          r.pricing_mode === "default" &&
          r.book_odds_source === "pre_round_audit",
      );
      if (alt) bets.push(attachSignals(b, alt));
      continue;
    }
    bets.push(attachSignals(b, row));
  }

  const cov = hasSignalCoverage(bets);
  const insights = [];

  if (cov.total >= 5) {
    const parts = [];
    if (cov.weather) parts.push(`weather ${cov.weather}/${cov.total}`);
    if (cov.sg) parts.push(`SG ${cov.sg}/${cov.total}`);
    if (cov.tee) parts.push(`tee wave ${cov.tee}/${cov.total}`);
    if (cov.pin) parts.push(`pin sheet ${cov.pin}/${cov.total}`);
    if (cov.course) parts.push(`course FW width ${cov.course}/${cov.total}`);
    if (parts.length) {
      insights.push({
        tone: cov.weather < cov.total * 0.2 ? "warn" : "",
        text: `Context signals on qualified bets: ${parts.join(", ")}.`,
      });
    }
  }

  const fw = bets.filter((b) => b.market === "Fairways hit");
  const gir = bets.filter((b) => b.market === "GIR");
  const bird = bets.filter((b) => b.market === "Birdies");

  const pools = [
    ["Fairways — wind", fw, (b) => {
      if (!Number.isFinite(b.weather_wind_mph)) return null;
      if (b.weather_wind_mph <= 8) return "Calm (≤8 mph)";
      if (b.weather_wind_mph <= 12) return "Moderate (8–12 mph)";
      if (b.weather_wind_mph <= 18) return "Windy (12–18 mph)";
      return "Very windy (>18 mph)";
    }, 8],
    ["Fairways — course width", fw, (b) => {
      if (!Number.isFinite(b.course_fw_width)) return null;
      if (b.course_fw_width < 30) return "Narrow fairways (<30 yd)";
      if (b.course_fw_width < 34) return "Medium fairways (30–34 yd)";
      return "Wide fairways (≥34 yd)";
    }, 8],
    ["Fairways — GIR−FW profile", fw, (b) => {
      if (!Number.isFinite(b.gir_minus_fw)) return null;
      if (b.gir_minus_fw >= 3.5) return "Approach-heavy (GIR−FW ≥3.5)";
      if (b.gir_minus_fw <= 2.5) return "Driving-heavy (GIR−FW ≤2.5)";
      return "Balanced GIR−FW";
    }, 8],
    ["Fairways — tee wave", fw, (b) => {
      if (b.tee_wave === "morning") return "Morning wave";
      if (b.tee_wave === "afternoon") return "Afternoon wave";
      return null;
    }, 8],
    ["GIR — wind", gir, (b) => {
      if (!Number.isFinite(b.weather_wind_mph)) return null;
      if (b.weather_wind_mph <= 10) return "Calm/moderate (≤10 mph)";
      return "Windy (>10 mph)";
    }, 8],
    ["Birdies — weather", bird, (b) => {
      if (b.rainy) return "Rain / wet";
      if (Number.isFinite(b.weather_difficulty) && b.weather_difficulty <= -0.05) return "Easy scoring weather";
      if (Number.isFinite(b.weather_difficulty) && b.weather_difficulty >= 0.08) return "Hard scoring weather";
      return Number.isFinite(b.weather_wind_mph) ? "Neutral weather" : null;
    }, 8],
    ["Birdies — pin sheet", bird, (b) => {
      if (b.pin_sheet_active === 1) return "Pin sheet active";
      if (b.pin_sheet_active === 0) return "No pin sheet";
      return null;
    }, 8],
    ["All markets — SG OTT", bets, (b) => {
      if (!Number.isFinite(b.sg_ott)) return null;
      if (b.sg_ott >= 0.35) return "Strong OTT (sg_ott ≥0.35)";
      if (b.sg_ott <= -0.15) return "Weak OTT (sg_ott ≤−0.15)";
      return "Average OTT";
    }, 12],
    ["All markets — SG APP", bets, (b) => {
      if (!Number.isFinite(b.sg_app)) return null;
      if (b.sg_app >= 0.35) return "Strong APP (sg_app ≥0.35)";
      if (b.sg_app <= -0.15) return "Weak APP (sg_app ≤−0.15)";
      return "Average APP";
    }, 12],
  ];

  for (const [label, pool, fn, minN] of pools) {
    const row = insightFromGroup(label, roiGroup(pool, fn, minN), { minN });
    if (row) insights.push(row);
  }

  if (!cov.weather && bets.length >= 10) {
    insights.push({
      tone: "warn",
      text: "Weather columns empty — run npm run build:history and re-export round_projection_vs_actual.csv for wind/temp segments.",
    });
  }

  return insights;
}
