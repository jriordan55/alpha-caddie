/**
 * Patch tracker detail rows with fresher live-week actuals from projections.json
 * and pgatour_event_rounds.json (without re-running the full CSV export).
 */
import { eventsLikelySame } from "../scripts/dg-events-align.mjs";
import { ouProjectedMeanForLive } from "../scripts/projected-mean-live.mjs";

function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function normEvent(s) {
  return String(s || "")
    .trim()
    .toLowerCase();
}

function birdiesFromAct(act) {
  if (!act || typeof act !== "object") return NaN;
  const b = num(act.birdies);
  const eob = num(act.eagles_or_better);
  const eg = num(act.eagles);
  const eagleAdd = Number.isFinite(eob) ? eob : Number.isFinite(eg) ? eg : 0;
  if (!Number.isFinite(b) && !Number.isFinite(eob) && !Number.isFinite(eg)) return NaN;
  return (Number.isFinite(b) ? b : 0) + eagleAdd;
}

function fmtActual(marketKey, v) {
  if (!Number.isFinite(v)) return "";
  if (marketKey === "total") return (Math.round(v * 10) / 10).toFixed(1);
  return String(Math.round(v * 10) / 10 === Math.round(v) ? Math.round(v) : v);
}

function ouSideResults(actual, line) {
  if (!Number.isFinite(actual) || !Number.isFinite(line)) return { over: "", under: "" };
  if (actual > line) return { over: "W", under: "L" };
  if (actual < line) return { over: "L", under: "W" };
  return { over: "P", under: "P" };
}

const MARKET_ACTUAL = [
  { market: "Total score", key: "total", actualCol: "actual_round_score", modelCol: "round_score_line", overCol: "round_score_over", underCol: "round_score_under" },
  { market: "Birdies", key: "birdies", actualCol: "actual_birdies", modelCol: "birdies_line", overCol: "birdies_over", underCol: "birdies_under" },
  { market: "Bogeys", key: "bogeys", actualCol: "actual_bogeys", modelCol: "bogeys_line", overCol: "bogeys_over", underCol: "bogeys_under" },
  { market: "GIR", key: "gir", actualCol: "actual_gir", modelCol: "gir_line", overCol: "gir_over", underCol: "gir_under" },
  { market: "Fairways hit", key: "fairways", actualCol: "actual_fairways", modelCol: "fairways_line", overCol: "fairways_over", underCol: "fairways_under" },
];

function actualForMarket(act, marketKey) {
  if (!act || typeof act !== "object") return NaN;
  if (marketKey === "total") return num(act.round_score ?? act.total_score);
  if (marketKey === "birdies") return birdiesFromAct(act);
  if (marketKey === "bogeys") return num(act.bogeys ?? act.bogies);
  if (marketKey === "gir") return num(act.gir);
  if (marketKey === "fairways") return num(act.fairways);
  return NaN;
}

/** @param {Map<string, object>} out */
function overlayLiveActuals(out, projections, eventName) {
  const live = projections?.live_round_actuals_by_dg;
  if (!live || typeof live !== "object") return 0;
  let n = 0;
  for (const [dgKey, perRound] of Object.entries(live)) {
    const dg = Math.round(num(dgKey));
    if (!Number.isFinite(dg) || !perRound || typeof perRound !== "object") continue;
    for (const [rndKey, act] of Object.entries(perRound)) {
      const rnd = Math.round(num(rndKey));
      if (!Number.isFinite(rnd) || !act) continue;
      const score = num(act.round_score);
      if (!Number.isFinite(score) || score <= 0) continue;
      const key = `${dg}|${rnd}`;
      const prev = out.get(key) || {};
      out.set(key, {
        ...prev,
        ...act,
        round_score: score,
        total_score: score,
        source: act.source || prev.source || "live_projections",
      });
      n++;
    }
  }
  return n;
}

/** @param {Map<string, object>} out */
function overlayPgatourActuals(out, pgPayload, eventName) {
  const rounds = Array.isArray(pgPayload?.rounds) ? pgPayload.rounds : [];
  const metaEvent = String(pgPayload?.meta?.event_name || "").trim();
  if (metaEvent && !eventsLikelySame(eventName, metaEvent)) return 0;
  let n = 0;
  for (const r of rounds) {
    const dg = Math.round(num(r.dg_id));
    const rnd = Math.round(num(r.round_num));
    const score = num(r.round_score);
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || !Number.isFinite(score) || score <= 0) continue;
    const key = `${dg}|${rnd}`;
    const prev = out.get(key) || {};
    out.set(key, {
      ...prev,
      round_score: score,
      total_score: score,
      birdies: num(r.birdies),
      bogeys: num(r.bogies ?? r.bogies),
      gir: num(r.gir),
      fairways: num(r.fairways),
      eagles_or_better: num(r.eagles_or_better),
      source: "pgatour_event_rounds",
    });
    n++;
  }
  return n;
}

function parseLine(v) {
  const s = String(v ?? "").trim();
  if (!s) return NaN;
  const n = Number(s);
  return Number.isFinite(n) ? n : NaN;
}

function fmtModelLine(market, mu) {
  if (!Number.isFinite(mu)) return "";
  if (market === "Total score") return (Math.round(mu * 10) / 10).toFixed(1);
  return String(Math.round(mu * 10) / 10);
}

function modelMuForMarket(market, player, meta) {
  return ouProjectedMeanForLive(market, player, meta);
}

function patchModelLinesFromProjections(detailRows, projections) {
  if (!projections || !Array.isArray(detailRows) || !detailRows.length) return detailRows;
  const eventName = String(projections?.event_name || projections?.meta?.event_name || "").trim();
  if (!eventName) return detailRows;
  const meta = projections?.meta || projections || {};
  const players = Array.isArray(projections?.players) ? projections.players : [];
  /** @type {Map<string, object>} */
  const byKey = new Map();
  for (const p of players) {
    const dg = Math.round(num(p?.dg_id));
    const rnd = Math.round(num(p?.round));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd)) continue;
    byKey.set(`${dg}|${rnd}`, p);
  }
  if (!byKey.size) return detailRows;

  let patched = 0;
  for (let i = 0; i < detailRows.length; i++) {
    const row = detailRows[i];
    if (!eventsLikelySame(row.event_name, eventName)) continue;
    if (String(row.pricing_mode || "") !== "default") continue;
    if (String(row.pricing_skill || "") !== "default") continue;
    const dg = Math.round(num(row.dg_id));
    const rnd = Math.round(num(row.round));
    const player = byKey.get(`${dg}|${rnd}`);
    if (!player) continue;

    let changed = false;
    const next = { ...row };
    for (const spec of MARKET_ACTUAL) {
      const mu = modelMuForMarket(spec.market, player, meta);
      if (!Number.isFinite(mu)) continue;
      const formatted = fmtModelLine(spec.market, mu);
      if (next[spec.modelCol] !== formatted) {
        next[spec.modelCol] = formatted;
        changed = true;
      }
      const actual = parseLine(next[spec.actualCol]);
      if (Number.isFinite(actual)) {
        const sides = ouSideResults(actual, mu);
        if (next[spec.overCol] !== sides.over) {
          next[spec.overCol] = sides.over;
          changed = true;
        }
        if (next[spec.underCol] !== sides.under) {
          next[spec.underCol] = sides.under;
          changed = true;
        }
      }
    }
    if (changed) {
      detailRows[i] = next;
      patched++;
    }
  }
  if (patched > 0) {
    console.log(`[projection-tracker] Refreshed model lines on ${patched} live-week row(s) from projections.json`);
  }
  return detailRows;
}

/**
 * @param {Record<string, string>[]} detailRows
 * @param {object | null} projections
 * @param {object | null} pgPayload
 */
export function patchDetailRowsFromLiveSources(detailRows, projections, pgPayload) {
  if (!Array.isArray(detailRows) || !detailRows.length) return detailRows;
  patchModelLinesFromProjections(detailRows, projections);
  const eventName = String(projections?.event_name || projections?.meta?.event_name || pgPayload?.meta?.event_name || "").trim();
  if (!eventName) return detailRows;

  /** @type {Map<string, object>} */
  const actuals = new Map();
  overlayLiveActuals(actuals, projections, eventName);
  overlayPgatourActuals(actuals, pgPayload, eventName);
  if (!actuals.size) return detailRows;

  /** @type {Map<string, number>} */
  const rowIdx = new Map();
  for (let i = 0; i < detailRows.length; i++) {
    const row = detailRows[i];
    if (!eventsLikelySame(row.event_name, eventName)) continue;
    const dg = Math.round(num(row.dg_id));
    const rnd = Math.round(num(row.round));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd)) continue;
    rowIdx.set(`${dg}|${rnd}`, i);
  }

  let patched = 0;
  for (const [key, act] of actuals.entries()) {
    const idx = rowIdx.get(key);
    if (idx === undefined) continue;
    const row = { ...detailRows[idx] };
    let changed = false;

    for (const spec of MARKET_ACTUAL) {
      const actual = actualForMarket(act, spec.key);
      if (!Number.isFinite(actual)) continue;
      const formatted = fmtActual(spec.key, actual);
      if (row[spec.actualCol] !== formatted) {
        row[spec.actualCol] = formatted;
        changed = true;
      }
      const modelLine = parseLine(row[spec.modelCol]);
      if (Number.isFinite(modelLine)) {
        const sides = ouSideResults(actual, modelLine);
        if (row[spec.overCol] !== sides.over) {
          row[spec.overCol] = sides.over;
          changed = true;
        }
        if (row[spec.underCol] !== sides.under) {
          row[spec.underCol] = sides.under;
          changed = true;
        }
      }
    }

    const src = String(act.source || "live_patch");
    if (row.actual_source !== src) {
      row.actual_source = src;
      changed = true;
    }
    if (changed) {
      detailRows[idx] = row;
      patched++;
    }
  }

  if (patched > 0) {
    console.log(`[projection-tracker] Patched ${patched} live-week detail row(s) from projections / pgatour actuals`);
  }
  return detailRows;
}
