/**
 * Patch tracker detail rows with fresher live-week actuals from projections.json
 * and pgatour_event_rounds.json (without re-running the full CSV export).
 */
import { eventsLikelySame } from "../scripts/dg-events-align.mjs";
import {
  completedRoundCapFromPayload,
  pgatourRowBelongsToEvent,
} from "../scripts/live-event-actuals-cap.mjs";
import { ouProjectedMeanForLive } from "../scripts/projected-mean-live.mjs";
import {
  DETAIL_EXPORT_MARKETS,
  fmtDkBookLine,
  fmtPpBookLine,
  gradeLineForDetailRow,
  ouSideResults,
  parseDkBookLine,
} from "./detail-market-specs.mjs";
import { formatAmerican } from "./ev-math.mjs";

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
  if (liveCountingPlaceholder(act)) return NaN;
  const b = num(act.birdies);
  const eob = num(act.eagles_or_better);
  const eg = num(act.eagles);
  const eagleAdd = Number.isFinite(eob) ? eob : Number.isFinite(eg) ? eg : 0;
  if (!Number.isFinite(b) && !Number.isFinite(eob) && !Number.isFinite(eg)) return NaN;
  return (Number.isFinite(b) ? b : 0) + eagleAdd;
}

function bogeysFromAct(act) {
  if (!act || typeof act !== "object") return NaN;
  if (liveCountingPlaceholder(act)) return NaN;
  const bg = num(act.bogeys ?? act.bogies);
  const dow = num(act.doubles_or_worse);
  const dbl = num(act.doubles);
  const dblAdd = Number.isFinite(dow) ? dow : Number.isFinite(dbl) ? dbl : 0;
  if (!Number.isFinite(bg) && !Number.isFinite(dow) && !Number.isFinite(dbl)) return NaN;
  return (Number.isFinite(bg) ? bg : 0) + Math.max(0, dblAdd);
}

function fmtActual(marketKey, v) {
  if (!Number.isFinite(v)) return "";
  if (marketKey === "total") return (Math.round(v * 10) / 10).toFixed(1);
  return String(Math.round(v));
}

const RESULT_COLS = {
  total: {
    actualCol: "actual_round_score",
    overCol: "round_score_over",
    underCol: "round_score_under",
  },
  birdies: { actualCol: "actual_birdies", overCol: "birdies_over", underCol: "birdies_under" },
  bogeys: { actualCol: "actual_bogeys", overCol: "bogeys_over", underCol: "bogeys_under" },
  gir: { actualCol: "actual_gir", overCol: "gir_over", underCol: "gir_under" },
  fairways: { actualCol: "actual_fairways", overCol: "fairways_over", underCol: "fairways_under" },
};

const MARKET_ACTUAL = DETAIL_EXPORT_MARKETS.map((spec) => ({
  ...spec,
  ...RESULT_COLS[spec.key],
  modelCol: spec.lineCol,
}));

function actualForMarket(act, marketKey) {
  if (!act || typeof act !== "object") return NaN;
  if (marketKey === "total") return num(act.round_score ?? act.total_score);
  if (marketKey === "birdies") return birdiesFromAct(act);
  if (marketKey === "bogeys") return bogeysFromAct(act);
  if (marketKey === "gir") return num(act.gir);
  if (marketKey === "fairways") return num(act.fairways);
  return NaN;
}

function liveCountingPlaceholder(act) {
  if (!act || typeof act !== "object") return true;
  const b = num(act.birdies);
  const p = num(act.pars);
  const bg = num(act.bogeys ?? act.bogies);
  // Missing entirely is not usable as a posted hole-count actual.
  if (!Number.isFinite(b) && !Number.isFinite(p) && !Number.isFinite(bg)) return true;
  // Explicit zero triad (or zero bird+bog with empty pars) = DG/live stub.
  if (b === 0 && bg === 0 && (!Number.isFinite(p) || p === 0)) return true;
  if (b === 0 && bg === 0 && Number.isFinite(p) && p >= 10) return true;
  return false;
}

function clearCountingActualCols(row) {
  for (const spec of MARKET_ACTUAL) {
    if (spec.key === "total" || spec.key === "gir" || spec.key === "fairways") continue;
    if (row[spec.actualCol] !== "" && row[spec.actualCol] != null) {
      row[spec.actualCol] = "";
    }
    if (spec.overCol) row[spec.overCol] = "";
    if (spec.underCol) row[spec.underCol] = "";
  }
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
      const next = {
        ...prev,
        round_score: score,
        total_score: score,
        source: prev.source ? `${prev.source}+live` : act.source || "live_projections",
      };
      // Prefer real PGA counting already on `prev`; never write stub zeros from live.
      if (!liveCountingPlaceholder(act)) {
        if (Number.isFinite(num(act.birdies))) next.birdies = act.birdies;
        if (Number.isFinite(num(act.pars))) next.pars = act.pars;
        if (Number.isFinite(num(act.bogeys ?? act.bogies))) next.bogeys = act.bogeys ?? act.bogies;
        if (Number.isFinite(num(act.eagles_or_better))) next.eagles_or_better = act.eagles_or_better;
        if (Number.isFinite(num(act.doubles_or_worse))) next.doubles_or_worse = act.doubles_or_worse;
        delete next._live_counting_placeholder;
      } else if (liveCountingPlaceholder(next)) {
        next._live_counting_placeholder = true;
      } else {
        delete next._live_counting_placeholder;
      }
      if (Number.isFinite(num(act.gir)) && num(act.gir) >= 0) next.gir = act.gir;
      if (Number.isFinite(num(act.fairways)) && num(act.fairways) >= 0) next.fairways = act.fairways;
      out.set(key, next);
      n++;
    }
  }
  return n;
}

/** @param {Map<string, object>} out */
function overlayPgatourActuals(out, pgPayload, eventName, projections) {
  const rounds = Array.isArray(pgPayload?.rounds) ? pgPayload.rounds : [];
  const metaEvent = String(pgPayload?.meta?.event_name || "").trim();
  if (metaEvent && !eventsLikelySame(eventName, metaEvent)) return 0;
  const completedCap = projections ? completedRoundCapFromPayload(projections) : NaN;
  const courseUsed = String(projections?.course_used || projections?.meta?.course_used || "").trim();
  let n = 0;
  for (const r of rounds) {
    if (!r?._from_pgatour) continue;
    if (!pgatourRowBelongsToEvent(r, eventName, { courseUsed })) continue;
    const dg = Math.round(num(r.dg_id));
    const rnd = Math.round(num(r.round_num));
    const score = num(r.round_score);
    if (!Number.isFinite(dg) || !Number.isFinite(rnd) || !Number.isFinite(score) || score <= 0) continue;
    if (Number.isFinite(completedCap) && rnd > completedCap) continue;
    const key = `${dg}|${rnd}`;
    const prev = out.get(key) || {};
    const next = { ...prev, source: prev.source ? `${prev.source}+pgatour` : "pgatour_event_rounds" };
    const probe = {
      birdies: num(r.birdies),
      pars: num(r.pars),
      bogeys: num(r.bogeys ?? r.bogies),
    };
    if (!liveCountingPlaceholder(probe)) {
      const eob = num(r.eagles_or_better);
      const b = num(r.birdies);
      if (Number.isFinite(b)) next.birdies = b + (Number.isFinite(eob) ? eob : 0);
      const bg = num(r.bogeys ?? r.bogies);
      if (Number.isFinite(bg)) next.bogeys = bg;
      if (Number.isFinite(num(r.pars))) next.pars = num(r.pars);
      delete next._live_counting_placeholder;
    }
    if (Number.isFinite(num(r.gir))) next.gir = num(r.gir);
    if (Number.isFinite(num(r.fairways))) next.fairways = num(r.fairways);
    if (!Number.isFinite(num(prev.round_score))) {
      next.round_score = score;
      next.total_score = score;
    }
    out.set(key, next);
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
        const gradeLine = gradeLineForDetailRow(next, spec);
        const sides = ouSideResults(actual, gradeLine);
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

function rowHasCompletedScore(row) {
  const score = num(row.actual_round_score);
  return Number.isFinite(score) && score > 0;
}

function fmtBookLine(market, line, { prizePicks = false } = {}) {
  return prizePicks ? fmtPpBookLine(market, line) : fmtDkBookLine(market, line);
}

function dkPropForPatch(preRound, liveDk, dg, rnd, propsMarket, row) {
  const key = `${dg}|${rnd}|${propsMarket}`;
  const pre = preRound?.[key];
  if (pre && Number.isFinite(pre.line)) return { ...pre, oddsSource: "pre_round_audit" };
  if (rowHasCompletedScore(row)) return null;
  const live = liveDk?.[key];
  if (live && Number.isFinite(live.line)) return { ...live, oddsSource: "live_snapshot" };
  return null;
}

function ppPropForPatch(preRound, livePp, dg, rnd, propsMarket, row) {
  const key = `${dg}|${rnd}|${propsMarket}`;
  const pre = preRound?.[key];
  if (pre && Number.isFinite(pre.line)) return { ...pre, oddsSource: "pre_round_audit" };
  if (rowHasCompletedScore(row)) return null;
  const live = livePp?.[key];
  if (live && Number.isFinite(live.line)) return { ...live, oddsSource: "prizepicks_live" };
  return null;
}

function auditModelFromPropSnap(snap, spec) {
  if (!snap) return NaN;
  if (spec.key === "total") return num(snap.modelTotal);
  if (spec.key === "birdies") return num(snap.modelBirdies);
  if (spec.key === "pars") return num(snap.modelPars);
  if (spec.key === "bogeys") return num(snap.modelBogeys);
  if (spec.key === "gir") return num(snap.modelGir);
  if (spec.key === "fairways") return num(snap.modelFairways);
  return NaN;
}

function patchBookLinesFromLiveProps(detailRows, projections, liveBookProps) {
  if (!liveBookProps || !Array.isArray(detailRows) || !detailRows.length) return detailRows;
  const eventName = String(liveBookProps.event_name || projections?.event_name || "").trim();
  if (!eventName) return detailRows;

  const preRoundDk = liveBookProps.pre_round_dk || {};
  const preRoundPp = liveBookProps.pre_round_pp || {};
  const liveDk = liveBookProps.live_dk || {};
  const livePp = liveBookProps.live_pp || {};
  let patched = 0;

  for (let i = 0; i < detailRows.length; i++) {
    const row = detailRows[i];
    if (!eventsLikelySame(row.event_name, eventName)) continue;
    if (row.pricing_mode !== "default" || row.pricing_skill !== "default") continue;
    if (String(row.book_odds_source || "").trim() && String(row.pp_book_odds_source || "").trim()) continue;

    const dg = Math.round(num(row.dg_id));
    const rnd = Math.round(num(row.round));
    if (!Number.isFinite(dg) || !Number.isFinite(rnd)) continue;

    const next = { ...row };
    let changed = false;
    let oddsSource = String(next.book_odds_source || "").trim();
    let ppOddsSource = String(next.pp_book_odds_source || "").trim();

    for (const spec of DETAIL_EXPORT_MARKETS) {
      const hasBook =
        String(next[spec.bookLineCol] || "").trim() &&
        (String(next[spec.overOddsCol] || "").trim() || String(next[spec.underOddsCol] || "").trim());
      if (!hasBook) {
        const dk = dkPropForPatch(preRoundDk, liveDk, dg, rnd, spec.propsMarket, row);
        if (dk) {
          next[spec.bookLineCol] = fmtBookLine(spec.market, parseDkBookLine(dk.line));
          next[spec.overOddsCol] = formatAmerican(dk.over);
          next[spec.underOddsCol] = formatAmerican(dk.under);
          if (!oddsSource) oddsSource = dk.oddsSource;
          if (dk.oddsSource === "pre_round_audit") {
            const auditMu = auditModelFromPropSnap(dk, spec);
            if (Number.isFinite(auditMu)) {
              next[spec.lineCol] = fmtModelLine(spec.market, auditMu);
            }
          }
          changed = true;
        }
      }

      const hasPp =
        String(next[spec.ppLineCol] || "").trim() &&
        (String(next[spec.ppOverOddsCol] || "").trim() || String(next[spec.ppUnderOddsCol] || "").trim());
      if (!hasPp) {
        const pp = ppPropForPatch(preRoundPp, livePp, dg, rnd, spec.propsMarket, row);
        if (pp) {
          next[spec.ppLineCol] = fmtBookLine(spec.market, num(pp.line), { prizePicks: true });
          next[spec.ppOverOddsCol] = formatAmerican(pp.over);
          next[spec.ppUnderOddsCol] = formatAmerican(pp.under);
          if (!ppOddsSource) ppOddsSource = pp.oddsSource;
          if (pp.oddsSource === "pre_round_audit") {
            const auditMu = auditModelFromPropSnap(pp, spec);
            if (Number.isFinite(auditMu)) {
              next[spec.lineCol] = fmtModelLine(spec.market, auditMu);
            }
          }
          changed = true;
        }
      }
    }

    if (oddsSource && next.book_odds_source !== oddsSource) {
      next.book_odds_source = oddsSource;
      changed = true;
    }
    if (ppOddsSource && next.pp_book_odds_source !== ppOddsSource) {
      next.pp_book_odds_source = ppOddsSource;
      changed = true;
    }

    if (changed) {
      detailRows[i] = next;
      patched++;
    }
  }

  if (patched > 0) {
    console.log(`[projection-tracker] Patched book lines on ${patched} live-week row(s) from live_event_book_props.json`);
  }
  return detailRows;
}

/**
 * @param {Record<string, string>[]} detailRows
 * @param {object | null} projections
 * @param {object | null} pgPayload
 * @param {object | null} [liveBookProps]
 */
export function patchDetailRowsFromLiveSources(detailRows, projections, pgPayload, liveBookProps = null) {
  if (!Array.isArray(detailRows) || !detailRows.length) return detailRows;
  patchModelLinesFromProjections(detailRows, projections);
  patchBookLinesFromLiveProps(detailRows, projections, liveBookProps);
  const eventName = String(projections?.event_name || projections?.meta?.event_name || pgPayload?.meta?.event_name || "").trim();
  if (!eventName) return detailRows;

  /** @type {Map<string, object>} */
  const actuals = new Map();
  // PGA first (real hole counts), then live (score/SG/GIR — never stub bird/bog zeros).
  overlayPgatourActuals(actuals, pgPayload, eventName, projections);
  overlayLiveActuals(actuals, projections, eventName);
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
  let clearedStub = 0;
  for (const [key, act] of actuals.entries()) {
    const idx = rowIdx.get(key);
    if (idx === undefined) continue;
    const row = { ...detailRows[idx] };
    let changed = false;

    const countingMissing = liveCountingPlaceholder(act) || act._live_counting_placeholder;
    if (countingMissing) {
      // Wipe CSV / prior stub zeros so tracker ROI never grades fake 0 birdies.
      const hadStub =
        String(row.actual_birdies || "").trim() === "0" ||
        String(row.actual_bogeys || "").trim() === "0" ||
        String(row.actual_pars || "").trim() === "0";
      clearCountingActualCols(row);
      if (hadStub) {
        clearedStub++;
        changed = true;
      }
    }

    for (const spec of MARKET_ACTUAL) {
      if (countingMissing && (spec.key === "birdies" || spec.key === "bogeys" || spec.key === "pars")) {
        continue;
      }
      const actual = actualForMarket(act, spec.key);
      if (!Number.isFinite(actual)) continue;
      const formatted = fmtActual(spec.key, actual);
      if (row[spec.actualCol] !== formatted) {
        row[spec.actualCol] = formatted;
        changed = true;
      }
      const gradeLine = gradeLineForDetailRow(row, spec);
      if (Number.isFinite(gradeLine)) {
        const sides = ouSideResults(actual, gradeLine);
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
    console.log(
      `[projection-tracker] Patched ${patched} live-week detail row(s) from projections / pgatour actuals` +
        (clearedStub ? ` (cleared ${clearedStub} stub zero counting actuals)` : ""),
    );
  }
  return detailRows;
}
