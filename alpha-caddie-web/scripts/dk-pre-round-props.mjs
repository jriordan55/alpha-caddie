/**
 * DraftKings round props from dk_round_projection_audit.csv, using only captures
 * strictly before the round's first tee time (closing pre-round lines).
 *
 * Round assignment: `round_num` when present, else `display_round` (which DK tab was scraped).
 * Temporal tee-window inference is only used when both are missing.
 */
import { createReadStream, existsSync, readFileSync } from "fs";
import { Readable } from "stream";
import { join } from "path";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { parseDgTeetimeParts } from "./open-meteo-forecast.mjs";
import { parseDkBookLine } from "./round-projection-mu.mjs";

export function num(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

/** Wall-clock local time in `timeZone` → UTC epoch ms (iterative; no extra deps). */
export function localWallClockToUtcMs(ymd, hh, mm, timeZone) {
  const [y, mo, d] = String(ymd || "")
    .split("-")
    .map((x) => Number(x));
  if (!Number.isFinite(y) || !Number.isFinite(mo) || !Number.isFinite(d)) return NaN;
  let utcMs = Date.UTC(y, mo - 1, d, hh, mm, 0, 0);
  const fmt = new Intl.DateTimeFormat("en-US", {
    timeZone,
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    hour12: false,
  });
  for (let i = 0; i < 5; i++) {
    const parts = Object.fromEntries(
      fmt.formatToParts(new Date(utcMs)).filter((p) => p.type !== "literal").map((p) => [p.type, p.value]),
    );
    const gotDay = Number(parts.day);
    const gotH = Number(parts.hour);
    const gotM = Number(parts.minute);
    const diffMin = hh * 60 + mm - (gotH * 60 + gotM) - (gotDay - d) * 24 * 60;
    if (diffMin === 0) return utcMs;
    utcMs += diffMin * 60 * 1000;
  }
  return utcMs;
}

function teetimeStrToUtcMs(teetimeStr, timeZone) {
  const p = parseDgTeetimeParts(teetimeStr);
  if (!p) return NaN;
  return localWallClockToUtcMs(p.ymd, p.hh, p.mm, timeZone);
}

function pad2(n) {
  return String(n).padStart(2, "0");
}

export function addDaysYmd(ymd, days) {
  const [y, mo, d] = ymd.split("-").map(Number);
  if (!Number.isFinite(y)) return "";
  const dt = new Date(Date.UTC(y, mo - 1, d + days));
  return `${dt.getUTCFullYear()}-${pad2(dt.getUTCMonth() + 1)}-${pad2(dt.getUTCDate())}`;
}

/**
 * First tee of the round (earliest dg_teetime_local among projection rows for that round).
 * @returns {Map<number, number>} round → UTC ms
 */
export function buildRoundStartUtcMs(players, payload) {
  const tz =
    String(payload?.meta?.forecast_weather_coords?.timezone || payload?.timezone || "").trim() ||
    "America/New_York";
  const dateStart = String(
    payload?.datagolf_field_date_start || payload?.meta?.datagolf_field_date_start || "",
  ).trim();

  const byRound = buildRoundStartUtcMsFromDateStart(dateStart, tz, players);
  return byRound;
}

/** @param {object[]} [players] optional projection rows with dg_teetime_local */
export function buildRoundStartUtcMsFromDateStart(dateStart, timeZone = "America/New_York", players = []) {
  const byRound = new Map();
  for (const p of players || []) {
    const rnd = Math.round(num(p?.round));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
    const teeMs = teetimeStrToUtcMs(p?.dg_teetime_local, timeZone);
    if (!Number.isFinite(teeMs)) continue;
    const prev = byRound.get(rnd);
    if (!Number.isFinite(prev) || teeMs < prev) byRound.set(rnd, teeMs);
  }

  for (let rnd = 1; rnd <= 4; rnd++) {
    if (byRound.has(rnd)) continue;
    if (!dateStart) continue;
    const ymd = addDaysYmd(dateStart, rnd - 1);
    const fallback = localWallClockToUtcMs(ymd, 7, 0, timeZone);
    if (Number.isFinite(fallback)) byRound.set(rnd, fallback);
  }
  return byRound;
}

function yearFromEventCompleted(dc) {
  const y = parseInt(String(dc || "").slice(0, 4), 10);
  return Number.isFinite(y) ? y : NaN;
}

/**
 * Round tee times for a prior event (historical CSV dates, live bundle, or audit capture span).
 * @returns {Map<number, number>}
 */
export function buildRoundStartUtcMsForAuditEvent(eventName, opts = {}) {
  const tz = opts.timeZone || "America/New_York";
  const eventYear = Math.round(num(opts.eventYear, NaN));
  const histRows = opts.histRows || [];
  const dateStart = String(opts.dateStart || "").trim();
  if (dateStart) return buildRoundStartUtcMsFromDateStart(dateStart, tz);

  const byRound = new Map();

  for (const row of histRows) {
    if (!eventsLikelySame(eventName, String(row.event_name || "").trim())) continue;
    const yr = Math.round(num(row.year, NaN)) || yearFromEventCompleted(row.event_completed);
    if (Number.isFinite(eventYear) && Number.isFinite(yr) && yr !== eventYear) continue;
    const rnd = Math.round(num(row.round_num, NaN));
    const dc = String(row.event_completed || row.date || "").trim().slice(0, 10);
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4 || !/^\d{4}-\d{2}-\d{2}$/.test(dc)) continue;
    const teeMs = localWallClockToUtcMs(dc, 7, 0, tz);
    if (!byRound.has(rnd)) byRound.set(rnd, teeMs);
  }
  if (byRound.size >= 2) return byRound;

  const livePath = opts.livePath;
  if (livePath && existsSync(livePath)) {
    try {
      const live = JSON.parse(readFileSync(livePath, "utf8"));
      const fu = live.field_updates && typeof live.field_updates === "object" ? live.field_updates : {};
      const infoEv = String(live?.info?.event_name || "").trim();
      const fuEv = String(fu.event_name || "").trim();
      const liveEv = eventsLikelySame(eventName, infoEv)
        ? infoEv
        : eventsLikelySame(eventName, fuEv)
          ? fuEv
          : "";
      if (liveEv) {
        const ds = String(
          (eventsLikelySame(eventName, infoEv) ? live?.info?.date_start : "") ||
            (eventsLikelySame(eventName, fuEv) ? fu.date_start : "") ||
            "",
        ).trim();
        if (ds) return buildRoundStartUtcMsFromDateStart(ds, tz);
      }
    } catch {
      /* ignore */
    }
  }

  return byRound;
}

/**
 * Which round's pre-tee window does this audit capture belong to?
 * @param {Map<number, number>} roundStartUtcMs
 */
function preRoundPropRoundIfValid(propRound, roundStartUtcMs, capturedMs) {
  if (!Number.isFinite(propRound) || propRound < 1 || propRound > 4) return NaN;
  if (!Number.isFinite(capturedMs)) return NaN;
  const start = roundStartUtcMs.get(propRound);
  if (Number.isFinite(start) && capturedMs >= start) return NaN;
  return propRound;
}

export function auditPropRoundFromCapture(row, roundStartUtcMs, capturedMs) {
  const explicit = Math.round(num(row.round_num, NaN));
  if (Number.isFinite(explicit)) {
    const r = preRoundPropRoundIfValid(explicit, roundStartUtcMs, capturedMs);
    if (Number.isFinite(r)) return r;
  }

  const displayRound = Math.round(num(row.display_round, NaN));
  if (Number.isFinite(displayRound)) {
    const r = preRoundPropRoundIfValid(displayRound, roundStartUtcMs, capturedMs);
    if (Number.isFinite(r)) return r;
    return NaN;
  }

  if (!Number.isFinite(capturedMs)) return NaN;

  for (let rnd = 4; rnd >= 1; rnd--) {
    const start = roundStartUtcMs.get(rnd);
    if (!Number.isFinite(start) || capturedMs >= start) continue;
    if (rnd === 1) return 1;
    const prevStart = roundStartUtcMs.get(rnd - 1);
    if (Number.isFinite(prevStart) && capturedMs >= prevStart) return rnd;
  }
  return NaN;
}

/**
 * Legacy audit header omitted round_num; newer rows include it and shift columns right.
 */
export function normalizeAuditRow(row) {
  if (!row || typeof row !== "object") return row;
  const dg = Math.round(num(row.dg_id, NaN));
  const pn = String(row.player_name || "").trim();
  if (Number.isFinite(dg) && dg >= 1 && dg <= 4 && /^\d{1,6}$/.test(pn)) {
    return {
      ...row,
      display_round: Math.round(num(row.display_round, dg)) || dg,
      round_num: dg,
      dg_id: Math.round(num(pn)),
      player_name: String(row.market || "").trim(),
      market: String(row.dk_line || "").trim(),
      dk_line: row.over_odds,
      over_odds: row.under_odds,
      under_odds: row.model_total_score,
      model_total_score: row.model_birdies,
      model_birdies: row.model_pars,
      model_pars: row.model_bogeys,
      model_bogeys: row.model_gir,
      model_gir: row.model_fairways,
      model_fairways: row.model_putts,
    };
  }
  const rn = Math.round(num(row.round_num, NaN));
  if (!Number.isFinite(rn)) {
    const dr = Math.round(num(row.display_round, NaN));
    if (Number.isFinite(dr)) return { ...row, round_num: dr };
  }
  return row;
}

function snapFromAuditRow(row, capturedMs) {
  return {
    capturedMs,
    projAt: String(row.projections_updated_at || "").trim(),
    course: String(row.course_used || "").trim(),
    dg: Math.round(num(row.dg_id)),
    playerName: String(row.player_name || "").trim(),
    market: String(row.market || "").trim(),
    dkLine: num(row.dk_line, NaN),
    overOdds: num(row.over_odds, NaN),
    underOdds: num(row.under_odds, NaN),
    modelTotal: num(row.model_total_score, NaN),
    modelBirdies: num(row.model_birdies, NaN),
    modelPars: num(row.model_pars, NaN),
    modelBogeys: num(row.model_bogeys, NaN),
    modelGir: num(row.model_gir, NaN),
    modelFairways: num(row.model_fairways, NaN),
  };
}

/**
 * @param {string} eventName
 * @param {string} auditPath
 * @param {Map<number, number>} roundStartUtcMs
 * @returns {Promise<Map<string, object>>} keys `${dg_id}|${round}|${market}`
 */
function ingestAuditRow(best, row, roundStartUtcMs) {
  const norm = normalizeAuditRow(row);
  const dg = Math.round(num(norm.dg_id));
  const market = String(norm.market || "").trim();
  if (!Number.isFinite(dg) || !market) return;

  const capturedMs = Date.parse(String(norm.captured_at || "").trim());
  const propRound = auditPropRoundFromCapture(norm, roundStartUtcMs, capturedMs);
  if (!Number.isFinite(propRound) || propRound < 1 || propRound > 4) return;

  const line = parseDkBookLine(norm.dk_line);
  const over = num(norm.over_odds, NaN);
  const under = num(norm.under_odds, NaN);
  if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) return;

  const key = `${dg}|${propRound}|${market}`;
  const prev = best.get(key);
  const snap = snapFromAuditRow(norm, capturedMs);
  const openFields = {
    openLine: line,
    openOver: over,
    openUnder: under,
    openCapturedMs: capturedMs,
  };
  // Close = most recent pre-tee capture; open = earliest pre-tee capture.
  if (!prev) {
    best.set(key, {
      line,
      over,
      under,
      capturedMs,
      ...snap,
      ...openFields,
      displayRound: propRound,
    });
    return;
  }
  const next = { ...prev };
  if (!Number.isFinite(prev.openCapturedMs) || capturedMs < prev.openCapturedMs) {
    next.openLine = line;
    next.openOver = over;
    next.openUnder = under;
    next.openCapturedMs = capturedMs;
  }
  if (!Number.isFinite(prev.capturedMs) || capturedMs > prev.capturedMs) {
    Object.assign(next, {
      line,
      over,
      under,
      capturedMs,
      ...snap,
      displayRound: propRound,
      openLine: next.openLine,
      openOver: next.openOver,
      openUnder: next.openUnder,
      openCapturedMs: next.openCapturedMs,
    });
  }
  best.set(key, next);
}

/** Parse audit CSV text (browser or Node) into pre-round DK props index. */
export async function loadPreRoundDkPropsFromAuditText(eventName, csvText, roundStartUtcMs) {
  const best = new Map();
  if (!eventName || !String(csvText || "").trim()) return best;
  await new Promise((resolve, reject) => {
    Readable.from([csvText])
      .pipe(
        parse({
          columns: true,
          relax_quotes: true,
          relax_column_count: true,
          skip_records_with_error: true,
        }),
      )
      .on("data", (row) => {
        const ev = String(row.event_name || "").trim();
        if (!eventsLikelySame(eventName, ev)) return;
        ingestAuditRow(best, row, roundStartUtcMs);
      })
      .on("end", resolve)
      .on("error", reject);
  });
  return best;
}

export async function loadPreRoundDkPropsFromAudit(eventName, auditPath, roundStartUtcMs) {
  const best = new Map();
  if (!eventName || !existsSync(auditPath)) return best;

  const parser = createReadStream(auditPath).pipe(
    parse({
      columns: true,
      relax_quotes: true,
      relax_column_count: true,
      skip_records_with_error: true,
    }),
  );

  for await (const row of parser) {
    const ev = String(row.event_name || "").trim();
    if (!eventsLikelySame(eventName, ev)) continue;
    ingestAuditRow(best, row, roundStartUtcMs);
  }
  return best;
}

function ymdInTimeZone(ms, timeZone = "America/New_York") {
  try {
    return new Intl.DateTimeFormat("en-CA", {
      timeZone,
      year: "numeric",
      month: "2-digit",
      day: "2-digit",
    }).format(new Date(ms));
  } catch {
    return new Date(ms).toISOString().slice(0, 10);
  }
}

/** Thursday (R1) of the PGA week containing `ymd`. */
function pgaThursdayForAnchorYmd(ymd) {
  const d = new Date(`${ymd}T12:00:00Z`);
  const dow = d.getUTCDay();
  if (dow === 4) return ymd;
  if (dow === 5) return addDaysYmd(ymd, -1);
  if (dow === 6) return addDaysYmd(ymd, -2);
  if (dow === 0) return addDaysYmd(ymd, -3);
  return addDaysYmd(ymd, 4 - dow);
}

/** Infer tournament date_start (R1 Thursday) from audit capture span. */
export async function inferDateStartFromAuditCaptures(eventName, auditPath) {
  if (!eventName || !existsSync(auditPath)) return "";
  let maxPreMs = -Infinity;
  let minMs = Infinity;
  const parser = createReadStream(auditPath).pipe(
    parse({ columns: true, relax_quotes: true, relax_column_count: true, skip_records_with_error: true }),
  );
  for await (const row of parser) {
    const ev = String(row.event_name || "").trim();
    if (!eventsLikelySame(eventName, ev)) continue;
    const ms = Date.parse(String(row.captured_at || ""));
    if (!Number.isFinite(ms)) continue;
    if (ms < minMs) minMs = ms;
    const dr = Math.round(num(row.display_round, NaN));
    const rn = Math.round(num(row.round_num, NaN));
    if (dr === 1 || rn === 1) {
      if (ms > maxPreMs) maxPreMs = ms;
    }
  }
  const anchorMs = Number.isFinite(maxPreMs) ? maxPreMs : minMs;
  if (!Number.isFinite(anchorMs)) return "";
  const ymd = ymdInTimeZone(anchorMs, "America/New_York");
  return pgaThursdayForAnchorYmd(addDaysYmd(ymd, 1));
}

export function defaultDkAuditPath(webRoot) {
  return join(webRoot, "data", "dk_round_projection_audit.csv");
}
