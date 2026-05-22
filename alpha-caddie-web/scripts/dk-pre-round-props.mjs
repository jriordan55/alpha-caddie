/**
 * DraftKings round props from dk_round_projection_audit.csv, using only captures
 * strictly before the round's first tee time (closing pre-round lines).
 */
import { createReadStream, existsSync } from "fs";
import { join } from "path";
import { parse } from "csv-parse";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { parseDgTeetimeParts } from "./open-meteo-forecast.mjs";

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

function addDaysYmd(ymd, days) {
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

  const byRound = new Map();
  for (const p of players || []) {
    const rnd = Math.round(num(p?.round));
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) continue;
    const teeMs = teetimeStrToUtcMs(p?.dg_teetime_local, tz);
    if (!Number.isFinite(teeMs)) continue;
    const prev = byRound.get(rnd);
    if (!Number.isFinite(prev) || teeMs < prev) byRound.set(rnd, teeMs);
  }

  for (let rnd = 1; rnd <= 4; rnd++) {
    if (byRound.has(rnd)) continue;
    if (!dateStart) continue;
    const ymd = addDaysYmd(dateStart, rnd - 1);
    const fallback = localWallClockToUtcMs(ymd, 7, 0, tz);
    if (Number.isFinite(fallback)) byRound.set(rnd, fallback);
  }
  return byRound;
}

/**
 * @param {string} eventName
 * @param {string} auditPath
 * @param {Map<number, number>} roundStartUtcMs
 * @returns {Promise<Map<string, { line: number, over: number, under: number, capturedMs: number }>>}
 *   keys: `${dg_id}|${round}|${market}`
 */
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
    const rnd = Math.round(num(row.display_round));
    const dg = Math.round(num(row.dg_id));
    const market = String(row.market || "").trim();
    if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4 || !Number.isFinite(dg) || !market) continue;

    const roundStart = roundStartUtcMs.get(rnd);
    const capturedMs = Date.parse(String(row.captured_at || "").trim());
    if (!Number.isFinite(capturedMs)) continue;
    if (Number.isFinite(roundStart) && capturedMs >= roundStart) continue;

    const line = num(row.dk_line, NaN);
    const over = num(row.over_odds, NaN);
    const under = num(row.under_odds, NaN);
    if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) continue;

    const key = `${dg}|${rnd}|${market}`;
    const prev = best.get(key);
    if (!prev || capturedMs > prev.capturedMs) {
      best.set(key, { line, over, under, capturedMs });
    }
  }
  return best;
}

export function defaultDkAuditPath(webRoot) {
  return join(webRoot, "data", "dk_round_projection_audit.csv");
}
