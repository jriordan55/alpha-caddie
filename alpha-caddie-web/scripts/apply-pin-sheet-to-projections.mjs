#!/usr/bin/env node
/**
 * Apply pin-sheet setup adjustments to projections.json for the active display round.
 *
 * Projections adjust ONLY when pin_sheet_active.json has apply_to_projections: true (user-sent
 * tee sheet). On apply, the sheet is also saved to data/pin_locations/ for Historical Trends.
 *
 *   npm run apply:pin-sheet
 *   push:live runs this after bake:weather (no-op unless manual sheet is armed).
 *   GOLF_SKIP_PIN_SHEET=1 to skip entirely.
 *   GOLF_PIN_SHEET_RULE_ONLY=1 — geometry-only (skip Bayesian hole-history calibration).
 */
import { copyFileSync, existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { num } from "./pin-sheet-difficulty.mjs";
import { roundAdjustmentsFromPinSheetBayesian } from "./pin-sheet-bayesian-calibration.mjs";
import { loadPinHoleScoringIndex } from "./pin-hole-scoring-index.mjs";
import {
  draftKingsDgIdsFromProjections,
  reconcileAllProjectionPlayerRows,
  reconcileProjectionRowCountsToScore,
} from "./course-round-adjustments.mjs";
import {
  courseKeyFromName,
  defaultPinLocationsRoot,
  playDateIsoForRound,
  savePinLocationSheet,
} from "./pin-locations-db.mjs";
import { effectiveDisplayRoundForPinSheet } from "./dg-display-round-from-bundle.mjs";
import { flattenProjectionExportMeta, projectionExportMeta } from "./projection-export-meta.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const PIN_DIR = join(WEB_ROOT, "data", "pin_sheets");
const PROJ_PATH = join(WEB_ROOT, "projections.json");
const ACTIVE_JSON = join(PIN_DIR, "pin_sheet_active.json");
const ACTIVE_IMG = join(PIN_DIR, "pin_sheet.png");

function loadJson(path) {
  return JSON.parse(readFileSync(path, "utf8"));
}

function manualPinSheetArmed(j) {
  if (j?.apply_to_projections === true) return true;
  if (String(j?.apply_to_projections || "").trim().toLowerCase() === "true") return true;
  return false;
}

/** Only pin_sheet_active.json when explicitly armed for this week/round (user-sent tee sheet). */
function resolveManualPinSheet(payload) {
  const meta = projectionExportMeta(payload);
  const event = String(meta.event_name || payload.event_name || "").trim();

  if (!existsSync(ACTIVE_JSON)) return null;

  let j;
  try {
    j = loadJson(ACTIVE_JSON);
  } catch {
    return null;
  }

  if (!manualPinSheetArmed(j)) return null;

  const sheetRound = Math.round(num(j.round ?? j.round_num, NaN));
  const matchRound = effectiveDisplayRoundForPinSheet(payload, sheetRound);
  const sheetEvent = String(j.event_name || j.event_name_ref || "").trim();
  if (!Number.isFinite(sheetRound) || sheetRound !== matchRound) return null;
  if (!sheetEvent || !event || !eventsLikelySame(sheetEvent, event)) return null;
  if (!Array.isArray(j.holes) || j.holes.length < 9) return null;

  return { kind: "file", path: ACTIVE_JSON, sheet: j };
}

/** Armed tee sheet for this event (any round) — for pin_locations DB before display_round catches up. */
function loadArmedPinSheetForEvent(payload) {
  const meta = projectionExportMeta(payload);
  const event = String(meta.event_name || payload.event_name || "").trim();
  if (!existsSync(ACTIVE_JSON)) return null;
  let j;
  try {
    j = loadJson(ACTIVE_JSON);
  } catch {
    return null;
  }
  if (!manualPinSheetArmed(j)) return null;
  const sheetRound = Math.round(num(j.round ?? j.round_num, NaN));
  const sheetEvent = String(j.event_name || j.event_name_ref || "").trim();
  if (!Number.isFinite(sheetRound) || sheetRound < 1 || sheetRound > 4) return null;
  if (!sheetEvent || !event || !eventsLikelySame(sheetEvent, event)) return null;
  if (!Array.isArray(j.holes) || j.holes.length < 9) return null;
  return { kind: "file", path: ACTIVE_JSON, sheet: j, sheetRound };
}

async function maybeParseImageToActiveJson() {
  if (!existsSync(ACTIVE_IMG)) return false;
  const key = String(process.env.OPENAI_API_KEY || "").trim();
  if (!key) {
    if (String(process.env.GOLF_PIN_SHEET_VISION || "").trim() === "1") {
      console.warn("[pin-sheet] OPENAI_API_KEY unset — save pin_sheet_active.json manually");
    }
    return false;
  }

  const visionForced = String(process.env.GOLF_PIN_SHEET_VISION || "").trim() === "1";
  const imgM = statSync(ACTIVE_IMG).mtimeMs;
  let needsParse = visionForced;
  if (!needsParse) {
    if (!existsSync(ACTIVE_JSON)) needsParse = true;
    else {
      const jsonM = statSync(ACTIVE_JSON).mtimeMs;
      needsParse = imgM > jsonM;
    }
  } else if (existsSync(ACTIVE_JSON)) {
    const jsonM = statSync(ACTIVE_JSON).mtimeMs;
    if (jsonM >= imgM) return false;
  }
  if (!needsParse) return false;
  const b64 = readFileSync(ACTIVE_IMG).toString("base64");
  const mime = ACTIVE_IMG.toLowerCase().endsWith(".jpg") || ACTIVE_IMG.toLowerCase().endsWith(".jpeg")
    ? "image/jpeg"
    : "image/png";
  const prompt = `You are reading a PGA Tour ShotLink pin sheet image. Extract all 18 holes.
Return ONLY valid JSON: {"event_name":"","round":3,"source":"screenshot","holes":[{"hole":1,"green_depth_yds":37,"pin_from_front_yds":8,"pin_from_side_yds":5,"pin_side":"L","near_hazard":false,"note":""}, ...]}
Use integers for hole numbers and yardages. near_hazard true if pin is beside water or severe bunker.`;

  const res = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${key}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      model: process.env.GOLF_PIN_SHEET_VISION_MODEL || "gpt-4o-mini",
      messages: [
        {
          role: "user",
          content: [
            { type: "text", text: prompt },
            { type: "image_url", image_url: { url: `data:${mime};base64,${b64}` } },
          ],
        },
      ],
      temperature: 0.1,
      max_tokens: 4000,
    }),
  });
  if (!res.ok) {
    console.warn("[pin-sheet] Vision API failed:", res.status, await res.text().catch(() => ""));
    return false;
  }
  const body = await res.json();
  const text = String(body?.choices?.[0]?.message?.content || "").trim();
  const m = text.match(/\{[\s\S]*\}/);
  if (!m) {
    console.warn("[pin-sheet] Vision response had no JSON");
    return false;
  }
  const parsed = JSON.parse(m[0]);
  parsed.apply_to_projections = true;
  mkdirSync(PIN_DIR, { recursive: true });
  writeFileSync(ACTIVE_JSON, JSON.stringify(parsed, null, 2), "utf8");
  console.log(`[pin-sheet] Vision parsed ${parsed.holes?.length || 0} hole(s) -> ${ACTIVE_JSON}`);
  return true;
}

/** Persist armed tee sheet to data/pin_locations/ (course + play_date + round). */
export function saveArmedPinSheetToPinLocationsDb(sheet, meta) {
  const rnd = Math.round(num(sheet.round ?? sheet.round_num ?? meta.display_round, NaN));
  if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) return null;

  const courseName = String(sheet.course_name || meta.course_used || "").trim();
  const playDate = String(sheet.play_date || "").trim() || playDateIsoForRound(meta, rnd);
  if (!courseName || !/^\d{4}-\d{2}-\d{2}$/.test(playDate)) {
    console.warn("[pin-sheet] skip pin_locations DB — need course_name and play_date (from sheet or projections)");
    return null;
  }

  const rootDir = defaultPinLocationsRoot();
  const imagesDir = join(rootDir, "images");
  mkdirSync(imagesDir, { recursive: true });

  let sourceImage = String(sheet.source_image || "").trim();
  if (existsSync(ACTIVE_IMG)) {
    const destName = `pin_sheet_${playDate}_r${rnd}.png`;
    const dest = join(imagesDir, destName);
    copyFileSync(ACTIVE_IMG, dest);
    sourceImage = destName;
  }

  const { key } = savePinLocationSheet(
    {
      course_name: courseName,
      play_date: playDate,
      round_num: rnd,
      event_name_ref: String(sheet.event_name || sheet.event_name_ref || meta.event_name || "").trim(),
      source_image: sourceImage,
      source: "pin_sheet_active",
      holes: sheet.holes,
    },
    rootDir,
  );
  console.log(`[pin-sheet] Saved to pin_locations DB: ${key}`);
  return key;
}

function restorePinBases(p, metaPin) {
  if (!metaPin?.round || Math.round(num(p.round)) !== Math.round(num(metaPin.round))) return;
  if (Number.isFinite(num(p._pin_base_total_score))) p.total_score = p._pin_base_total_score;
  if (Number.isFinite(num(p._pin_base_birdies))) p.birdies = p._pin_base_birdies;
  if (Number.isFinite(num(p._pin_base_pars))) p.pars = p._pin_base_pars;
  if (Number.isFinite(num(p._pin_base_bogeys))) p.bogeys = p._pin_base_bogeys;
  if (Number.isFinite(num(p._pin_base_gir))) p.gir = p._pin_base_gir;
  if (Number.isFinite(num(p._pin_base_fairways))) p.fairways = p._pin_base_fairways;
  if (Number.isFinite(num(p._pin_base_putts))) p.putts = p._pin_base_putts;
}

function snapshotPinBases(p) {
  p._pin_base_total_score = num(p.total_score, NaN);
  p._pin_base_birdies = num(p.birdies, NaN);
  p._pin_base_pars = num(p.pars, NaN);
  p._pin_base_bogeys = num(p.bogeys, NaN);
  p._pin_base_gir = num(p.gir, NaN);
  p._pin_base_fairways = num(p.fairways, NaN);
  p._pin_base_putts = num(p.putts, NaN);
}

function applyDelta(field, delta) {
  const v = num(field, NaN);
  if (!Number.isFinite(v) || !Number.isFinite(delta)) return field;
  return Math.round((v + delta) * 100) / 100;
}

export async function applyPinSheetToProjections(payload, sheet, pinPath = "", pinIndexCached = null) {
  const meta = projectionExportMeta(payload);
  const rnd = Math.round(num(sheet.round ?? sheet.round_num ?? meta.display_round, NaN));
  if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) {
    throw new Error("pin sheet: invalid round");
  }
  const holes = Array.isArray(sheet.holes) ? sheet.holes : [];
  if (holes.length < 9) {
    throw new Error("pin sheet: need at least 9 holes");
  }

  const adj = await roundAdjustmentsFromPinSheetBayesian(
    {
      ...sheet,
      holes,
      course_key: sheet.course_key || courseKeyFromName(sheet.course_name || meta.course_used),
      play_date: sheet.play_date,
      round: rnd,
    },
    pinIndexCached ? { index: pinIndexCached } : {},
  );
  const stamp = pinPath && existsSync(pinPath) ? `${pinPath}:${statSync(pinPath).mtimeMs}` : "inline";

  const players = Array.isArray(payload.players) ? payload.players : [];
  const prev = meta.pin_sheet;

  for (const p of players) {
    if (Math.round(num(p.round)) !== rnd) continue;
    if (prev?.source_stamp && prev.round === rnd) restorePinBases(p, prev);
    snapshotPinBases(p);
    const par18 = Math.round(num(payload.course_par_18 ?? meta.course_par_18, NaN)) || 72;
    p.total_score = applyDelta(p.total_score, adj.totalScoreDelta);
    if (Number.isFinite(num(p.total_score, NaN))) {
      p.score_to_par = Math.round((p.total_score - par18) * 100) / 100;
    }
    p.gir = applyDelta(p.gir, adj.girDelta);
    p.fairways = applyDelta(p.fairways, adj.fairwaysDelta);
    p.putts = applyDelta(p.putts, adj.totalScoreDelta * 0.35);
    p._pin_adjusted = true;
    const basis = meta.projection_course_basis && typeof meta.projection_course_basis === "object" ? meta.projection_course_basis : {};
    reconcileProjectionRowCountsToScore(p, {
      coursePar18: par18,
      venueAvgBirdies: num(basis.venue_avg_birdies, 4.2),
      venueAvgBogeys: num(basis.venue_avg_bogeys, 2.1),
      venueAvgGir: num(basis.venue_avg_gir, 12),
      venueAvgFairways: num(basis.venue_avg_fairways, 9),
      nFairwayHoles: Math.round(num(basis.fairway_holes_modeled, 14)) || 14,
      fieldCountingMeans: basis.field_counting_means_by_round || null,
      eventWeekFieldScoreByRound: basis.event_week_field_avg_score_by_round || null,
      scoreDeriveCounts: true,
      girBlend: 0.22,
      fairwaysBlend: 0.2,
    });
  }

  const dkField = draftKingsDgIdsFromProjections(payload);
  reconcileAllProjectionPlayerRows(payload, {
    minField: 8,
    skipFieldCalibrate: true,
  });

  meta.pin_sheet = {
    round: rnd,
    event_name: String(sheet.event_name || meta.event_name || "").trim(),
    course_name: String(sheet.course_name || meta.course_used || "").trim(),
    course_key: String(sheet.course_key || "").trim(),
    play_date: String(sheet.play_date || "").trim(),
    grid_yards_per_square: sheet.grid_yards_per_square ?? 5,
    source_file: pinPath ? pinPath.replace(/\\/g, "/") : "inline",
    source_stamp: stamp,
    applied_at: new Date().toISOString().replace(/\.\d{3}Z$/, "Z"),
    source: String(sheet.source || "json").trim(),
    analyst_note: String(sheet.analyst_note || "").trim(),
    summary: adj.summary,
    avg_difficulty: Math.round(adj.avgDifficulty * 1000) / 1000,
    total_score_delta: adj.totalScoreDelta,
    birdies_delta: adj.birdiesDelta,
    bogeys_delta: adj.bogeysDelta,
    pars_delta: adj.parsDelta,
    gir_delta: adj.girDelta,
    fairways_delta: adj.fairwaysDelta,
    putts_delta: Math.round(adj.totalScoreDelta * 0.35 * 100) / 100,
    hard_holes: adj.hardHoles,
    easy_holes: adj.easyHoles,
    holes: adj.perHole,
    calibration: adj.calibration || null,
    rule_total_score_delta: adj.rule_adjustments?.total_score_delta ?? null,
  };

  return { adjustedPlayers: players.filter((p) => Math.round(num(p.round)) === rnd).length, adj };
}

async function main() {
  if (!existsSync(PROJ_PATH)) {
    console.error("[pin-sheet] Missing projections.json");
    process.exit(1);
  }
  mkdirSync(PIN_DIR, { recursive: true });
  await maybeParseImageToActiveJson();

  const payload = loadJson(PROJ_PATH);
  const resolved = resolveManualPinSheet(payload);
  const armed = resolved || loadArmedPinSheetForEvent(payload);
  if (!armed) {
    const dr = Math.round(num(payload.display_round, NaN)) || "?";
    let sr = "?";
    if (existsSync(ACTIVE_JSON)) {
      try {
        const hint = Math.round(num(loadJson(ACTIVE_JSON)?.round, NaN));
        if (Number.isFinite(hint)) sr = hint;
      } catch {
        /* ignore */
      }
    }
    console.log(
      `[pin-sheet] No armed tee sheet for current round (projections R${dr}, sheet R${sr}) — unchanged. Pre-tournament: date_start must be in the future; live week: sheet round must match display_round.`,
    );
    return;
  }

  const sheet = armed.sheet ?? loadJson(armed.path);
  const pinPath = armed.path;
  const meta = projectionExportMeta(payload);
  const enrichedSheet = {
    ...sheet,
    course_name: String(sheet.course_name || meta.course_used || "").trim(),
    play_date: String(sheet.play_date || "").trim() || playDateIsoForRound(meta, sheet.round ?? sheet.round_num),
  };

  const dbKey = saveArmedPinSheetToPinLocationsDb(enrichedSheet, meta);
  if (dbKey) {
    enrichedSheet.course_key = courseKeyFromName(enrichedSheet.course_name);
    enrichedSheet.play_date = enrichedSheet.play_date || dbKey.split("|")[1];
  }
  const pinIndexCached = await loadPinHoleScoringIndex();

  if (!resolved) {
    const sr = Math.round(num(sheet.round ?? sheet.round_num, NaN));
    const dr = Math.round(num(payload.display_round ?? meta.display_round, NaN)) || "?";
    console.log(
      `[pin-sheet] Saved R${sr} tee sheet to pin_locations (projections still R${dr} — apply on push:live when display_round=${sr}).`,
    );
    return;
  }

  const { adjustedPlayers, adj } = await applyPinSheetToProjections(
    payload,
    enrichedSheet,
    pinPath,
    pinIndexCached,
  );
  const metaAfter = projectionExportMeta(payload);
  if (dbKey && metaAfter.pin_sheet) {
    metaAfter.pin_sheet.pin_location_key = dbKey;
    metaAfter.pin_sheet.saved_to_pin_locations = true;
  }
  flattenProjectionExportMeta(payload);
  writeFileSync(PROJ_PATH, JSON.stringify(payload, null, 2), "utf8");
  console.log(
    `[pin-sheet] Applied to ${adjustedPlayers} player row(s) for R${sheet.round ?? sheet.round_num ?? payload.display_round}: ${adj.summary}`,
  );
}

const isMain = process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1]);
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
