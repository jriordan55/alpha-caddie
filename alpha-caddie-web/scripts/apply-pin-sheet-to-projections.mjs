#!/usr/bin/env node
/**
 * Apply pin-sheet setup adjustments to projections.json for the active display round.
 *
 * Data (first match wins):
 *   data/pin_sheets/pin_sheet_active.json  — set round + holes after a screenshot
 *   data/pin_sheets/{event-slug}-r{N}.json
 *   data/pin_sheets/pin_sheet.png + OPENAI_API_KEY — optional vision parse (GOLF_PIN_SHEET_VISION=1)
 *
 *   npm run apply:pin-sheet
 *   push:live runs this after bake:weather (GOLF_SKIP_PIN_SHEET=1 to skip).
 */
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame } from "./dg-events-align.mjs";
import { eventSlug, num, roundAdjustmentsFromPinSheet } from "./pin-sheet-difficulty.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const PIN_DIR = join(WEB_ROOT, "data", "pin_sheets");
const PROJ_PATH = join(WEB_ROOT, "projections.json");
const ACTIVE_JSON = join(PIN_DIR, "pin_sheet_active.json");
const ACTIVE_IMG = join(PIN_DIR, "pin_sheet.png");

function loadJson(path) {
  return JSON.parse(readFileSync(path, "utf8"));
}

function resolvePinSheetPath(payload) {
  const event = String(payload.event_name || "").trim();
  const rnd = Math.round(num(payload.display_round, 1)) || 1;
  const slug = eventSlug(event);
  if (slug) {
    const named = join(PIN_DIR, `${slug}-r${rnd}.json`);
    if (existsSync(named)) return named;
  }
  if (existsSync(ACTIVE_JSON)) {
    const j = loadJson(ACTIVE_JSON);
    const sheetRound = Math.round(num(j.round, NaN));
    const sheetEvent = String(j.event_name || "").trim();
    if (!sheetEvent || !event || eventsLikelySame(sheetEvent, event)) {
      if (!Number.isFinite(sheetRound) || sheetRound === rnd) return ACTIVE_JSON;
    }
  }
  return null;
}

async function maybeParseImageToActiveJson() {
  if (String(process.env.GOLF_PIN_SHEET_VISION || "").trim() !== "1") return false;
  if (!existsSync(ACTIVE_IMG)) return false;
  const key = String(process.env.OPENAI_API_KEY || "").trim();
  if (!key) {
    console.warn("[pin-sheet] GOLF_PIN_SHEET_VISION=1 but OPENAI_API_KEY unset — use pin_sheet_active.json");
    return false;
  }
  if (existsSync(ACTIVE_JSON)) {
    const imgM = statSync(ACTIVE_IMG).mtimeMs;
    const jsonM = statSync(ACTIVE_JSON).mtimeMs;
    if (jsonM >= imgM) return false;
  }
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
  mkdirSync(PIN_DIR, { recursive: true });
  writeFileSync(ACTIVE_JSON, JSON.stringify(parsed, null, 2), "utf8");
  console.log(`[pin-sheet] Vision parsed ${parsed.holes?.length || 0} hole(s) -> ${ACTIVE_JSON}`);
  return true;
}

function restorePinBases(p, metaPin) {
  if (!metaPin?.round || Math.round(num(p.round)) !== Math.round(num(metaPin.round))) return;
  if (Number.isFinite(num(p._pin_base_total_score))) p.total_score = p._pin_base_total_score;
  if (Number.isFinite(num(p._pin_base_birdies))) p.birdies = p._pin_base_birdies;
  if (Number.isFinite(num(p._pin_base_pars))) p.pars = p._pin_base_pars;
  if (Number.isFinite(num(p._pin_base_bogeys))) p.bogeys = p._pin_base_bogeys;
  if (Number.isFinite(num(p._pin_base_gir))) p.gir = p._pin_base_gir;
  if (Number.isFinite(num(p._pin_base_fairways))) p.fairways = p._pin_base_fairways;
}

function snapshotPinBases(p) {
  p._pin_base_total_score = num(p.total_score, NaN);
  p._pin_base_birdies = num(p.birdies, NaN);
  p._pin_base_pars = num(p.pars, NaN);
  p._pin_base_bogeys = num(p.bogeys, NaN);
  p._pin_base_gir = num(p.gir, NaN);
  p._pin_base_fairways = num(p.fairways, NaN);
}

function applyDelta(field, delta) {
  const v = num(field, NaN);
  if (!Number.isFinite(v) || !Number.isFinite(delta)) return field;
  return Math.round((v + delta) * 100) / 100;
}

export function applyPinSheetToProjections(payload, sheet, pinPath = "") {
  if (!payload?.meta) payload.meta = {};
  const rnd = Math.round(num(sheet.round ?? payload.display_round, NaN));
  if (!Number.isFinite(rnd) || rnd < 1 || rnd > 4) {
    throw new Error("pin sheet: invalid round");
  }
  const holes = Array.isArray(sheet.holes) ? sheet.holes : [];
  if (holes.length < 9) {
    throw new Error("pin sheet: need at least 9 holes");
  }

  const adj = roundAdjustmentsFromPinSheet(holes);
  const stamp = pinPath && existsSync(pinPath) ? `${pinPath}:${statSync(pinPath).mtimeMs}` : "inline";

  const players = Array.isArray(payload.players) ? payload.players : [];
  const prev = payload.meta.pin_sheet;

  for (const p of players) {
    if (Math.round(num(p.round)) !== rnd) continue;
    if (prev?.source_stamp && prev.round === rnd) restorePinBases(p, prev);
    snapshotPinBases(p);
    p.total_score = applyDelta(p.total_score, adj.totalScoreDelta);
    p.birdies = applyDelta(p.birdies, adj.birdiesDelta);
    p.pars = applyDelta(p.pars, adj.parsDelta);
    p.bogeys = applyDelta(p.bogeys, adj.bogeysDelta);
    p.gir = applyDelta(p.gir, adj.girDelta);
    p.fairways = applyDelta(p.fairways, adj.fairwaysDelta);
    p._pin_adjusted = true;
  }

  payload.meta.pin_sheet = {
    round: rnd,
    event_name: String(sheet.event_name || payload.event_name || "").trim(),
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
    hard_holes: adj.hardHoles,
    easy_holes: adj.easyHoles,
    holes: adj.perHole,
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
  const pinPath = resolvePinSheetPath(payload);
  if (!pinPath) {
    console.log(
      "[pin-sheet] No pin sheet for this event/round (add data/pin_sheets/pin_sheet_active.json or {event}-r{N}.json)",
    );
    return;
  }

  const sheet = loadJson(pinPath);
  const { adjustedPlayers, adj } = applyPinSheetToProjections(payload, sheet, pinPath);
  writeFileSync(PROJ_PATH, JSON.stringify(payload), "utf8");
  console.log(
    `[pin-sheet] Applied to ${adjustedPlayers} player row(s) for R${sheet.round ?? payload.display_round}: ${adj.summary}`,
  );
}

const isMain = process.argv[1] && fileURLToPath(import.meta.url) === resolve(process.argv[1]);
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
