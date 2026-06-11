#!/usr/bin/env node
/**
 * Import ShotLink pin location PNGs from data/pin_locations.zip into
 * data/pin_locations/ keyed by course + play_date + round_num.
 *
 * Requires OPENAI_API_KEY for vision extraction (gpt-4o-mini).
 *
 *   npm run import:pin-locations
 *   npm run import:pin-locations -- --limit 5
 *   npm run import:pin-locations -- --force
 *
 * Env: PIN_LOCATIONS_ZIP, PIN_LOCATIONS_DIR, OPENAI_API_KEY,
 *      GOLF_PIN_SHEET_VISION_MODEL (default gpt-4o-mini)
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { execSync } from "child_process";
import {
  defaultPinLocationsRoot,
  loadPinLocationsIndex,
  normalizePinLocationSheet,
  pinLocationKey,
  savePinLocationSheet,
} from "./pin-locations-db.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const MODEL_ROOT = path.resolve(__dirname, "..", "..");

function parseArgs(argv) {
  const out = { limit: 0, force: false, dryRun: false };
  for (let i = 2; i < argv.length; i++) {
    if (argv[i] === "--limit") out.limit = parseInt(argv[++i], 10) || 0;
    else if (argv[i] === "--force") out.force = true;
    else if (argv[i] === "--dry-run") out.dryRun = true;
  }
  return out;
}

function resolveZipPath() {
  const env = process.env.PIN_LOCATIONS_ZIP;
  if (env) return path.resolve(env);
  for (const p of [
    path.join(MODEL_ROOT, "data", "pin_locations.zip"),
    path.join(MODEL_ROOT, "pin_locations.zip"),
  ]) {
    if (fs.existsSync(p)) return p;
  }
  throw new Error("Missing data/pin_locations.zip");
}

function extractZip(zipPath, destDir) {
  fs.mkdirSync(destDir, { recursive: true });
  if (process.platform === "win32") {
    execSync(
      `powershell -NoProfile -Command "Expand-Archive -Path '${zipPath.replace(/'/g, "''")}' -DestinationPath '${destDir.replace(/'/g, "''")}' -Force"`,
      { stdio: "inherit" },
    );
  } else {
    execSync(`unzip -o "${zipPath}" -d "${destDir}"`, { stdio: "inherit" });
  }
}

const VISION_PROMPT = `You are reading an official PGA Tour ShotLink pin location sheet image (often from X/Twitter).

Extract metadata and all 18 holes. Each green grid square side = 5 YARDS (not feet).

Return ONLY valid JSON:
{
  "course_name": "TPC Toronto at Osprey Valley (North Course)",
  "play_date": "2025-06-05",
  "round_num": 1,
  "event_name_ref": "RBC Canadian Open",
  "holes": [
    {
      "hole": 1,
      "green_depth_yds": 32,
      "pin_from_front_yds": 6,
      "pin_from_side_yds": 4,
      "pin_side": "L",
      "near_hazard": false,
      "note": ""
    }
  ]
}

Rules:
- play_date: ISO YYYY-MM-DD for when this round is played (from sheet header, e.g. "Thursday, June 5, 2025" → 2025-06-05)
- round_num: integer 1-4 from "Round N" in header
- pin_side: "L" or "R" (which side the pin distance is measured from)
- near_hazard: true if pin is beside water or severe bunker
- Include all 18 holes when visible; use null for missing numeric fields only if unreadable
- course_name from header (not tournament name)`;

async function parsePinImageWithVision(imagePath, apiKey) {
  const b64 = fs.readFileSync(imagePath).toString("base64");
  const lower = imagePath.toLowerCase();
  const mime = lower.endsWith(".jpg") || lower.endsWith(".jpeg") ? "image/jpeg" : "image/png";
  const res = await fetch("https://api.openai.com/v1/chat/completions", {
    method: "POST",
    headers: {
      Authorization: `Bearer ${apiKey}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify({
      model: process.env.GOLF_PIN_SHEET_VISION_MODEL || "gpt-4o-mini",
      messages: [
        {
          role: "user",
          content: [
            { type: "text", text: VISION_PROMPT },
            { type: "image_url", image_url: { url: `data:${mime};base64,${b64}` } },
          ],
        },
      ],
      temperature: 0.05,
      max_tokens: 4500,
    }),
  });
  if (!res.ok) {
    throw new Error(`Vision API ${res.status}: ${await res.text().catch(() => "")}`);
  }
  const body = await res.json();
  const text = String(body?.choices?.[0]?.message?.content || "").trim();
  const m = text.match(/\{[\s\S]*\}/);
  if (!m) throw new Error("Vision response had no JSON");
  return JSON.parse(m[0]);
}

function listImages(dir) {
  return fs
    .readdirSync(dir)
    .filter((f) => /\.(png|jpg|jpeg)$/i.test(f))
    .sort((a, b) => a.localeCompare(b, undefined, { numeric: true }));
}

async function main() {
  const args = parseArgs(process.argv);
  const rootDir = defaultPinLocationsRoot();
  const zipPath = resolveZipPath();
  const extractDir = path.join(rootDir, "_import_staging");
  const imagesDir = path.join(rootDir, "images");

  console.log("[import:pin-locations] Zip:", zipPath);
  console.log("[import:pin-locations] DB root:", rootDir);

  extractZip(zipPath, extractDir);
  fs.mkdirSync(imagesDir, { recursive: true });

  const apiKey = String(process.env.OPENAI_API_KEY || "").trim();
  if (!apiKey) {
    console.error(
      "[import:pin-locations] OPENAI_API_KEY required to parse pin sheet images. Set the key and re-run.",
    );
    process.exit(1);
  }

  const images = listImages(extractDir);
  console.log("[import:pin-locations] Images to process:", images.length);

  const index = loadPinLocationsIndex(rootDir);
  const existingByImage = new Map(
    (index.entries || []).filter((e) => e.source_image).map((e) => [e.source_image, e.key]),
  );

  let processed = 0;
  let saved = 0;
  let skipped = 0;
  let failed = 0;

  for (const file of images) {
    if (args.limit > 0 && processed >= args.limit) break;
    processed++;

    if (!args.force && existingByImage.has(file)) {
      skipped++;
      continue;
    }

    const srcPath = path.join(extractDir, file);
    try {
      console.log(`[import:pin-locations] Parsing ${file} (${processed}/${images.length})…`);
      const parsed = await parsePinImageWithVision(srcPath, apiKey);
      const sheet = normalizePinLocationSheet(
        {
          ...parsed,
          source_image: file,
          source: "pin_locations_zip_vision",
        },
      );

      if (!sheet.course_key || !sheet.play_date || !sheet.round_num) {
        throw new Error(`missing course/date/round: ${JSON.stringify(sheet).slice(0, 200)}`);
      }
      if (sheet.holes.length < 9) {
        throw new Error(`only ${sheet.holes.length} holes parsed`);
      }

      const destImage = path.join(imagesDir, file);
      if (!args.dryRun) {
        fs.copyFileSync(srcPath, destImage);
        const { key } = savePinLocationSheet(sheet, rootDir);
        existingByImage.set(file, key);
        saved++;
        console.log(`  → ${key} (${sheet.holes.length} holes)`);
      } else {
        console.log(`  [dry-run] would save ${pinLocationKey(sheet.course_key, sheet.play_date, sheet.round_num)}`);
      }

      await new Promise((r) => setTimeout(r, 400));
    } catch (e) {
      failed++;
      console.warn(`[import:pin-locations] FAILED ${file}:`, e?.message || e);
    }
  }

  console.log(
    `[import:pin-locations] Done — saved ${saved}, skipped ${skipped}, failed ${failed}, processed ${processed}`,
  );
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
