#!/usr/bin/env node
/**
 * Save an array of pin sheet objects into data/pin_locations/.
 * Usage: node scripts/save-pin-sheets-batch.mjs path/to/batch.json
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import {
  defaultPinLocationsRoot,
  loadPinLocationsIndex,
  normalizePinLocationSheet,
  pinLocationKey,
  savePinLocationSheet,
} from "./pin-locations-db.mjs";

const force = process.argv.includes("--force");

const batchPath = process.argv[2];
if (!batchPath) {
  console.error("Usage: save-pin-sheets-batch.mjs <batch.json>");
  process.exit(1);
}

const rootDir = defaultPinLocationsRoot();
const imagesDir = path.join(rootDir, "images");
fs.mkdirSync(imagesDir, { recursive: true });
const index = loadPinLocationsIndex(rootDir);
const existingKeys = new Set((index.entries || []).map((e) => e.key));

const batch = JSON.parse(fs.readFileSync(batchPath, "utf8"));
const sheets = Array.isArray(batch) ? batch : batch.sheets || [];
let saved = 0;
let failed = 0;

for (const raw of sheets) {
  try {
    const sheet = normalizePinLocationSheet({
      ...raw,
      source: raw.source || "pin_locations_batch_vision",
    });
    if (!sheet.course_key || !sheet.play_date || !sheet.round_num || sheet.holes.length < 9) {
      throw new Error(`invalid sheet ${raw.source_image || "?"}: ${sheet.holes.length} holes`);
    }
    const key = pinLocationKey(sheet.course_key, sheet.play_date, sheet.round_num);
    if (!force && key && existingKeys.has(key)) {
      console.log(`[save-pin-batch] skip existing ${raw.source_image || "?"} → ${key}`);
      continue;
    }
    if (raw._image_src && raw.source_image) {
      const dest = path.join(imagesDir, raw.source_image);
      if (fs.existsSync(raw._image_src) && !fs.existsSync(dest)) {
        fs.copyFileSync(raw._image_src, dest);
      }
    }
    const { key: savedKey } = savePinLocationSheet(sheet, rootDir);
    existingKeys.add(savedKey);
    saved++;
    console.log(`[save-pin-batch] ${raw.source_image || "?"} → ${savedKey} (${sheet.holes.length} holes)`);
  } catch (e) {
    failed++;
    console.warn(`[save-pin-batch] FAILED ${raw?.source_image || "?"}:`, e?.message || e);
  }
}

console.log(`[save-pin-batch] Done — saved ${saved}, failed ${failed}`);
