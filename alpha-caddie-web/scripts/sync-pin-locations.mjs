#!/usr/bin/env node
/**
 * Copy pin_locations index + sheets into alpha-caddie-web/data for static deploy.
 *   npm run sync:pin-locations
 */
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { defaultPinLocationsRoot } from "./pin-locations-db.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const MODEL_ROOT = path.resolve(WEB_ROOT, "..");

function copyDir(src, dest) {
  if (!fs.existsSync(src)) return;
  fs.mkdirSync(dest, { recursive: true });
  for (const name of fs.readdirSync(src)) {
    const s = path.join(src, name);
    const d = path.join(dest, name);
    if (fs.statSync(s).isDirectory()) copyDir(s, d);
    else fs.copyFileSync(s, d);
  }
}

const src = defaultPinLocationsRoot();
const dest = path.join(WEB_ROOT, "data", "pin_locations");
if (!fs.existsSync(path.join(src, "index.json"))) {
  console.log("[sync:pin-locations] No index.json at", src, "— run import:pin-locations first");
  process.exit(0);
}
fs.mkdirSync(dest, { recursive: true });
for (const sub of ["index.json", "sheets", "images"]) {
  const sp = path.join(src, sub);
  const dp = path.join(dest, sub);
  if (!fs.existsSync(sp)) continue;
  if (fs.statSync(sp).isDirectory()) copyDir(sp, dp);
  else fs.copyFileSync(sp, dp);
}
console.log("[sync:pin-locations] Mirrored", src, "→", dest);

const webPublic = path.join(MODEL_ROOT, "website", "public", "data", "pin_locations");
if (fs.existsSync(path.join(MODEL_ROOT, "website", "public", "data"))) {
  copyDir(dest, webPublic);
  console.log("[sync:pin-locations] Mirrored → website/public/data/pin_locations");
}
