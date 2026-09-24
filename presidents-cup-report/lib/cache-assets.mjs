import { createWriteStream, existsSync, mkdirSync } from "fs";
import { join, dirname } from "path";
import { fileURLToPath } from "url";
import { pipeline } from "stream/promises";
import { Readable } from "stream";
import { ASSETS, HEADSHOT_MAP, PC_BASE } from "./branding.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
export const ASSETS_DIR = join(__dirname, "../public/assets");

async function download(url, dest) {
  if (existsSync(dest)) return dest;
  const res = await fetch(url);
  if (!res.ok) throw new Error(`HTTP ${res.status} for ${url}`);
  mkdirSync(dirname(dest), { recursive: true });
  await pipeline(Readable.fromWeb(res.body), createWriteStream(dest));
  return dest;
}

/** Download team badges + all roster headshots to public/assets/ for local serving. */
export async function cachePresidentsCupAssets() {
  mkdirSync(ASSETS_DIR, { recursive: true });
  const manifest = { badges: {}, headshots: {} };

  const badgeJobs = [
    ["usa-badge.png", ASSETS.usaBadge],
    ["int-badge.png", ASSETS.intBadge],
    ["hero.jpg", ASSETS.hero],
    ["course.jpg", ASSETS.course],
  ];
  for (const [file, url] of badgeJobs) {
    await download(url, join(ASSETS_DIR, file));
    manifest.badges[file.replace(/\..+$/, "")] = `/assets/${file}`;
  }

  const bundledLogos = ["pc-logo-full.png", "pc-logo-icon.png", "usa-flag.png"];
  for (const file of bundledLogos) {
    const dest = join(ASSETS_DIR, file);
    if (existsSync(dest)) manifest.badges[file.replace(/\..+$/, "")] = `/assets/${file}`;
  }

  for (const [key, file] of Object.entries(HEADSHOT_MAP)) {
    const url = `${PC_BASE}/${file}?width=320&format=png&optimize=medium`;
    const safe = key.replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
    const localFile = `player-${safe}.png`;
    await download(url, join(ASSETS_DIR, localFile));
    manifest.headshots[key] = `/assets/${localFile}`;
  }

  return manifest;
}
