#!/usr/bin/env node
/**
 * Update Onyx Odds round projections in projections.json (source=onyx).
 *   npm run update:ox-round-projections
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { refreshOnyxRoundProps } from "./merge-onyx-round-props.mjs";
import { robustWriteFile } from "./robust-copy-file.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");

async function main() {
  if (String(process.env.GOLF_SKIP_OX_OU || "").trim() === "1") {
    console.error("[update:ox-round-projections] GOLF_SKIP_OX_OU=1 — nothing to do.");
    process.exit(0);
  }
  const projPath = join(WEB_ROOT, String(process.env.GOLF_PROJECTIONS_FILE || "projections.json").trim() || "projections.json");
  if (!existsSync(projPath)) {
    console.error("[update:ox-round-projections] Missing", projPath);
    process.exit(1);
  }
  const payload = JSON.parse(readFileSync(projPath, "utf8"));
  if (!payload?.players?.length) {
    console.error("[update:ox-round-projections] projections.json has no players");
    process.exit(1);
  }
  const { props, nOx, oxError } = await refreshOnyxRoundProps(payload);
  if (!nOx) {
    console.error("[update:ox-round-projections] No Onyx rows merged.", oxError ? `(${oxError})` : "");
    process.exit(1);
  }
  const now = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const next = { ...payload, props, updated_at: now, ox_round_props_refreshed_at: now };
  const outJson = JSON.stringify(next, null, 2);
  const wrote = await robustWriteFile(projPath, outJson);
  if (!wrote.ok) {
    console.error(`[update:ox-round-projections] FAIL writing ${projPath}:`, wrote.error?.message || wrote.error);
    process.exit(1);
  }
  console.log(`[update:ox-round-projections] Wrote ${projPath} — ${nOx} Onyx prop rows`);
  const websiteProj = join(GOLF_MODEL_ROOT, "website", "public", "data", "projections.json");
  if (existsSync(dirname(websiteProj))) {
    const mir = await robustWriteFile(websiteProj, outJson, { attempts: 6 });
    if (!mir.ok) console.warn(`[update:ox-round-projections] WARN: could not write ${websiteProj}`);
  }
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
