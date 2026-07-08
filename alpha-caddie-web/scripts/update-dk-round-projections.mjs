#!/usr/bin/env node
/**
 * Update all DraftKings round O/U odds in projections.json for the Round projections tab.
 * Does not refresh DataGolf field, outrights, or matchups — only `props` (DK scrape + optional model fallback).
 *
 *   npm run update:dk-round-projections
 *   npm run push:dk-round-projections   — remote only (Render API or GitHub Actions; not local git)
 *
 * Same DK scrape as fetch:book-odds (Playwright + sportsbook-nash). Requires Chromium:
 *   npx playwright install chromium
 *
 * Env (see draftkings-ou-props.mjs / merge-dk-round-props.mjs):
 *   GOLF_SKIP_DK_OU=1 — exit without scraping
 *   GOLF_SKIP_PROPS_CSV=1 — do not merge data/player_props_*.csv (default for this command)
 *   GOLF_SKIP_MODEL_FALLBACK_OU=1 — DK rows only; no synthetic -110 lines when DK omits GIR/etc.
 *   GOLF_SKIP_DK_ROUND_AUDIT_CSV=1 — skip dk_round_projection_audit.csv append
 *   GOLF_SKIP_ROUND_PROJECTIONS_CSV=1 — skip data/round_projections.csv snapshot
 *   DK_LEAGUE_URL, DK_SITE_SEGMENT, DK_LEAGUE_ID
 *   GOLF_MODEL_DIR — repo root (parent of alpha-caddie-web)
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { appendDkRoundProjectionAuditCsv } from "./export-dk-round-model-audit-csv.mjs";
import { writeRoundProjectionsCsv } from "./export-round-projections-csv.mjs";
import { refreshRoundProjectionProps } from "./merge-dk-round-props.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const GOLF_MODEL_ROOT = process.env.GOLF_MODEL_DIR?.trim()
  ? resolve(process.env.GOLF_MODEL_DIR.trim())
  : resolve(WEB_ROOT, "..");

async function main() {
  if (String(process.env.GOLF_SKIP_DK_OU || "").trim() === "1") {
    console.error("[update:dk-round-projections] GOLF_SKIP_DK_OU=1 — nothing to do.");
    process.exit(0);
  }

  const projPath = join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("[update:dk-round-projections] Missing", projPath, "— run npm run fetch:dg first.");
    process.exit(1);
  }

  let payload;
  try {
    payload = JSON.parse(readFileSync(projPath, "utf8"));
  } catch (e) {
    console.error("[update:dk-round-projections] Could not parse projections.json:", e.message);
    process.exit(1);
  }
  if (!payload?.players?.length) {
    console.error("[update:dk-round-projections] projections.json has no players — run npm run fetch:dg first.");
    process.exit(1);
  }

  const prevSkipCsv = process.env.GOLF_SKIP_PROPS_CSV;
  if (prevSkipCsv == null || String(prevSkipCsv).trim() === "") {
    process.env.GOLF_SKIP_PROPS_CSV = "1";
  }

  const { props, nCsv, nDk, dkError, dkLeagueSlug } = await refreshRoundProjectionProps(payload, GOLF_MODEL_ROOT);
  const nPp = props.filter((r) => String(r?.source || "").trim().toLowerCase() === "prizepicks").length;

  if (!props.length) {
    console.error(
      "[update:dk-round-projections] No prop rows merged.",
      dkError ? `(${dkError})` : "",
    );
    process.exit(1);
  }

  const now = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const next = {
    ...payload,
    props,
    updated_at: now,
    book_odds_refreshed_at: now,
    dk_round_props_refreshed_at: now,
    ...(dkLeagueSlug ? { dk_league_slug: dkLeagueSlug } : {}),
  };

  const outJson = JSON.stringify(next, null, 2);
  writeFileSync(projPath, outJson, "utf8");
  console.log(
    `[update:dk-round-projections] Wrote ${projPath} — ${props.length} prop rows (DK: ${nDk}, CSV: ${nCsv}, PP preserved: ${nPp})`,
  );

  const websiteProj = join(GOLF_MODEL_ROOT, "website", "public", "data", "projections.json");
  const websiteDir = dirname(websiteProj);
  if (existsSync(websiteDir)) {
    writeFileSync(websiteProj, outJson, "utf8");
    console.log("[update:dk-round-projections] Wrote", websiteProj);
  }

  if (String(process.env.GOLF_SKIP_DK_ROUND_AUDIT_CSV || "").trim() !== "1") {
    try {
      const audit = appendDkRoundProjectionAuditCsv(next);
      if (audit.appended > 0) {
        console.log(`[update:dk-round-projections] DK audit CSV +${audit.appended} rows -> ${audit.path}`);
      }
    } catch (e) {
      console.warn("[update:dk-round-projections] DK audit CSV:", e.message || e);
    }
  }

  if (String(process.env.GOLF_SKIP_ROUND_PROJECTIONS_CSV || "").trim() !== "1") {
    try {
      const snap = writeRoundProjectionsCsv(next);
      console.log(
        `[update:dk-round-projections] Round projections CSV ${snap.rows} rows -> ${snap.path}`,
      );
    } catch (e) {
      console.warn("[update:dk-round-projections] Round projections CSV:", e.message || e);
    }
  }

  console.log("\n[update:dk-round-projections] Done. Hard-refresh the Round projections tab (Ctrl+Shift+R).\n");
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
