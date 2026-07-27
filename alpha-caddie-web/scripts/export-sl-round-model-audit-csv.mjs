#!/usr/bin/env node
/** npm run export:sl-round-audit-csv — post-repair Sleeper audit append. */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { appendSlRoundProjectionAuditCsv } from "./export-pickem-round-model-audit-csv.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = process.env.GOLF_PROJECTIONS_JSON?.trim()
  ? resolve(process.env.GOLF_PROJECTIONS_JSON.trim())
  : join(WEB_ROOT, "projections.json");

if (!existsSync(projPath)) {
  console.error("Missing projections file:", projPath);
  process.exit(1);
}
const payload = JSON.parse(readFileSync(projPath, "utf8"));
const { appended, path } = appendSlRoundProjectionAuditCsv(payload);
console.log(`[sl-round-audit] appended ${appended} row(s) -> ${path}`);
