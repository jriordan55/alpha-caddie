#!/usr/bin/env node
/** npm run export:ud-round-audit-csv — post-repair Underdog audit append. */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { appendUdRoundProjectionAuditCsv } from "./export-pickem-round-model-audit-csv.mjs";

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
const { appended, path } = appendUdRoundProjectionAuditCsv(payload);
console.log(`[ud-round-audit] appended ${appended} row(s) -> ${path}`);
