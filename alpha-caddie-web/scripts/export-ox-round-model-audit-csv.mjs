#!/usr/bin/env node
import { existsSync, readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { appendOxRoundProjectionAuditCsv } from "./export-pickem-round-model-audit-csv.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const projPath = join(WEB_ROOT, "projections.json");
if (!existsSync(projPath)) {
  console.error("Missing", projPath);
  process.exit(1);
}
const payload = JSON.parse(readFileSync(projPath, "utf8"));
const { appended, path } = appendOxRoundProjectionAuditCsv(payload);
console.log(`[ox-round-audit] appended ${appended} row(s) -> ${path}`);
