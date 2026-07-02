#!/usr/bin/env node
/**
 * Merge missing DK audit rows from an older git commit (default eaeb128 = 2026-06-27).
 * Travelers R2/R3 captures on 2026-06-26–27 were dropped from the live audit log.
 */
import { execSync } from "child_process";
import { readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { auditPropRoundFromCapture, buildRoundStartUtcMsFromDateStart } from "./dk-pre-round-props.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const AUDIT = join(WEB, "data", "dk_round_projection_audit.csv");
const GIT_REF = process.env.DK_AUDIT_RECOVER_REF || "eaeb128";

function parseCsvRow(line) {
  const c = [];
  let cur = "";
  let q = false;
  for (const ch of line) {
    if (ch === '"') {
      q = !q;
      continue;
    }
    if (ch === "," && !q) {
      c.push(cur);
      cur = "";
      continue;
    }
    cur += ch;
  }
  c.push(cur);
  return c;
}

function rowKey(parts) {
  return `${parts[0]}|${parts[6]}|${parts[8]}`;
}

function main() {
  const gitCsv = execSync(`git show ${GIT_REF}:alpha-caddie-web/data/dk_round_projection_audit.csv`, {
    encoding: "utf8",
    maxBuffer: 256 * 1024 * 1024,
  });
  const curCsv = readFileSync(AUDIT, "utf8");
  const gitLines = gitCsv.split(/\r?\n/).filter(Boolean);
  const curLines = curCsv.split(/\r?\n/).filter(Boolean);
  const hdr = curLines[0];

  const curKeys = new Set();
  for (const l of curLines.slice(1)) {
    if (!l.includes("Travelers Championship")) continue;
    curKeys.add(rowKey(parseCsvRow(l)));
  }

  const recoverDates = new Set(
    String(process.env.DK_AUDIT_RECOVER_DATES || "2026-06-25,2026-06-26,2026-06-27")
      .split(",")
      .map((d) => d.trim())
      .filter(Boolean),
  );

  const toAdd = [];
  for (const l of gitLines.slice(1)) {
    if (!l.includes("Travelers Championship")) continue;
    const parts = parseCsvRow(l);
    const date = parts[0].slice(0, 10);
    if (!recoverDates.has(date)) continue;
    if (curKeys.has(rowKey(parts))) continue;
    toAdd.push(l);
  }

  if (!toAdd.length) {
    console.log("[recover-dk-audit] No missing rows to merge.");
    return;
  }

  const roundStart = buildRoundStartUtcMsFromDateStart("2026-06-25", "America/New_York");
  const assigned = {};
  for (const l of toAdd) {
    const p = parseCsvRow(l);
    const pr = auditPropRoundFromCapture(
      { display_round: p[5], round_num: "", captured_at: p[0] },
      roundStart,
      Date.parse(p[0]),
    );
    const k = Number.isFinite(pr) ? `R${pr}` : "reject";
    assigned[k] = (assigned[k] || 0) + 1;
  }

  const out = `${curLines.join("\n")}\n${toAdd.join("\n")}\n`;
  writeFileSync(AUDIT, out, "utf8");
  console.log(`[recover-dk-audit] Appended ${toAdd.length} row(s) from ${GIT_REF} -> ${AUDIT}`);
  console.log("[recover-dk-audit] Round assignment of recovered rows:", assigned);
}

main();
