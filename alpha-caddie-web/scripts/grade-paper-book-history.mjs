#!/usr/bin/env node
/**
 * Grade open paper book bets using round_projection_vs_actual.csv.
 * Runs on push:live after vs-actual export so paper-book-history.json settles on deploy.
 *
 *   npm run grade:paper-book
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath, pathToFileURL } from "url";
import { buildOuGradeIndexFromCsvText, gradePersistedState } from "../paper-book/paper-book-grade.mjs";
import { normalizePersistedState } from "../paper-book/paper-book-state.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const HISTORY_PATH = join(WEB_ROOT, "paper-book", "paper-book-history.json");
const VS_ACTUAL_PATH = join(WEB_ROOT, "data", "round_projection_vs_actual.csv");

export function gradePaperBookHistoryFile(opts = {}) {
  const historyPath = opts.historyPath || HISTORY_PATH;
  const csvPath = opts.csvPath || VS_ACTUAL_PATH;

  if (!existsSync(csvPath)) {
    console.warn("[grade:paper-book] missing round_projection_vs_actual.csv — skip grading");
    return { changedCount: 0 };
  }
  if (!existsSync(historyPath)) {
    console.warn("[grade:paper-book] missing paper-book-history.json — skip grading");
    return { changedCount: 0 };
  }

  const ouGradeIndex = buildOuGradeIndexFromCsvText(readFileSync(csvPath, "utf8"));
  if (!ouGradeIndex) {
    console.warn("[grade:paper-book] could not build grade index from CSV");
    return { changedCount: 0 };
  }

  const persisted = normalizePersistedState(JSON.parse(readFileSync(historyPath, "utf8")));
  const { persisted: graded, changedCount } = gradePersistedState(persisted, ouGradeIndex);

  if (changedCount > 0) {
    writeFileSync(historyPath, `${JSON.stringify(graded, null, 2)}\n`, "utf8");
    console.log(`[grade:paper-book] Settled ${changedCount} open bet(s) → ${historyPath}`);

    const websiteHistory = join(WEB_ROOT, "..", "website", "public", "paper-book", "paper-book-history.json");
    if (existsSync(dirname(websiteHistory))) {
      writeFileSync(websiteHistory, `${JSON.stringify(graded, null, 2)}\n`, "utf8");
      console.log(`[grade:paper-book] Wrote ${websiteHistory}`);
    }
  } else {
    console.log("[grade:paper-book] No open bets to grade (or results not posted yet)");
  }

  return { changedCount };
}

async function main() {
  gradePaperBookHistoryFile();
}

const isMain = process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href;
if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
