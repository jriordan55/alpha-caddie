/**
 * Fill DP World (DataGolf `euro`) seasons that are missing from historical_rounds_all.csv.
 * Years already on disk are skipped. Older PGA and LIV rows are left in place.
 *
 *   node scripts/ensure-dp-world-history.mjs
 *
 * First year defaults to 2017 (same window as the round model). Override with GOLF_EURO_HISTORY_FROM.
 */
import { createReadStream, existsSync } from "fs";
import { spawnSync } from "child_process";
import path from "path";
import { fileURLToPath } from "url";
import { parse } from "csv-parse";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB = path.resolve(__dirname, "..");
const REPO = path.resolve(WEB, "..");
const CSV = path.join(REPO, "data", "historical_rounds_all.csv");

const cy = new Date().getFullYear();
const from = Math.max(2017, Number(process.env.GOLF_EURO_HISTORY_FROM) || 2017);

function yearsWanted() {
  const out = [];
  for (let y = from; y <= cy; y++) out.push(y);
  return out;
}

async function euroYearsOnDisk() {
  const have = new Set();
  if (!existsSync(CSV)) return have;
  await new Promise((resolve, reject) => {
    createReadStream(CSV)
      .pipe(parse({ columns: true, relax_quotes: true, skip_records_with_error: true }))
      .on("data", (r) => {
        if (String(r.tour || "").toLowerCase() !== "euro") return;
        const y = Math.round(Number(r.year));
        if (Number.isFinite(y)) have.add(y);
      })
      .on("end", resolve)
      .on("error", reject);
  });
  return have;
}

const have = await euroYearsOnDisk();
const missing = yearsWanted().filter((y) => !have.has(y));
if (!missing.length) {
  console.log(`[dp-history] DP World seasons ${from}–${cy} are already in historical_rounds_all.csv.`);
  process.exit(0);
}

console.log(`[dp-history] Fetching missing DP World seasons: ${missing.join(", ")}`);
const r = spawnSync(process.execPath, [path.join(WEB, "scripts", "update-historical-rounds-node.mjs")], {
  cwd: WEB,
  stdio: "inherit",
  env: {
    ...process.env,
    GOLF_MODEL_DIR: REPO,
    GOLF_HISTORICAL_ROUNDS_TOURS: "euro",
    GOLF_HISTORICAL_ROUNDS_YEARS: missing.join(","),
    GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS: "",
    GOLF_HISTORICAL_ROUNDS_FULL_HISTORY: "",
    GOLF_HISTORICAL_ROUNDS_LIGHT: "",
  },
});
process.exit(r.status ?? 1);
