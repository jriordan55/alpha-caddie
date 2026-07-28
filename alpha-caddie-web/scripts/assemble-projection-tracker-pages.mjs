#!/usr/bin/env node
/**
 * Assemble a static GitHub Pages tree for the Projection Tracker.
 *
 * Layout (matches local / Render relative fetches from projection-tracker/):
 *   tracker-pages/
 *     .nojekyll
 *     index.html              → redirect to projection-tracker/
 *     projections.json
 *     data/<tracker CSVs + JSON>
 *     scripts/<browser modules imported via ../scripts/>
 *     projection-tracker/*    → UI
 *
 * Usage:
 *   node scripts/assemble-projection-tracker-pages.mjs
 *   node scripts/assemble-projection-tracker-pages.mjs --out /tmp/tracker-pages
 */
import { cpSync, copyFileSync, existsSync, mkdirSync, writeFileSync, rmSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = resolve(__dirname, "..");
const REPO = resolve(WEB, "..");

function argOut() {
  const i = process.argv.indexOf("--out");
  if (i >= 0 && process.argv[i + 1]) return resolve(process.argv[i + 1]);
  return join(REPO, "tracker-pages");
}

const DATA_FILES = [
  "round_projection_vs_actual.csv",
  "round_projection_vs_actual_summary.csv",
  "round_projection_vs_actual.csv.new",
  "round_projection_vs_actual_summary.csv.new",
  "walkforward_oos_roi.json",
  "skill_window_oos_roi.json",
  "odds_model_roi_summary.csv",
  "odds_model_roi_lines.csv",
  "pgatour_event_rounds.json",
  "live_event_book_props.json",
  "edge_signal_scan.json",
  "course_table.csv",
  "parlay_correlations.json",
];

/** Browser modules imported by projection-tracker via ../scripts/ (must ship on Pages). */
const SCRIPT_MODULES = [
  "projected-mean-live.mjs",
  "weather-mu-adjustments.mjs",
  "live-in-play-pricing.mjs",
  "projection-book-props.mjs",
  "dg-events-align.mjs",
  "live-event-actuals-cap.mjs",
  "course-name-key.mjs",
];

const out = argOut();
if (existsSync(out)) rmSync(out, { recursive: true, force: true });
mkdirSync(join(out, "data"), { recursive: true });
mkdirSync(join(out, "scripts"), { recursive: true });
mkdirSync(join(out, "projection-tracker"), { recursive: true });

writeFileSync(join(out, ".nojekyll"), "");
writeFileSync(
  join(out, "index.html"),
  `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta http-equiv="refresh" content="0; url=projection-tracker/" />
  <title>Alpha Caddie · Projection Tracker</title>
  <link rel="canonical" href="projection-tracker/" />
</head>
<body>
  <p><a href="projection-tracker/">Open Projection Tracker</a></p>
</body>
</html>
`,
);

const trackerSrc = join(WEB, "projection-tracker");
if (!existsSync(trackerSrc)) {
  console.error("[assemble-tracker-pages] missing", trackerSrc);
  process.exit(1);
}
cpSync(trackerSrc, join(out, "projection-tracker"), { recursive: true });

const projSrc = join(WEB, "projections.json");
if (existsSync(projSrc)) {
  copyFileSync(projSrc, join(out, "projections.json"));
} else {
  console.warn("[assemble-tracker-pages] projections.json missing — Best bets will be empty");
}

const dataDir = join(WEB, "data");
let copied = 0;
for (const name of DATA_FILES) {
  const src = join(dataDir, name);
  if (!existsSync(src)) continue;
  copyFileSync(src, join(out, "data", name));
  copied++;
}

const scriptsDir = join(WEB, "scripts");
let scriptsCopied = 0;
const missingScripts = [];
for (const name of SCRIPT_MODULES) {
  const src = join(scriptsDir, name);
  if (!existsSync(src)) {
    missingScripts.push(name);
    continue;
  }
  copyFileSync(src, join(out, "scripts", name));
  scriptsCopied++;
}
if (missingScripts.length) {
  console.error("[assemble-tracker-pages] missing browser script modules:", missingScripts.join(", "));
  process.exit(1);
}

writeFileSync(
  join(out, "deploy-meta.json"),
  JSON.stringify(
    {
      assembled_at: new Date().toISOString(),
      source: "alpha-caddie-web",
      data_files_copied: copied,
      script_modules_copied: scriptsCopied,
    },
    null,
    2,
  ),
);

console.log(
  `[assemble-tracker-pages] wrote ${out} (tracker + projections + ${copied} data files + ${scriptsCopied} scripts)`,
);
