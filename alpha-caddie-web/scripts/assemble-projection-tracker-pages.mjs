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
  "matchup_backtest_detail.csv",
  "matchup_backtest_summary.csv",
  "walkforward_oos_roi.json",
  "skill_window_oos_roi.json",
  "odds_model_roi_summary.csv",
  "odds_model_roi_lines.csv",
  "pgatour_event_rounds.json",
  "live_event_book_props.json",
  "edge_signal_scan.json",
  "course_table.csv",
  "parlay_correlations.json",
  "win_prob_calibration.json",
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
mkdirSync(join(out, "matchup-tracker"), { recursive: true });

writeFileSync(join(out, ".nojekyll"), "");
writeFileSync(
  join(out, "index.html"),
  `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <title>Alpha Caddie · Trackers</title>
  <link rel="canonical" href="projection-tracker/" />
  <style>
    body { font-family: system-ui, sans-serif; background:#090b10; color:#f1f5f9; padding:2rem; }
    a { color:#10b981; }
    li { margin:0.75rem 0; }
  </style>
</head>
<body>
  <h1>Alpha Caddie trackers</h1>
  <ul>
    <li><a href="projection-tracker/">Projection tracker</a> — round O/U vs actual</li>
    <li><a href="matchup-tracker/">Matchup tracker</a> — round matchups + 3-balls (DK / FanDuel / BetMGM)</li>
  </ul>
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

const matchupSrc = join(WEB, "matchup-tracker");
if (existsSync(matchupSrc)) {
  cpSync(matchupSrc, join(out, "matchup-tracker"), { recursive: true });
} else {
  console.warn("[assemble-tracker-pages] matchup-tracker missing — Pages will omit matchup UI");
}

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
