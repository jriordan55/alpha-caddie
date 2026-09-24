import { readFileSync } from "fs";
import { join } from "path";
import { loadPlayers, loadProjections, WEB_ROOT } from "../lib/data.mjs";
import { simulatePresidentsCup } from "../lib/cup-sim.mjs";
import { buildDashboardPayload } from "../lib/dashboard-data.mjs";

const proj = loadProjections(join(WEB_ROOT, "projections-pga.json"));
const field = loadPlayers(proj);
const sim = simulatePresidentsCup(field, { nSims: 3000, seed: 42 });
const prior = JSON.parse(readFileSync(join("output", "model.json"), "utf8"));
const props = [];
for (const tab of prior.tabs || []) {
  for (const r of tab.rows || []) {
    props.push({
      market: r.market,
      selection: r.selection,
      american: r.book_american,
      american_display: r.dk_odds_display,
      dk_implied_prob: r.book_prob,
      marketId: r.dk_market_id,
      dk_subcategory: tab.id,
    });
  }
}
const payload = buildDashboardPayload({ field, sim, dkMeta: { props }, manifest: {}, nSims: 3000 });

const blank = [];
for (const tab of payload.tabs) {
  for (const r of tab.rows) {
    if (r.model_prob == null) blank.push({ tab: tab.id, market: r.market, sel: r.selection });
  }
}
console.log("tabs", payload.tabs.length, "rows", payload.tabs.reduce((n, t) => n + t.rows.length, 0));
console.log("blank model_prob", blank.length);
if (blank.length) console.log("sample", blank.slice(0, 8));

const usaCap =
  payload.tabs.find((t) => t.id === "usa-props")?.rows.filter((r) => r.market_group === "Top USA Captain's Pick") ||
  [];
console.log(
  "Top USA Captain Pick:",
  usaCap.map((r) => ({ sel: r.selection, model: r.model_pct?.toFixed(1), dk: r.dk_pct?.toFixed(1) })),
);
