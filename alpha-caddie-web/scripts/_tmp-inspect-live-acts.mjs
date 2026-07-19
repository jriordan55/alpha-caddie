import { readFileSync } from "fs";
import { join } from "path";

const WEB = join(import.meta.dirname, "..");
const live = JSON.parse(readFileSync(join(WEB, "live-in-play.json"), "utf8"));
const pga = JSON.parse(readFileSync(join(WEB, "data/pgatour_event_rounds.json"), "utf8"));

const dgs = ["23950", "23602", "15191", "19483"];
for (const dg of dgs) {
  console.log("\n===", dg, "===");
  for (const rnd of ["1", "2", "3"]) {
    const act = live.live_round_actuals_by_dg?.[dg]?.[rnd];
    if (!act) {
      console.log("R" + rnd, "no act");
      continue;
    }
    console.log("R" + rnd, {
      score: act.round_score,
      birdies: act.birdies,
      pars: act.pars,
      bogeys: act.bogeys,
      eagles: act.eagles,
      gir: act.gir,
      fairways: act.fairways,
      source: act.source,
      thru: act.thru,
    });
  }
  const pgaRows = (pga.rounds || []).filter((r) => String(r.dg_id) === dg);
  console.log(
    "pga",
    pgaRows.map((r) => ({
      rnd: r.round_num,
      score: r.round_score,
      bird: r.birdies,
      bog: r.bogeys || r.bogies,
      par: r.pars,
    })),
  );
}

// Count how many live acts have real birdies for Open
let n = 0;
let withBird = 0;
let stub = 0;
for (const [dg, per] of Object.entries(live.live_round_actuals_by_dg || {})) {
  for (const [rnd, act] of Object.entries(per || {})) {
    n++;
    const b = act?.birdies;
    const p = act?.pars;
    const bg = act?.bogeys;
    if (Number.isFinite(Number(b)) && Number(b) > 0) withBird++;
    if ((b === 0 || b == null) && (bg === 0 || bg == null) && (p === 0 || p == null || p === 18)) stub++;
  }
}
console.log({ liveActs: n, withBird, stub });
