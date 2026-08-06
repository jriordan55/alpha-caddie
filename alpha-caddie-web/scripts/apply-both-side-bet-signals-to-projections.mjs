#!/usr/bin/env node
/**
 * After DG μ + both-side bias: stamp tracker bet YES/NO on DK props,
 * sync live_event_book_props.json, rewrite round_projections.csv.
 *
 *   node scripts/apply-both-side-bet-signals-to-projections.mjs
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import {
  annotatePropsWithBetSignals,
  syncLiveEventBookProps,
} from "./both-side-bet-signal.mjs";
import { writeRoundProjectionsCsv } from "./export-round-projections-csv.mjs";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PROJ = join(WEB, "projections.json");

function main() {
  if (!existsSync(PROJ)) throw new Error(`Missing ${PROJ}`);
  const payload = JSON.parse(readFileSync(PROJ, "utf8"));
  const betYes = annotatePropsWithBetSignals(payload, WEB);
  const liveN = syncLiveEventBookProps(payload, WEB);
  writeFileSync(PROJ, `${JSON.stringify(payload, null, 2)}\n`);
  let csv = null;
  if (String(process.env.GOLF_SKIP_ROUND_PROJECTIONS_CSV || "").trim() !== "1") {
    csv = writeRoundProjectionsCsv(payload);
  }
  console.log(
    `[both-side-bet] bet=YES ${betYes} · live_dk ${liveN}` +
      (csv ? ` · CSV ${csv.rows} rows → ${csv.path}` : ""),
  );
}

main();
