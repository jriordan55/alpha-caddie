/**
 * Courtside golf round O/U → projections.props via OpticOdds (mobile-only book).
 *
 * Courtside's public API (api.courtside.app) does not expose sports routes without
 * the native app session. OpticOdds lists sportsbook id `courtside` and returns the
 * same Round N fixture shape used for other golf prop books.
 *
 *   npm run fetch:cs-ou
 *
 * Env:
 *   GOLF_SKIP_CS_OU=1 — skip
 *   OPTICODDS_API_KEY — required for live Courtside pulls
 *   OPTICODDS_GOLF_LEAGUE — default pga
 *   CS_TARGET_ROUND — override
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { fetchOpticOddsGolfRoundProps } from "./opticodds-golf-round-props.mjs";
import { num } from "./pickem-ou-shared.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));

/**
 * @param {{ payload?: object, targetRound?: number }} opts
 */
export async function fetchCourtsideOuProps(opts = {}) {
  if (String(process.env.GOLF_SKIP_CS_OU || "").trim() === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_CS_OU=1)" };
  }
  const payload = opts.payload || {};
  const wantRound =
    Math.round(num(opts.targetRound, NaN)) ||
    Math.round(num(process.env.CS_TARGET_ROUND, NaN)) ||
    Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) ||
    1;
  const hit = await fetchOpticOddsGolfRoundProps({
    sportsbook: "courtside",
    source: "courtside",
    payload,
    targetRound: wantRound,
  });
  return hit;
}

async function main() {
  const projPath = join(
    __dirname,
    "..",
    String(process.env.GOLF_PROJECTIONS_FILE || "projections.json").trim() || "projections.json",
  );
  const payload = existsSync(projPath) ? JSON.parse(readFileSync(projPath, "utf8")) : {};
  const hit = await fetchCourtsideOuProps({ payload });
  console.log(`[courtside-ou] ${hit.props.length} props${hit.error ? ` (${hit.error})` : ""}`);
  const byM = {};
  for (const p of hit.props) byM[p.market] = (byM[p.market] || 0) + 1;
  if (Object.keys(byM).length) console.log(byM);
  for (const p of hit.props.slice(0, 6)) {
    console.log(`  ${p.market} ${p.player_name} R${p.round_num} ${p.line} O${p.over_odds}/U${p.under_odds}`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
