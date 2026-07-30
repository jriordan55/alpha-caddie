/**
 * Caesars golf Round Props → projections.props (Round Score, Birdies or Better, Pars).
 * Uses Playwright to open sportsbook.caesars.com and capture /v4/sports/golf/tabs JSON.
 *
 *   npm run fetch:czr-ou
 *
 * Env:
 *   GOLF_SKIP_CZR_OU=1 — skip
 *   CAESARS_LOCATION — default nj
 *   CZR_HEADLESS — 0 headed (default on win/mac), 1 headless
 *   CZR_TARGET_ROUND — override
 */
import { existsSync, readFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { chromium } from "playwright";
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";
import {
  canonicalRoundOuMarket,
  dedupePropsOnePerPlayerMarket,
  num,
  preferPropsForTargetRound,
  withImpliedFromAmerican,
} from "./pickem-ou-shared.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const LOC = String(process.env.CAESARS_LOCATION || "nj").trim() || "nj";

function resolveHeadless() {
  const v = String(process.env.CZR_HEADLESS ?? "").trim().toLowerCase();
  if (v === "0" || v === "false" || v === "no") return false;
  if (v === "1" || v === "true" || v === "yes") return true;
  return process.platform !== "win32" && process.platform !== "darwin";
}

function stripPipes(s) {
  return String(s || "")
    .replace(/\|/g, "")
    .replace(/\s+/g, " ")
    .trim();
}

function americanFromSel(sel) {
  const a = Number(sel?.price?.a);
  if (Number.isFinite(a) && a !== 0) return Math.round(a);
  const d = Number(sel?.price?.d);
  if (Number.isFinite(d) && d > 1) {
    if (d >= 2) return Math.round((d - 1) * 100);
    return Math.round(-100 / (d - 1));
  }
  return NaN;
}

/**
 * @param {string} marketName
 * @returns {{ market: string, player: string, round: number } | null}
 */
export function parseCaesarsRoundPropMarketName(marketName) {
  const n = stripPipes(marketName);
  let m = n.match(/^Round\s+(\d+)\s+Score\s+-\s+(.+)$/i);
  if (m) return { market: "Total Score", round: Math.round(Number(m[1])), player: m[2].trim() };
  m = n.match(/^Number Of Birdies \(or better\) in the Round \(Round\s+(\d+)\)\s+-\s+(.+)$/i);
  if (m) return { market: "Birdies", round: Math.round(Number(m[1])), player: m[2].trim() };
  m = n.match(/^Number Of Pars in the Round \(Round\s+(\d+)\)\s+-\s+(.+)$/i);
  if (m) return { market: "Pars", round: Math.round(Number(m[1])), player: m[2].trim() };
  return null;
}

function lineFromSelections(selections) {
  for (const sel of selections || []) {
    const name = stripPipes(sel.name);
    const m = name.match(/^(Over|Under)\s+(\d+(?:\.\d+)?)$/i);
    if (m) return Number(m[2]);
  }
  return NaN;
}

function ouOddsFromSelections(selections) {
  let over = NaN;
  let under = NaN;
  for (const sel of selections || []) {
    const name = stripPipes(sel.name).toLowerCase();
    const am = americanFromSel(sel);
    if (!Number.isFinite(am)) continue;
    if (name.startsWith("over")) over = am;
    else if (name.startsWith("under")) under = am;
  }
  return { over, under };
}

function walkMarkets(node, out = []) {
  if (!node || typeof node !== "object") return out;
  if (Array.isArray(node)) {
    for (const x of node) walkMarkets(x, out);
    return out;
  }
  if (node.name && Array.isArray(node.selections) && node.selections.length >= 2) {
    out.push(node);
  }
  for (const [k, v] of Object.entries(node)) {
    if (k === "tabs" || k === "secondaryTabs") continue;
    if (typeof v === "object") walkMarkets(v, out);
  }
  return out;
}

/**
 * @param {object[]} bodies tab JSON payloads
 * @param {object} payload
 * @param {number} wantRound
 */
export function propsFromCaesarsTabBodies(bodies, payload = {}, wantRound = NaN) {
  const fieldPlayers = Array.isArray(payload?.players) ? payload.players : [];
  const rows = [];
  for (const body of bodies || []) {
    for (const mkt of walkMarkets(body)) {
      const parsed = parseCaesarsRoundPropMarketName(mkt.name);
      if (!parsed) continue;
      if (!canonicalRoundOuMarket(parsed.market)) continue;
      if (Number.isFinite(wantRound) && parsed.round !== wantRound) continue;
      const line = lineFromSelections(mkt.selections);
      const { over, under } = ouOddsFromSelections(mkt.selections);
      if (!Number.isFinite(line) || !Number.isFinite(over) || !Number.isFinite(under)) continue;
      const matched = matchPlayerByGolferLabel(fieldPlayers, parsed.player);
      rows.push(
        withImpliedFromAmerican({
          player_name: matched ? String(matched.player_name || "").trim() : parsed.player,
          dg_id: matched ? Math.round(num(matched.dg_id, NaN)) : NaN,
          market: parsed.market,
          line,
          over_odds: over,
          under_odds: under,
          round_num: parsed.round,
          source: "caesars",
        }),
      );
    }
  }
  return dedupePropsOnePerPlayerMarket(rows);
}

async function captureCaesarsTabBodies() {
  const browser = await chromium.launch({
    headless: resolveHeadless(),
    args: ["--disable-blink-features=AutomationControlled"],
  });
  const ctx = await browser.newContext({
    viewport: { width: 1400, height: 900 },
    locale: "en-US",
    userAgent:
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
  });
  const page = await ctx.newPage();
  /** @type {object[]} */
  const bodies = [];
  page.on("response", async (res) => {
    const u = res.url();
    if (!/\/v4\/sports\/golf\/tabs\//.test(u)) return;
    if (res.status() !== 200) return;
    try {
      const body = await res.json();
      bodies.push(body);
    } catch {
      /* ignore */
    }
  });

  try {
    await page.goto(`https://sportsbook.caesars.com/us/${LOC}/bet/golf`, {
      waitUntil: "domcontentloaded",
      timeout: 90000,
    });
    await page.waitForTimeout(3500);
    await page.getByText("Round Props", { exact: true }).first().click({ timeout: 15000 });
    await page.waitForTimeout(3500);
    for (const label of ["Round Score", "Birdies or Better - Round", "Pars - Round"]) {
      await page.getByText(label, { exact: true }).first().click({ timeout: 10000 }).catch(() => {});
      await page.waitForTimeout(4500);
    }
  } finally {
    await browser.close().catch(() => {});
  }
  return bodies;
}

/**
 * @param {{ payload?: object, targetRound?: number }} opts
 */
export async function fetchCaesarsOuProps(opts = {}) {
  if (String(process.env.GOLF_SKIP_CZR_OU || "").trim() === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_CZR_OU=1)" };
  }
  const payload = opts.payload || {};
  const wantRound =
    Math.round(num(opts.targetRound, NaN)) ||
    Math.round(num(process.env.CZR_TARGET_ROUND, NaN)) ||
    Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) ||
    1;

  let bodies;
  try {
    bodies = await captureCaesarsTabBodies();
  } catch (e) {
    return { props: [], error: e?.message || String(e) };
  }
  let props = propsFromCaesarsTabBodies(bodies, payload, wantRound);
  props = preferPropsForTargetRound(props, wantRound);
  return {
    props,
    error: props.length ? undefined : "no Caesars Round Props markets parsed",
  };
}

async function main() {
  const projPath = join(__dirname, "..", "projections.json");
  const payload = existsSync(projPath) ? JSON.parse(readFileSync(projPath, "utf8")) : {};
  const hit = await fetchCaesarsOuProps({ payload });
  console.log(`[caesars-ou] ${hit.props.length} props${hit.error ? ` (${hit.error})` : ""}`);
  const byM = {};
  for (const p of hit.props) byM[p.market] = (byM[p.market] || 0) + 1;
  console.log(byM);
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
