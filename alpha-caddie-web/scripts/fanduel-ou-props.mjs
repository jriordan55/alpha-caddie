/**
 * FanDuel golf round O/U (Round Score, Birdies or Better) via sportsbook Playwright + sbapi capture.
 *
 *   npm run fetch:fd-ou
 *
 * Env:
 *   GOLF_SKIP_FD_OU=1 — skip
 *   FD_LOCATION — default nj (sbapi region)
 *   FD_HEADLESS — 0 headed on win/mac
 *   FD_TARGET_ROUND — override
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
const FD_AK = String(process.env.FD_API_KEY || "FhMFpcPWXMeyZxOx").trim();
const FD_LOC = String(process.env.FD_LOCATION || "nj").trim() || "nj";

function resolveHeadless() {
  const v = String(process.env.FD_HEADLESS ?? "").trim().toLowerCase();
  if (v === "0" || v === "false" || v === "no") return false;
  if (v === "1" || v === "true" || v === "yes") return true;
  return process.platform !== "win32" && process.platform !== "darwin";
}

function americanFromRunner(runner) {
  const odds = runner?.winRunnerOdds?.americanDisplayOdds?.americanOdds
    ?? runner?.winRunnerOdds?.trueOdds?.decimalOdds?.decimalOdds;
  if (typeof odds === "number" && Number.isFinite(odds) && odds !== 0) {
    // already american
    if (Math.abs(odds) >= 100) return Math.round(odds);
  }
  const amStr = runner?.winRunnerOdds?.americanDisplayOdds?.americanOddsInt
    ?? runner?.winRunnerOdds?.americanDisplayOdds?.americanOdds;
  const am = Number(amStr);
  if (Number.isFinite(am) && am !== 0) return Math.round(am);
  const dec = Number(runner?.winRunnerOdds?.trueOdds?.decimalOdds?.decimalOdds);
  if (Number.isFinite(dec) && dec > 1) {
    if (dec >= 2) return Math.round((dec - 1) * 100);
    return Math.round(-100 / (dec - 1));
  }
  return NaN;
}

/**
 * Parse FanDuel marketName / runnerName pairs into our markets.
 * @param {object} market
 * @param {object} payload
 * @param {number} wantRound
 */
export function propsFromFanduelMarket(market, payload = {}, wantRound = NaN) {
  const fieldPlayers = Array.isArray(payload?.players) ? payload.players : [];
  const mName = String(market?.marketName || market?.name || "").trim();
  const marketType = String(market?.marketType || "").trim();
  const runners = Array.isArray(market?.runners) ? market.runners : [];
  if (!runners.length) return [];

  let canon = canonicalRoundOuMarket(mName) || canonicalRoundOuMarket(marketType);
  // FanDuel often labels "Player A - Round 1 Score" style via runner handicaps
  const rows = [];

  // Pattern A: market is "Round 1 Score" / "Birdies or Better" with runners as players + handicap line
  if (
    /round\s*\d*\s*score|player\s*round\s*score|strokes/i.test(mName) ||
    /ROUND_SCORE|PLAYER_ROUND_SCORE/i.test(marketType)
  ) {
    canon = "Total Score";
  } else if (/birdie/i.test(mName) || /BIRDIE/i.test(marketType)) {
    canon = "Birdies";
  } else if (/^pars?\b/i.test(mName) || /PARS/i.test(marketType)) {
    canon = "Pars";
  }
  if (!canon) return [];

  const roundFromName =
    Math.round(num((mName.match(/\bRound\s+(\d+)\b/i) || [])[1], NaN)) ||
    Math.round(num((mName.match(/\bR(\d+)\b/i) || [])[1], NaN)) ||
    NaN;

  // Over/Under paired runners under same market
  const overRunners = runners.filter((r) => /^over\b/i.test(String(r.runnerName || "")));
  const underRunners = runners.filter((r) => /^under\b/i.test(String(r.runnerName || "")));
  if (overRunners.length && underRunners.length) {
    // Single-player market: "Over 68.5" / "Under 68.5" with player in market name
    const playerMatch = mName.match(/[-–—]\s*(.+)$/) || mName.match(/^(.+?)\s+[-–—]/);
    const playerLabel = playerMatch ? playerMatch[1].trim() : "";
    const line = num(overRunners[0].handicap, num(underRunners[0].handicap, NaN));
    const lineFromName = num((String(overRunners[0].runnerName).match(/(\d+(?:\.\d+)?)/) || [])[1], NaN);
    const L = Number.isFinite(line) && line !== 0 ? line : lineFromName;
    const over = americanFromRunner(overRunners[0]);
    const under = americanFromRunner(underRunners[0]);
    const rnd = Number.isFinite(roundFromName) ? roundFromName : wantRound;
    if (Number.isFinite(L) && Number.isFinite(over) && Number.isFinite(under) && playerLabel) {
      if (Number.isFinite(wantRound) && Number.isFinite(rnd) && rnd !== wantRound) return [];
      const matched = matchPlayerByGolferLabel(fieldPlayers, playerLabel);
      rows.push(
        withImpliedFromAmerican({
          player_name: matched ? String(matched.player_name || "").trim() : playerLabel,
          dg_id: matched ? Math.round(num(matched.dg_id, NaN)) : NaN,
          market: canon,
          line: L,
          over_odds: over,
          under_odds: under,
          round_num: Number.isFinite(rnd) ? rnd : wantRound,
          source: "fanduel",
        }),
      );
    }
    return rows;
  }

  // Pattern B: each runner is a player with handicap as the line; need paired O/U markets elsewhere
  return rows;
}

export function propsFromFanduelAttachments(attachments, payload = {}, wantRound = NaN) {
  const markets = attachments?.markets || {};
  const rows = [];
  for (const m of Object.values(markets)) {
    rows.push(...propsFromFanduelMarket(m, payload, wantRound));
  }
  return dedupePropsOnePerPlayerMarket(rows);
}

async function captureFanduelMarketAttachments(eventNameHint = "") {
  const browser = await chromium.launch({
    headless: resolveHeadless(),
    args: ["--disable-blink-features=AutomationControlled"],
  });
  const ctx = await browser.newContext({
    viewport: { width: 1400, height: 900 },
    locale: "en-US",
  });
  const page = await ctx.newPage();
  /** @type {object[]} */
  const attachmentsList = [];
  page.on("response", async (res) => {
    const u = res.url();
    if (!/sbapi|api\.sportsbook\.fanduel\.com/.test(u)) return;
    if (res.status() !== 200) return;
    const ct = res.headers()["content-type"] || "";
    if (!ct.includes("json")) return;
    try {
      const body = await res.json();
      const att = body?.attachments;
      if (att?.markets && Object.keys(att.markets).length) attachmentsList.push(att);
    } catch {
      /* ignore */
    }
  });

  try {
    const tabs = ["player-round-score", "player-birdies", "birdies-or-better"];
    for (const tab of tabs) {
      await page.goto(`https://sportsbook.fanduel.com/navigation/pga?tab=${tab}`, {
        waitUntil: "domcontentloaded",
        timeout: 90000,
      });
      await page.waitForTimeout(5000);
      if (eventNameHint) {
        const link = page.getByText(eventNameHint, { exact: false }).first();
        if (await link.count()) {
          await link.click({ timeout: 8000 }).catch(() => {});
          await page.waitForTimeout(4000);
        }
      }
    }
    // Also hit sbapi content page directly in-page fetch context
    await page.evaluate(
      async ({ ak, loc }) => {
        const tabs = ["player-round-score", "player-birdies"];
        for (const tab of tabs) {
          await fetch(
            `https://sbapi.${loc}.sportsbook.fanduel.com/api/content-managed-page?page=CUSTOM&customPageId=pga&tab=${tab}&_ak=${ak}`,
            { credentials: "include" },
          ).catch(() => null);
        }
      },
      { ak: FD_AK, loc: FD_LOC },
    );
    await page.waitForTimeout(3000);
  } finally {
    await browser.close().catch(() => {});
  }
  return attachmentsList;
}

/**
 * @param {{ payload?: object, targetRound?: number }} opts
 */
export async function fetchFanduelOuProps(opts = {}) {
  if (String(process.env.GOLF_SKIP_FD_OU || "").trim() === "1") {
    return { props: [], error: "skipped (GOLF_SKIP_FD_OU=1)" };
  }
  const payload = opts.payload || {};
  const wantRound =
    Math.round(num(opts.targetRound, NaN)) ||
    Math.round(num(process.env.FD_TARGET_ROUND, NaN)) ||
    Math.round(num(payload.display_round ?? payload.datagolf_field_current_round, NaN)) ||
    1;
  const eventHint = String(payload.event_name || "")
    .replace(/\b20\d{2}\b/g, "")
    .trim();

  let attachmentsList;
  try {
    attachmentsList = await captureFanduelMarketAttachments(eventHint);
  } catch (e) {
    return { props: [], error: e?.message || String(e) };
  }

  let props = [];
  for (const att of attachmentsList) {
    props.push(...propsFromFanduelAttachments(att, payload, wantRound));
  }
  props = dedupePropsOnePerPlayerMarket(props);
  props = preferPropsForTargetRound(props, wantRound);
  return {
    props,
    error: props.length ? undefined : "no FanDuel round score/birdies markets parsed (may be unposted)",
  };
}

async function main() {
  const projPath = join(__dirname, "..", "projections.json");
  const payload = existsSync(projPath) ? JSON.parse(readFileSync(projPath, "utf8")) : {};
  const hit = await fetchFanduelOuProps({ payload });
  console.log(`[fanduel-ou] ${hit.props.length} props${hit.error ? ` (${hit.error})` : ""}`);
  for (const p of hit.props.slice(0, 6)) {
    console.log(`  ${p.market} ${p.player_name} R${p.round_num} ${p.line}`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
