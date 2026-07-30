/**
 * FanDuel golf round O/U (Round Score, Birdies or Better) via sbapi coupons + market prices.
 *
 * Discovers markets from layout cards titled Round Scores / Birdies or Better / Player Round Scores:
 *   coupon.display[].rows[].marketIds → attachments.markets / smp getMarketPrices
 *
 *   npm run fetch:fd-ou
 *
 * Env:
 *   GOLF_SKIP_FD_OU=1 — skip
 *   FD_LOCATION — sbapi region (default nj)
 *   FD_SMP_LOCATION — smp prices region (default = FD_LOCATION)
 *   FD_TARGET_ROUND — override
 *   FD_HEADLESS — Playwright fallback (0 headed on win/mac)
 *   FD_USE_PLAYWRIGHT=1 — force Playwright capture even when API path runs
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
const FD_SMP_LOC = String(process.env.FD_SMP_LOCATION || FD_LOC).trim() || FD_LOC;

const WANT_CARD_TITLES = [
  /^round\s*scores?$/i,
  /^player\s*round\s*scores?$/i,
  /^birdies?\s*or\s*better$/i,
  /^birdies?$/i,
];

function resolveHeadless() {
  const v = String(process.env.FD_HEADLESS ?? "").trim().toLowerCase();
  if (v === "0" || v === "false" || v === "no") return false;
  if (v === "1" || v === "true" || v === "yes") return true;
  return process.platform !== "win32" && process.platform !== "darwin";
}

function sbapiBase(loc = FD_LOC) {
  return `https://sbapi.${loc}.sportsbook.fanduel.com/api`;
}

function smpBase(loc = FD_SMP_LOC) {
  return `https://smp.${loc}.sportsbook.fanduel.com/api`;
}

function fdHeaders() {
  return {
    Accept: "application/json",
    "User-Agent":
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    Referer: "https://sportsbook.fanduel.com/",
    Origin: "https://sportsbook.fanduel.com",
  };
}

function americanFromRunner(runner) {
  const amStr =
    runner?.winRunnerOdds?.americanDisplayOdds?.americanOddsInt ??
    runner?.winRunnerOdds?.americanDisplayOdds?.americanOdds;
  const am = Number(amStr);
  if (Number.isFinite(am) && am !== 0) return Math.round(am);
  const dec = Number(
    runner?.winRunnerOdds?.trueOdds?.decimalOdds?.decimalOdds ??
      runner?.winRunnerOdds?.decimalDisplayOdds?.decimalOdds,
  );
  if (Number.isFinite(dec) && dec > 1) {
    if (dec >= 2) return Math.round((dec - 1) * 100);
    return Math.round(-100 / (dec - 1));
  }
  return NaN;
}

function americanFromPriceRunner(rd) {
  const am = Number(rd?.winRunnerOdds?.americanDisplayOdds?.americanOdds);
  if (Number.isFinite(am) && am !== 0) return Math.round(am);
  const dec = Number(rd?.winRunnerOdds?.trueOdds?.decimalOdds?.decimalOdds);
  if (Number.isFinite(dec) && dec > 1) {
    if (dec >= 2) return Math.round((dec - 1) * 100);
    return Math.round(-100 / (dec - 1));
  }
  return NaN;
}

function marketCanon(marketName, marketType, cardTitle = "") {
  const blob = `${cardTitle} ${marketName} ${marketType}`;
  if (/round\s*\d*\s*score|player\s*round\s*score|strokes/i.test(blob) || /ROUND_SCORE|PLAYER_ROUND_SCORE/i.test(blob)) {
    return "Total Score";
  }
  if (/birdie/i.test(blob)) return "Birdies";
  if (/\bpars?\b/i.test(blob) && !/birdie/i.test(blob)) return "Pars";
  return canonicalRoundOuMarket(marketName) || canonicalRoundOuMarket(marketType) || "";
}

function roundFromText(...parts) {
  for (const p of parts) {
    const m = String(p || "").match(/\bRound\s+(\d+)\b/i) || String(p || "").match(/\bR(\d+)\b/i);
    if (m) {
      const r = Math.round(Number(m[1]));
      if (r >= 1 && r <= 4) return r;
    }
  }
  return NaN;
}

/**
 * Parse a FanDuel market object into prop rows.
 */
export function propsFromFanduelMarket(market, payload = {}, wantRound = NaN, cardTitle = "") {
  const fieldPlayers = Array.isArray(payload?.players) ? payload.players : [];
  const mName = String(market?.marketName || market?.name || "").trim();
  const marketType = String(market?.marketType || "").trim();
  const runners = Array.isArray(market?.runners) ? market.runners : [];
  if (!runners.length) return [];

  const canon = marketCanon(mName, marketType, cardTitle);
  if (!canon) return [];

  const roundFromName = roundFromText(mName, marketType, cardTitle);
  const rows = [];

  const overRunners = runners.filter((r) => /^over\b/i.test(String(r.runnerName || "")));
  const underRunners = runners.filter((r) => /^under\b/i.test(String(r.runnerName || "")));

  if (overRunners.length && underRunners.length) {
    // Single-player O/U market: player in market name, Over/Under runners
    if (overRunners.length === 1 && underRunners.length === 1) {
      const playerMatch =
        mName.match(/[-–—]\s*(.+)$/) ||
        mName.match(/^(.+?)\s+[-–—]/) ||
        mName.match(/^(.+?)\s+Round\b/i);
      const playerLabel = playerMatch ? playerMatch[1].trim() : "";
      const line = num(overRunners[0].handicap, num(underRunners[0].handicap, NaN));
      const lineFromName = num((String(overRunners[0].runnerName).match(/(\d+(?:\.\d+)?)/) || [])[1], NaN);
      const L = Number.isFinite(line) && line !== 0 ? Math.abs(line) : lineFromName;
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
            fanduel_market_id: market.marketId,
          }),
        );
      }
      return rows;
    }

    // Multi-player: pair Over/Under by handicap + player name embedded in runner
    const byKey = new Map();
    for (const r of [...overRunners, ...underRunners]) {
      const side = /^over\b/i.test(r.runnerName || "") ? "over" : "under";
      const playerFromRunner = String(r.runnerName || "")
        .replace(/^(Over|Under)\s+/i, "")
        .replace(/\d+(?:\.\d+)?/, "")
        .trim();
      const playerLabel = playerFromRunner || mName;
      const line = Math.abs(num(r.handicap, NaN));
      const key = `${playerLabel}|${line}`;
      const cur = byKey.get(key) || { playerLabel, line };
      cur[side] = americanFromRunner(r);
      byKey.set(key, cur);
    }
    for (const cur of byKey.values()) {
      if (!Number.isFinite(cur.line) || !Number.isFinite(cur.over) || !Number.isFinite(cur.under)) continue;
      const rnd = Number.isFinite(roundFromName) ? roundFromName : wantRound;
      if (Number.isFinite(wantRound) && Number.isFinite(rnd) && rnd !== wantRound) continue;
      const matched = matchPlayerByGolferLabel(fieldPlayers, cur.playerLabel);
      rows.push(
        withImpliedFromAmerican({
          player_name: matched ? String(matched.player_name || "").trim() : cur.playerLabel,
          dg_id: matched ? Math.round(num(matched.dg_id, NaN)) : NaN,
          market: canon,
          line: cur.line,
          over_odds: cur.over,
          under_odds: cur.under,
          round_num: Number.isFinite(rnd) ? rnd : wantRound,
          source: "fanduel",
          fanduel_market_id: market.marketId,
        }),
      );
    }
    return rows;
  }

  // Pattern: runners are players with handicap line; need paired markets (skip alone)
  return rows;
}

export function propsFromFanduelAttachments(attachments, payload = {}, wantRound = NaN, cardTitle = "") {
  const markets = attachments?.markets || {};
  const rows = [];
  for (const m of Object.values(markets)) {
    rows.push(...propsFromFanduelMarket(m, payload, wantRound, cardTitle));
  }
  return dedupePropsOnePerPlayerMarket(rows);
}

function cardTitleWanted(title) {
  const t = String(title || "").trim();
  return WANT_CARD_TITLES.some((re) => re.test(t));
}

function marketIdsFromCoupon(coupon) {
  const ids = [];
  if (coupon?.marketId) ids.push(String(coupon.marketId));
  if (coupon?.externalMarketId) ids.push(String(coupon.externalMarketId));
  for (const d of coupon?.display || []) {
    for (const row of d.rows || []) {
      for (const mid of row.marketIds || []) ids.push(String(mid));
    }
  }
  return [...new Set(ids.filter(Boolean))];
}

async function fetchJson(url, opts = {}) {
  const res = await fetch(url, { ...opts, headers: { ...fdHeaders(), ...(opts.headers || {}) } });
  const text = await res.text();
  let j = null;
  try {
    j = JSON.parse(text);
  } catch {
    /* */
  }
  return { ok: res.ok, status: res.status, j, text };
}

async function fetchContentPages() {
  const pages = [];
  const paths = [
    `content-managed-page?page=SPORT&eventTypeId=3&pbHorizontal=false&_ak=${FD_AK}&timezone=America%2FNew_York`,
    `content-managed-page?page=CUSTOM&customPageId=pga&pbHorizontal=false&_ak=${FD_AK}&timezone=America%2FNew_York`,
  ];
  for (const loc of [...new Set([FD_LOC, "nj", "ma"])]) {
    for (const path of paths) {
      const hit = await fetchJson(`${sbapiBase(loc)}/${path}`);
      if (hit.ok && hit.j?.layout) pages.push({ loc, path, page: hit.j });
    }
  }
  return pages;
}

async function fetchEventPage(eventId) {
  const qs = `eventId=${eventId}&_ak=${FD_AK}&includePrices=true&pbHorizontal=false`;
  for (const loc of [...new Set([FD_LOC, "nj", "ma"])]) {
    const hit = await fetchJson(`${sbapiBase(loc)}/event-page?${qs}`);
    if (hit.ok && hit.j) return hit.j;
  }
  return null;
}

async function fetchMarketPrices(marketIds) {
  const ids = [...new Set((marketIds || []).map(String).filter(Boolean))];
  if (!ids.length) return [];
  const out = [];
  for (let i = 0; i < ids.length; i += 40) {
    const batch = ids.slice(i, i + 40);
    const hit = await fetchJson(
      `${smpBase()}/sports/fixedodds/readonly/v1/getMarketPrices?priceHistory=0`,
      {
        method: "POST",
        headers: { "Content-Type": "application/json", "X-Application": FD_AK },
        body: JSON.stringify({ marketIds: batch }),
      },
    );
    if (Array.isArray(hit.j)) out.push(...hit.j);
  }
  return out;
}

function mergePriceRunnersIntoMarket(market, priceRow) {
  if (!market || !priceRow) return market;
  const bySel = new Map((priceRow.runnerDetails || []).map((rd) => [rd.selectionId, rd]));
  const runners = (market.runners || []).map((r) => {
    const rd = bySel.get(r.selectionId);
    if (!rd) return r;
    const am = americanFromPriceRunner(rd);
    if (!Number.isFinite(am)) return r;
    return {
      ...r,
      handicap: num(rd.handicap, r.handicap),
      winRunnerOdds: {
        ...(r.winRunnerOdds || {}),
        americanDisplayOdds: { americanOdds: am },
      },
    };
  });
  return { ...market, runners };
}

/**
 * Collect candidate markets from FanDuel page coupons for round score / birdies cards.
 */
export function collectFanduelRoundPropTargets(page) {
  const cards = page?.layout?.cards || {};
  const coupons = page?.layout?.coupons || {};
  const markets = page?.attachments?.markets || {};
  const targets = [];

  for (const card of Object.values(cards)) {
    if (!cardTitleWanted(card.title)) continue;
    for (const ref of card.coupons || []) {
      const coupon = coupons[String(ref.id)];
      if (!coupon) continue;
      const marketIds = marketIdsFromCoupon(coupon);
      targets.push({
        cardTitle: card.title,
        couponId: coupon.id,
        eventId: coupon.eventId,
        competitionIds: coupon.competitionIds,
        externalMarketId: coupon.externalMarketId,
        marketIds,
        hasDisplay: (coupon.display || []).length > 0,
        attachedMarkets: marketIds.map((id) => markets[id]).filter(Boolean),
      });
    }
  }
  return targets;
}

async function propsFromApiPath(payload, wantRound) {
  const pages = await fetchContentPages();
  if (!pages.length) return { props: [], meta: { error: "FanDuel content-managed-page failed" } };

  const allTargets = [];
  const marketById = new Map();
  for (const { page } of pages) {
    for (const t of collectFanduelRoundPropTargets(page)) allTargets.push(t);
    for (const [id, m] of Object.entries(page.attachments?.markets || {})) {
      marketById.set(String(id), m);
    }
  }

  const uniqTargets = [];
  const seen = new Set();
  for (const t of allTargets) {
    const key = `${t.cardTitle}|${t.couponId}|${(t.marketIds || []).join(",")}`;
    if (seen.has(key)) continue;
    seen.add(key);
    uniqTargets.push(t);
  }

  const marketIds = [...new Set(uniqTargets.flatMap((t) => t.marketIds))];
  const eventIds = [...new Set(uniqTargets.map((t) => t.eventId).filter(Boolean))];

  for (const eventId of eventIds) {
    const ev = await fetchEventPage(eventId);
    for (const [id, m] of Object.entries(ev?.attachments?.markets || {})) {
      marketById.set(String(id), m);
    }
  }

  const prices = await fetchMarketPrices(marketIds);
  const priceById = new Map(prices.map((p) => [String(p.marketId), p]));

  const rows = [];
  let couponsWithDisplay = 0;
  let couponsEmpty = 0;
  for (const t of uniqTargets) {
    if (t.hasDisplay) couponsWithDisplay++;
    else couponsEmpty++;
    for (const mid of t.marketIds) {
      let market = marketById.get(String(mid));
      if (!market && priceById.has(String(mid))) {
        // Synthesize minimal market from price runners alone is insufficient without names.
        continue;
      }
      if (market && priceById.has(String(mid))) {
        market = mergePriceRunnersIntoMarket(market, priceById.get(String(mid)));
      }
      if (market) rows.push(...propsFromFanduelMarket(market, payload, wantRound, t.cardTitle));
    }
    // Also parse any already-attached markets on the coupon target
    for (const m of t.attachedMarkets || []) {
      rows.push(...propsFromFanduelMarket(m, payload, wantRound, t.cardTitle));
    }
  }

  // Fallback: scan all attached markets on pages for score/birdie labels
  for (const { page } of pages) {
    rows.push(...propsFromFanduelAttachments(page.attachments, payload, wantRound));
  }

  const props = dedupePropsOnePerPlayerMarket(rows);
  return {
    props,
    meta: {
      pages: pages.length,
      targets: uniqTargets.length,
      couponsWithDisplay,
      couponsEmpty,
      marketIds: marketIds.length,
      priced: prices.length,
      eventIds,
    },
  };
}

async function captureFanduelViaPlaywright(payload, wantRound) {
  const browser = await chromium.launch({
    headless: resolveHeadless(),
    args: ["--disable-blink-features=AutomationControlled"],
  });
  const ctx = await browser.newContext({
    viewport: { width: 1440, height: 960 },
    locale: "en-US",
    userAgent:
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
  });
  await ctx.addInitScript(() => {
    Object.defineProperty(navigator, "webdriver", { get: () => undefined });
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
      if (body?.attachments?.markets && Object.keys(body.attachments.markets).length) {
        attachmentsList.push(body.attachments);
      }
      if (body?.layout?.cards) attachmentsList.push(body.attachments || {});
    } catch {
      /* ignore */
    }
  });

  try {
    const eventHint = String(payload.event_name || "")
      .replace(/\b20\d{2}\b/g, "")
      .trim();
    await page.goto("https://sportsbook.fanduel.com/navigation/pga", {
      waitUntil: "domcontentloaded",
      timeout: 120000,
    });
    await page.waitForTimeout(5000);
    if (eventHint) {
      const link = page.getByText(eventHint, { exact: false }).first();
      if (await link.count()) {
        await link.click({ timeout: 8000 }).catch(() => {});
        await page.waitForTimeout(4000);
      }
    }
    for (const label of ["Round Scores", "Birdies or Better", "Player Round Scores", "Player Props", "All Odds"]) {
      const el = page.getByText(label, { exact: false }).first();
      if (await el.count()) {
        await el.click({ timeout: 5000 }).catch(() => {});
        await page.waitForTimeout(4000);
      }
    }
  } finally {
    await browser.close().catch(() => {});
  }

  const rows = [];
  for (const att of attachmentsList) {
    rows.push(...propsFromFanduelAttachments(att, payload, wantRound));
  }
  return dedupePropsOnePerPlayerMarket(rows);
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

  let props = [];
  let meta = {};
  try {
    const hit = await propsFromApiPath(payload, wantRound);
    props = hit.props || [];
    meta = hit.meta || {};
  } catch (e) {
    meta = { error: e?.message || String(e) };
  }

  const forcePw = String(process.env.FD_USE_PLAYWRIGHT || "").trim() === "1";
  if (!props.length && forcePw) {
    try {
      const pwProps = await captureFanduelViaPlaywright(payload, wantRound);
      if (pwProps.length) props = pwProps;
      meta.playwright = pwProps.length;
    } catch (e) {
      meta.playwrightError = e?.message || String(e);
    }
  }

  props = preferPropsForTargetRound(dedupePropsOnePerPlayerMarket(props), wantRound);

  let error;
  if (!props.length) {
    if (meta.couponsEmpty > 0 && meta.couponsWithDisplay === 0) {
      error =
        `FanDuel Round Scores/Birdies cards found (${meta.targets} coupons) but no live markets posted ` +
        `(empty coupon displays / 0 priced markets). Typical pre-tee or geo gap — will fill when FanDuel opens lines.`;
    } else {
      error =
        meta.error ||
        "no FanDuel round score/birdies markets parsed (may be unposted)";
    }
  }

  if (props.length) {
    console.log(
      `[fanduel-ou] ${props.length} props (targets=${meta.targets ?? "?"} display=${meta.couponsWithDisplay ?? "?"} priced=${meta.priced ?? "?"})`,
    );
  }

  return { props, error, meta };
}

async function main() {
  const projPath = join(__dirname, "..", "projections.json");
  const payload = existsSync(projPath) ? JSON.parse(readFileSync(projPath, "utf8")) : {};
  const hit = await fetchFanduelOuProps({ payload });
  console.log(`[fanduel-ou] ${hit.props.length} props${hit.error ? ` (${hit.error})` : ""}`);
  if (hit.meta) console.log("[fanduel-ou] meta", hit.meta);
  for (const p of hit.props.slice(0, 8)) {
    console.log(`  ${p.market} ${p.player_name} R${p.round_num} ${p.line} O${p.over_odds}/U${p.under_odds}`);
  }
}

if (process.argv[1] && resolve(process.argv[1]) === resolve(fileURLToPath(import.meta.url))) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
