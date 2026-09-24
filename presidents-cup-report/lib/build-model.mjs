import { join } from "path";
import { writeFileSync, mkdirSync, readFileSync, existsSync } from "fs";
import { loadPlayers, loadProjections, WEB_ROOT } from "./data.mjs";
import { simulatePresidentsCup } from "./cup-sim.mjs";
import { buildModelMarkets, attachBookLines, matchDkOnlyProps } from "./markets.mjs";
import { scrapeDraftKingsPresidentsCupProps, loadPaperBookFallback } from "./dk-scrape.mjs";
import { organizeDkProps } from "./dk-markets.mjs";
import { cachePresidentsCupAssets } from "./cache-assets.mjs";
import { buildDashboardPayload } from "./dashboard-data.mjs";

export async function buildPresidentsCupModel(opts = {}) {
  const outDir = opts.outDir;
  mkdirSync(outDir, { recursive: true });

  const projPath = process.env.PC_PROJECTIONS?.trim() || join(WEB_ROOT, "projections-pga.json");
  const proj = loadProjections(projPath);
  const field = loadPlayers(proj);
  const nSims = Math.max(5000, Number(process.env.PC_SIMS) || opts.nSims || 20000);

  console.log(`[presidents-cup] Simulating ${nSims.toLocaleString()} cups…`);
  const sim = simulatePresidentsCup(field, { nSims, seed: 20260924 });

  const skipDk = opts.skipDk === true || (opts.skipDk !== false && String(process.env.PC_SKIP_DK || "").trim() === "1");
  const dkCachePath = join(outDir, "dk-props-cache.json");
  let dkMeta = { props: [], url: "", note: skipDk ? "DK scrape skipped" : null };
  if (skipDk && existsSync(dkCachePath)) {
    try {
      const cached = JSON.parse(readFileSync(dkCachePath, "utf8"));
      dkMeta.props = cached.props || [];
      dkMeta.url = cached.url || "";
      dkMeta.note = "Using cached DK lines (--skip-dk).";
      console.log(`[presidents-cup] DK cache: ${dkMeta.props.length} lines`);
    } catch {
      /* fall through */
    }
  }
  if (!skipDk) {
    console.log("[presidents-cup] Scraping DraftKings…");
    try {
      dkMeta = await scrapeDraftKingsPresidentsCupProps();
      if (!dkMeta.props.length) {
        const fb = loadPaperBookFallback();
        if (fb.length) {
          dkMeta.props = fb;
          dkMeta.note = "Using paper-book fallback.";
        }
      }
      console.log(`[presidents-cup] DK: ${dkMeta.props.length} raw lines`);
    } catch (e) {
      dkMeta.note = `DK scrape failed: ${e.message}`;
      dkMeta.props = loadPaperBookFallback();
    }
  }

  dkMeta.props_raw_count = dkMeta.props?.length || 0;
  const { props: dkOrganized, meta: dkMarketMeta } = organizeDkProps(dkMeta.props || []);
  dkMeta.props = dkOrganized;
  dkMeta.market_ids = dkMarketMeta;
  if (dkOrganized.length && !skipDk) {
    writeFileSync(
      dkCachePath,
      JSON.stringify({ scraped_at: dkMeta.scraped_at, url: dkMeta.url, props: dkOrganized }, null, 2),
    );
  }
  console.log(
    `[presidents-cup] DK: ${dkOrganized.length} lines · ${dkMarketMeta.market_count} markets · cup=${dkMarketMeta.cup_market_id}`,
  );

  console.log("[presidents-cup] Caching logos & headshots…");
  const manifest = await cachePresidentsCupAssets();

  const modelMarkets = buildModelMarkets(field, sim);
  let markets = attachBookLines(modelMarkets, dkMeta.props || [], dkMarketMeta);
  markets = [...markets, ...matchDkOnlyProps(dkMeta.props || [], modelMarkets, markets, dkMarketMeta)];

  const payload = buildDashboardPayload({
    field,
    sim,
    dkMeta,
    manifest,
    nSims,
  });

  writeFileSync(join(outDir, "model.json"), JSON.stringify(payload, null, 2));

  const cal = sim.tieCalibration;
  console.log(
    `[presidents-cup] Cup: USA ${payload.cup.usa_implied.toFixed(1)}% · INT ${payload.cup.int_win.toFixed(1)}% · tie ${payload.cup.tie.toFixed(1)}% (raw ${(cal?.raw_tie * 100).toFixed(1)}% → hist ${(cal?.target_tie * 100).toFixed(1)}%) · DK ${payload.dk.matched}/${payload.dk.count}`,
  );

  return payload;
}
