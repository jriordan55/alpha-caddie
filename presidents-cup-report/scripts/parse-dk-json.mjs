import { readFileSync } from "fs";

const blobs = JSON.parse(readFileSync("output/dk-team-props.json", "utf8"));

for (const [i, b] of blobs.entries()) {
  const markets = b.markets || [];
  const selections = b.selections || [];
  console.log(`\n=== blob ${i}: ${markets.length} markets, ${selections.length} selections ===`);

  const selByMarket = new Map();
  for (const s of selections) {
    const mid = s.marketId || s.market_id;
    if (!selByMarket.has(mid)) selByMarket.set(mid, []);
    selByMarket.get(mid).push(s);
  }

  for (const m of markets) {
    const sels = selByMarket.get(m.id) || [];
    const labels = sels.map((s) => s.label || s.name);
    const hasUsa = labels.some((l) => /^usa$/i.test(String(l)));
    if (!hasUsa && sels.length !== 3) continue;
    console.log("\n---", m.id, m.name || m.marketType?.name);
    console.log(
      sels.map((s) => `${s.label} ${s.displayOdds?.american ?? ""}`).join(" | "),
    );
  }
}
