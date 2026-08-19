import { americanToDecimal } from "./ev-math.mjs";

/** @param {number} stake @param {number} americanOdds @param {string} result */
export function calcManualBetPnl(stake, americanOdds, result) {
  const r = String(result || "").toUpperCase();
  if (r === "OPEN" || !r) return NaN;
  const s = Number(stake);
  if (!Number.isFinite(s) || s <= 0) return 0;
  const dec = americanToDecimal(americanOdds);
  if (!Number.isFinite(dec) || dec <= 1) return 0;
  if (r === "W") return s * (dec - 1);
  if (r === "L") return -s;
  return 0;
}

/** @param {object} a @param {object} b */
export function compareManualBets(a, b) {
  const da = String(a.placedAt || a.eventDate || "");
  const db = String(b.placedAt || b.eventDate || "");
  const t = da.localeCompare(db);
  if (t) return t;
  const ev = String(a.eventName || "").localeCompare(String(b.eventName || ""));
  if (ev) return ev;
  const rd = Number(a.round) - Number(b.round);
  if (rd) return rd;
  return String(a.id || "").localeCompare(String(b.id || ""));
}

/**
 * Bankroll curve from user-entered stakes on settled bets (chronological).
 * Open bets are listed but do not move the curve until graded.
 * @param {object[]} bets
 * @param {number} startingBankroll
 */
export function simulateMyBetsLedger(bets, startingBankroll) {
  const B0 = Math.max(0, Number(startingBankroll) || 0);
  const sorted = [...bets].sort(compareManualBets);

  let bankroll = B0;
  let peak = B0;
  let maxDd = 0;
  let totalStaked = 0;
  let totalPnl = 0;
  let wins = 0;
  let losses = 0;
  let pushes = 0;
  let openCount = 0;
  let openStake = 0;

  /** @type {object[]} */
  const ledger = [];
  /** @type {{ i: number, bankroll: number }[]} */
  const series = [{ i: 0, bankroll: B0 }];
  let settledIdx = 0;

  for (const bet of sorted) {
    const res = String(bet.result || "open").toUpperCase();
    const stake = Number(bet.stake) || 0;

    if (stake <= 0) {
      ledger.push({
        ...bet,
        stake: 0,
        pnl: NaN,
        bankrollAfter: bankroll,
        status: "draft",
        betRes: res,
      });
      continue;
    }

    if (res === "OPEN") {
      openCount++;
      openStake += stake;
      ledger.push({
        ...bet,
        stake,
        pnl: NaN,
        bankrollAfter: bankroll,
        status: "open",
        betRes: "OPEN",
      });
      continue;
    }

    const pnl = calcManualBetPnl(stake, bet.odds, res);
    totalStaked += stake;
    totalPnl += pnl;
    bankroll += pnl;
    if (res === "W") wins++;
    else if (res === "L") losses++;
    else pushes++;

    peak = Math.max(peak, bankroll);
    maxDd = Math.max(maxDd, peak - bankroll);
    settledIdx++;

    ledger.push({
      ...bet,
      stake,
      pnl,
      bankrollAfter: bankroll,
      status: "settled",
      betRes: res,
    });
    series.push({ i: settledIdx, bankroll });
  }

  const ending = bankroll;
  const pl = ending - B0;
  const roi = B0 > 0 ? (pl / B0) * 100 : NaN;
  const roiOnStaked = totalStaked > 0 ? (totalPnl / totalStaked) * 100 : NaN;
  const settledN = wins + losses + pushes;
  const avgStake = settledN > 0 ? totalStaked / settledN : NaN;
  const maxDdPct = peak > 0 ? (maxDd / peak) * 100 : NaN;
  const hitPct = wins + losses > 0 ? (wins / (wins + losses)) * 100 : NaN;

  return {
    B0,
    ending,
    pl,
    roi,
    roiOnStaked,
    peak,
    maxDd,
    maxDdPct,
    ledger,
    series,
    bets: settledN,
    avgStake,
    totalStaked,
    wins,
    losses,
    pushes,
    openCount,
    openStake,
    hitPct,
    totalBets: sorted.length,
  };
}

/** @param {object[]} ledger */
export function myBetsSummaryByMarket(ledger) {
  /** @type {Map<string, { stakes: number[], pnls: number[], wins: number, losses: number }>} */
  const m = new Map();
  for (const row of ledger) {
    if (row.status !== "settled" || (Number(row.stake) || 0) <= 0) continue;
    const market = String(row.market || "Other");
    let acc = m.get(market);
    if (!acc) acc = { stakes: [], pnls: [], wins: 0, losses: 0 };
    acc.stakes.push(row.stake);
    acc.pnls.push(row.pnl);
    const r = String(row.betRes || "").toUpperCase();
    if (r === "W") acc.wins++;
    else if (r === "L") acc.losses++;
    m.set(market, acc);
  }
  return [...m.entries()].map(([market, acc]) => {
    const totalStaked = acc.stakes.reduce((s, x) => s + x, 0);
    const totalPnl = acc.pnls.reduce((s, x) => s + x, 0);
    const n = acc.stakes.length;
    const roi = totalStaked > 0 ? (totalPnl / totalStaked) * 100 : NaN;
    const hitPct = acc.wins + acc.losses > 0 ? (acc.wins / (acc.wins + acc.losses)) * 100 : NaN;
    return {
      market,
      bets: n,
      hitPct,
      roi,
      avgStake: n ? totalStaked / n : NaN,
      totalPnl,
      totalStaked,
    };
  });
}
