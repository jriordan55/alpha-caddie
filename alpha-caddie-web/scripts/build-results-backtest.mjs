/**
 * Build compact historical Results cube for UI filters/line charts.
 *
 * Inputs (repo root data/):
 *   - historical_matchups_outcomes.csv
 *   - historical_outrights_outcomes.csv
 *
 * Output (web):
 *   - alpha-caddie-web/data/results_backtest.json
 */
import { createReadStream, existsSync, mkdirSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import readline from "readline";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = resolve(__dirname, "..");
const REPO_ROOT = resolve(WEB_ROOT, "..");
const DATA_DIR = resolve(REPO_ROOT, "data");

const MATCHUPS_CSV = join(DATA_DIR, "historical_matchups_outcomes.csv");
const OUTRIGHTS_CSV = join(DATA_DIR, "historical_outrights_outcomes.csv");
const OUT_JSON = join(WEB_ROOT, "data", "results_backtest.json");

const EV_BIN_STEP = 0.5;
const EV_BIN_MIN = -10;
const EV_BIN_MAX = 40;

function parseCsvLine(line) {
  const out = [];
  let cur = "";
  let q = false;
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
    if (ch === '"') {
      if (q && line[i + 1] === '"') {
        cur += '"';
        i++;
      } else {
        q = !q;
      }
      continue;
    }
    if (ch === "," && !q) {
      out.push(cur);
      cur = "";
      continue;
    }
    cur += ch;
  }
  out.push(cur);
  return out;
}

function toNum(v) {
  const n = Number(v);
  return Number.isFinite(n) ? n : NaN;
}

function americanToDecimal(am) {
  const a = toNum(am);
  if (!Number.isFinite(a) || a === 0) return NaN;
  if (a > 0) return 1 + a / 100;
  return 1 + 100 / Math.abs(a);
}

function normDate(raw, yearRaw) {
  const s = String(raw || "").trim();
  const m = s.match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (m) return `${m[1]}-${m[2]}-${m[3]}`;
  const y = Math.round(toNum(yearRaw));
  if (Number.isFinite(y) && y >= 2000 && y <= 2100) return `${y}-01-01`;
  return "2000-01-01";
}

function evBin(evPct) {
  if (!Number.isFinite(evPct)) return NaN;
  const cl = Math.max(EV_BIN_MIN, Math.min(EV_BIN_MAX, evPct));
  return Math.round(cl / EV_BIN_STEP) * EV_BIN_STEP;
}

function pushAgg(map, source, market, book, date, edgeBin, pnl) {
  if (!Number.isFinite(edgeBin) || !Number.isFinite(pnl)) return;
  const key = `${source}|${market}|${book}|${date}|${edgeBin.toFixed(1)}`;
  let row = map.get(key);
  if (!row) {
    row = { source, market, book, date, ev_bin: edgeBin, bets: 0, pnl: 0 };
    map.set(key, row);
  }
  row.bets += 1;
  row.pnl += pnl;
}

function classifyMatchupMarket(betTypeRaw) {
  const t = String(betTypeRaw || "").toLowerCase();
  if (/3[\s-]*ball/.test(t)) return "3_balls";
  if (/^r[1-4]\b/.test(t)) return "round_matchups";
  return "tournament_matchups";
}

function bucketProb(p) {
  if (!Number.isFinite(p)) return NaN;
  const cl = Math.max(0, Math.min(1, p));
  return Math.floor(cl * 50) / 50; // 0.02 buckets
}

async function readCsvRows(path, onHeader, onRow) {
  const rl = readline.createInterface({
    input: createReadStream(path, { encoding: "utf8" }),
    crlfDelay: Infinity,
  });
  let header = null;
  for await (const line of rl) {
    if (!line) continue;
    const cols = parseCsvLine(line);
    if (!header) {
      header = cols;
      onHeader(header);
      continue;
    }
    const row = {};
    for (let i = 0; i < header.length; i++) row[header[i]] = cols[i] ?? "";
    onRow(row);
  }
}

function calKey(market, bucket) {
  return `${market}|${bucket.toFixed(2)}`;
}

async function buildCalibration(matchCsv, outrCsv) {
  const wins = new Map();
  const totals = new Map();

  const addCal = (market, implied, hit) => {
    const b = bucketProb(implied);
    if (!Number.isFinite(b)) return;
    const k = calKey(market, b);
    totals.set(k, (totals.get(k) || 0) + 1);
    if (hit) wins.set(k, (wins.get(k) || 0) + 1);
  };

  await readCsvRows(
    matchCsv,
    () => {},
    (r) => {
      const market = classifyMatchupMarket(r.bet_type);
      const odds = [toNum(r.p1_close), toNum(r.p2_close), toNum(r.p3_close)];
      const outc = [toNum(r.p1_outcome), toNum(r.p2_outcome), toNum(r.p3_outcome)];
      const valid = [];
      for (let i = 0; i < 3; i++) {
        if (Number.isFinite(odds[i]) && odds[i] > 1 && (outc[i] === 0 || outc[i] === 1)) valid.push(i);
      }
      if (!valid.length) return;
      let invSum = 0;
      for (const i of valid) invSum += 1 / odds[i];
      if (!Number.isFinite(invSum) || invSum <= 0) return;
      for (const i of valid) {
        const implied = (1 / odds[i]) / invSum;
        addCal(market, implied, outc[i] === 1);
      }
    },
  );

  await readCsvRows(
    outrCsv,
    () => {},
    (r) => {
      const market = String(r.market || "").trim();
      const close = toNum(r.close_odds);
      const open = toNum(r.open_odds);
      const dec = Number.isFinite(close) ? americanToDecimal(close) : americanToDecimal(open);
      const outc = toNum(r.bet_outcome_numeric);
      if (!Number.isFinite(dec) || dec <= 1 || !(outc === 0 || outc === 1)) return;
      const implied = 1 / dec;
      addCal(market, implied, outc === 1);
    },
  );

  const cal = new Map();
  for (const [k, n] of totals.entries()) {
    const w = wins.get(k) || 0;
    if (n > 0) cal.set(k, w / n);
  }
  return cal;
}

function modelProbFromCal(cal, market, implied) {
  const b = bucketProb(implied);
  if (!Number.isFinite(b)) return implied;
  const hit = cal.get(calKey(market, b));
  return Number.isFinite(hit) ? hit : implied;
}

async function build() {
  if (!existsSync(MATCHUPS_CSV) || !existsSync(OUTRIGHTS_CSV)) {
    throw new Error("Missing historical outcomes CSV(s) in data/.");
  }
  const cal = await buildCalibration(MATCHUPS_CSV, OUTRIGHTS_CSV);

  const agg = new Map();
  const marketsBySource = { matchups: new Set(), outrights: new Set() };
  const booksBySource = { matchups: new Set(), outrights: new Set() };

  await readCsvRows(
    MATCHUPS_CSV,
    () => {},
    (r) => {
      const market = classifyMatchupMarket(r.bet_type);
      const book = String(r.book || "").trim().toLowerCase() || "unknown";
      const date = normDate(r.close_time || r.open_time, r.year);
      marketsBySource.matchups.add(market);
      booksBySource.matchups.add(book);

      const odds = [toNum(r.p1_close), toNum(r.p2_close), toNum(r.p3_close)];
      const outc = [toNum(r.p1_outcome), toNum(r.p2_outcome), toNum(r.p3_outcome)];
      const valid = [];
      for (let i = 0; i < 3; i++) {
        if (Number.isFinite(odds[i]) && odds[i] > 1 && (outc[i] === 0 || outc[i] === 1)) valid.push(i);
      }
      if (!valid.length) return;
      let invSum = 0;
      for (const i of valid) invSum += 1 / odds[i];
      if (!Number.isFinite(invSum) || invSum <= 0) return;
      for (const i of valid) {
        const dec = odds[i];
        const implied = (1 / dec) / invSum; // de-vig close line
        const modelP = modelProbFromCal(cal, market, implied);
        const evPct = 100 * (modelP * dec - 1);
        const b = evBin(evPct);
        const pnl = outc[i] === 1 ? dec - 1 : -1;
        pushAgg(agg, "matchups", market, book, date, b, pnl);
        pushAgg(agg, "matchups", "__all__", book, date, b, pnl);
        pushAgg(agg, "matchups", market, "__all__", date, b, pnl);
        pushAgg(agg, "matchups", "__all__", "__all__", date, b, pnl);
      }
    },
  );

  await readCsvRows(
    OUTRIGHTS_CSV,
    () => {},
    (r) => {
      const market = String(r.market || "").trim() || "unknown";
      const book = String(r.book || r.sportsbook || "").trim().toLowerCase() || "unknown";
      const date = normDate(r.close_time || r.open_time, r.year);
      marketsBySource.outrights.add(market);
      booksBySource.outrights.add(book);

      const close = toNum(r.close_odds);
      const open = toNum(r.open_odds);
      const dec = Number.isFinite(close) ? americanToDecimal(close) : americanToDecimal(open);
      const outc = toNum(r.bet_outcome_numeric);
      if (!Number.isFinite(dec) || dec <= 1 || !(outc === 0 || outc === 1)) return;
      const implied = 1 / dec;
      const modelP = modelProbFromCal(cal, market, implied);
      const evPct = 100 * (modelP * dec - 1);
      const b = evBin(evPct);
      const pnl = outc === 1 ? dec - 1 : -1;
      pushAgg(agg, "outrights", market, book, date, b, pnl);
      pushAgg(agg, "outrights", "__all__", book, date, b, pnl);
      pushAgg(agg, "outrights", market, "__all__", date, b, pnl);
      pushAgg(agg, "outrights", "__all__", "__all__", date, b, pnl);
    },
  );

  const rows = [...agg.values()].map((r) => ({
    ...r,
    pnl: Number(r.pnl.toFixed(6)),
  }));
  rows.sort((a, b) =>
    a.source.localeCompare(b.source) ||
    a.market.localeCompare(b.market) ||
    a.book.localeCompare(b.book) ||
    a.date.localeCompare(b.date) ||
    a.ev_bin - b.ev_bin
  );

  const out = {
    generated_at: new Date().toISOString(),
    ev_bin_step: EV_BIN_STEP,
    ev_bin_min: EV_BIN_MIN,
    ev_bin_max: EV_BIN_MAX,
    markets: {
      matchups: [...marketsBySource.matchups].sort(),
      outrights: [...marketsBySource.outrights].sort(),
    },
    books: {
      matchups: [...booksBySource.matchups].sort(),
      outrights: [...booksBySource.outrights].sort(),
    },
    rows,
  };

  mkdirSync(dirname(OUT_JSON), { recursive: true });
  writeFileSync(OUT_JSON, JSON.stringify(out));
  console.log(`Wrote ${OUT_JSON} (${rows.length} rows)`);
}

const isMain = resolve(process.argv[1] || "") === resolve(fileURLToPath(import.meta.url));
if (isMain) {
  build().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}

