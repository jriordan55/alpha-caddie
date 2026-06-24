/**
 * Summary tab for round_projection_vs_actual: model vs book line error + flat-unit EV backtest.
 */
import { EXPORT_MARKETS } from "./round-projection-mu.mjs";
import { pickBetSide } from "../projection-tracker/ev-math.mjs";

/** Same step as Results tab min-EV filter (0.5% bins up to 20%). */
export const EV_THRESHOLDS_PCT = [0, 2.5, 5, 7.5, 10, 12.5, 15, 17.5, 20];

const MARKET_LABEL = Object.fromEntries(EXPORT_MARKETS.map((m) => [m.key, m.market]));

export function americanToDecimal(am) {
  const v = Number(am);
  if (!Number.isFinite(v) || v === 0) return NaN;
  if (v > 0) return 1 + v / 100;
  return 1 + 100 / Math.abs(v);
}

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

function fmtNum(v, digits = 3) {
  if (!Number.isFinite(v)) return "";
  return (Math.round(v * 10 ** digits) / 10 ** digits).toFixed(digits);
}

function fmtPct(v) {
  if (!Number.isFinite(v)) return "";
  return (Math.round(v * 10) / 10).toFixed(1);
}

function emptyEvAcc() {
  return { bets: 0, wins: 0, losses: 0, pushes: 0, units: 0 };
}

function addEvBet(acc, result, americanOdds) {
  acc.bets += 1;
  if (result === "W") {
    acc.wins += 1;
    const dec = americanToDecimal(americanOdds);
    acc.units += Number.isFinite(dec) ? dec - 1 : 0;
  } else if (result === "L") {
    acc.losses += 1;
    acc.units -= 1;
  } else {
    acc.pushes += 1;
  }
}

/**
 * @param {object[]} samples
 * @param {{ exported: string, projectionsUpdatedAt: string, eventName: string, course: string, displayRound: number }} meta
 */
export function buildRoundProjectionVsActualSummary(samples, meta) {
  const lineAcc = new Map();
  const evAcc = new Map();

  for (const s of samples) {
    const pm = String(s.pricingMode || "default");
    const ps = String(s.pricingSkill || "default");
    const mk = String(s.marketKey || "");
    const pk = `${pm}\x1f${ps}\x1f${mk}`;

    if (Number.isFinite(s.modelLine) && Number.isFinite(s.bookLine)) {
      const diff = s.modelLine - s.bookLine;
      let acc = lineAcc.get(pk);
      if (!acc) {
        acc = {
          pricingMode: pm,
          pricingSkill: ps,
          marketKey: mk,
          sq: 0,
          abs: 0,
          n: 0,
        };
        lineAcc.set(pk, acc);
      }
      acc.sq += diff * diff;
      acc.abs += Math.abs(diff);
      acc.n += 1;
    }

    if (s.bookOddsSource !== "pre_round_audit" || !s.hasActual) continue;

    for (const th of EV_THRESHOLDS_PCT) {
      const pick = pickBetSide(s.edgeOver, s.edgeUnder, th);
      if (!pick) continue;
      const side = pick.side;
      const result = side === "over" ? s.overResult : s.underResult;
      const odds = side === "over" ? s.overOdds : s.underOdds;
      if (result !== "W" && result !== "L" && result !== "") continue;

      const ek = `${pk}\x1f${th}\x1f${side}`;
      let acc = evAcc.get(ek);
      if (!acc) {
        acc = {
          pricingMode: pm,
          pricingSkill: ps,
          marketKey: mk,
          threshold: th,
          side,
          ...emptyEvAcc(),
        };
        evAcc.set(ek, acc);
      }
      addEvBet(acc, result, odds);
    }
  }

  const header =
    "section,exported_at,projections_updated_at,event_name,course_used,display_round,pricing_mode,pricing_skill,market,rmse,mae,n_line_pairs,ev_threshold_pct,bet_side,bets,wins,losses,pushes,units_net,roi_pct\n";

  const rows = [];
  const base = [
    meta.exported,
    meta.projectionsUpdatedAt,
    meta.eventName,
    meta.course,
    meta.displayRound,
  ];

  for (const acc of lineAcc.values()) {
    if (!acc.n) continue;
    const rmse = Math.sqrt(acc.sq / acc.n);
    const mae = acc.abs / acc.n;
    rows.push(
      [
        "model_vs_book",
        ...base,
        acc.pricingMode,
        acc.pricingSkill,
        MARKET_LABEL[acc.marketKey] || acc.marketKey,
        fmtNum(rmse, 3),
        fmtNum(mae, 3),
        acc.n,
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
      ].map(csvCell).join(","),
    );
  }

  for (const acc of evAcc.values()) {
    const roi = acc.bets > 0 ? (acc.units / acc.bets) * 100 : NaN;
    rows.push(
      [
        "ev_backtest",
        ...base,
        acc.pricingMode,
        acc.pricingSkill,
        MARKET_LABEL[acc.marketKey] || acc.marketKey,
        "",
        "",
        "",
        fmtPct(acc.threshold),
        acc.side,
        acc.bets,
        acc.wins,
        acc.losses,
        acc.pushes,
        fmtNum(acc.units, 2),
        fmtNum(roi, 1),
      ].map(csvCell).join(","),
    );
  }

  const mktLineAcc = new Map();
  const mktEvAcc = new Map();
  for (const acc of lineAcc.values()) {
    if (!acc.n) continue;
    const mk = acc.marketKey;
    let m = mktLineAcc.get(mk);
    if (!m) { m = { marketKey: mk, sq: 0, abs: 0, n: 0 }; mktLineAcc.set(mk, m); }
    m.sq += acc.sq;
    m.abs += acc.abs;
    m.n += acc.n;
  }
  for (const acc of evAcc.values()) {
    const mk = acc.marketKey;
    const ek = `${mk}\x1f${acc.threshold}\x1f${acc.side}`;
    let m = mktEvAcc.get(ek);
    if (!m) {
      m = { marketKey: mk, threshold: acc.threshold, side: acc.side, ...emptyEvAcc() };
      mktEvAcc.set(ek, m);
    }
    m.bets += acc.bets;
    m.wins += acc.wins;
    m.losses += acc.losses;
    m.pushes += acc.pushes;
    m.units += acc.units;
  }

  for (const m of mktLineAcc.values()) {
    const rmse = Math.sqrt(m.sq / m.n);
    const mae = m.abs / m.n;
    rows.push(
      [
        "model_vs_book_by_market",
        ...base,
        "(all)",
        "(all)",
        MARKET_LABEL[m.marketKey] || m.marketKey,
        fmtNum(rmse, 3),
        fmtNum(mae, 3),
        m.n,
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
      ].map(csvCell).join(","),
    );
  }

  for (const m of mktEvAcc.values()) {
    const roi = m.bets > 0 ? (m.units / m.bets) * 100 : NaN;
    rows.push(
      [
        "ev_backtest_by_market",
        ...base,
        "(all)",
        "(all)",
        MARKET_LABEL[m.marketKey] || m.marketKey,
        "",
        "",
        "",
        fmtPct(m.threshold),
        m.side,
        m.bets,
        m.wins,
        m.losses,
        m.pushes,
        fmtNum(m.units, 2),
        fmtNum(roi, 1),
      ].map(csvCell).join(","),
    );
  }

  rows.sort((a, b) => {
    const sectionRank = (r) => {
      if (r.startsWith("model_vs_book_by_market")) return 2;
      if (r.startsWith("model_vs_book")) return 0;
      if (r.startsWith("ev_backtest_by_market")) return 3;
      return 1;
    };
    const ra = sectionRank(a);
    const rb = sectionRank(b);
    if (ra !== rb) return ra - rb;
    return a.localeCompare(b);
  });

  return header + rows.map((r) => `${r}\n`).join("");
}

/** Write detail + summary sheets to round_projection_vs_actual.xlsx (optional; needs xlsx). */
export async function writeRoundProjectionVsActualWorkbook(detailCsvPath, summaryCsvText, xlsxPath) {
  let mod;
  try {
    mod = await import("xlsx");
  } catch {
    console.warn("[round-projection-vs-actual] xlsx package missing; skipping .xlsx workbook.");
    return false;
  }
  const XLSX = mod.default || mod;
  const { readFileSync, writeFileSync } = await import("fs");
  const detailCsv = readFileSync(detailCsvPath, "utf8");
  const wb = XLSX.utils.book_new();
  XLSX.utils.book_append_sheet(wb, XLSX.utils.aoa_to_sheet(parseCsvToAoa(detailCsv)), "detail");
  XLSX.utils.book_append_sheet(wb, XLSX.utils.aoa_to_sheet(parseCsvToAoa(summaryCsvText)), "summary");
  writeFileSync(xlsxPath, XLSX.write(wb, { type: "buffer", bookType: "xlsx" }));
  return true;
}

function parseCsvToAoa(text) {
  const lines = String(text || "").split(/\r?\n/).filter((l) => l.length > 0);
  return lines.map((line) => {
    const out = [];
    let cur = "";
    let q = false;
    for (let i = 0; i < line.length; i++) {
      const ch = line[i];
      if (ch === '"') {
        if (q && line[i + 1] === '"') {
          cur += '"';
          i++;
        } else q = !q;
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
  });
}
