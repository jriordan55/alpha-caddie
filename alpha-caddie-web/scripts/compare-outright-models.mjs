#!/usr/bin/env node
/**
 * Side-by-side: DataGolf placement model vs Alpha Caddie composite-rating tournament MC.
 *   node scripts/compare-outright-models.mjs
 */
import { readFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { runTournamentMcFromProjections } from "./tournament-mc-outrights.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const proj = JSON.parse(readFileSync(join(WEB, "projections.json"), "utf8"));
const players = Array.isArray(proj.players) ? proj.players : [];
const par = Math.round(Number(proj.course_par_18 ?? proj.meta?.course_par_18) || 70);

function num(x, f = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : f;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function datagolfModelProb01(v) {
  const x = num(v, NaN);
  if (!Number.isFinite(x) || x < 0) return NaN;
  if (x === 0) return 0;
  if (x > 0 && x <= 1) return Math.min(1, Math.max(0, x));
  if (x > 1 && x < 100) return Math.min(1, x / 100);
  return NaN;
}

function americanFromDecimal(d) {
  if (!Number.isFinite(d) || d <= 1) return NaN;
  if (d >= 2) return Math.round((d - 1) * 100);
  return Math.round(-100 / (d - 1));
}

function americanFromProb(p) {
  const pp = clamp(p, 1e-6, 1 - 1e-6);
  return americanFromDecimal(1 / pp);
}

function fmtAm(p) {
  if (!Number.isFinite(p) || p <= 0) return "—";
  const a = americanFromProb(p);
  return Number.isFinite(a) ? (a > 0 ? `+${a}` : String(a)) : "—";
}

function fmtPct(p) {
  return Number.isFinite(p) ? `${(p * 100).toFixed(2)}%` : "—";
}

function placementMerged(id) {
  const rows = players.filter((p) => Math.round(num(p.dg_id)) === id);
  const base = players.find((p) => Math.round(num(p.dg_id)) === id && Math.round(num(p.round)) === 1) || rows[0];
  if (!base) return null;
  const out = { ...base };
  for (const col of ["win", "top_5", "top_10", "top_20", "make_cut"]) {
    if (Number.isFinite(datagolfModelProb01(out[col]))) continue;
    for (const p of rows) {
      const pp = datagolfModelProb01(p[col]);
      if (Number.isFinite(pp) && pp > 0) {
        out[col] = p[col];
        break;
      }
    }
  }
  return out;
}

function oldModelProb(dgId, marketKey) {
  const prow = placementMerged(dgId);
  const col =
    marketKey === "win"
      ? "win"
      : marketKey === "top_5"
        ? "top_5"
        : marketKey === "top_10"
          ? "top_10"
          : marketKey === "top_20"
            ? "top_20"
            : marketKey === "make_cut"
              ? "make_cut"
              : marketKey === "mc"
                ? "make_cut"
                : "win";
  const p = datagolfModelProb01(prow?.[col]);
  if (!Number.isFinite(p) || p <= 0) return NaN;
  if (marketKey === "mc") return clamp(1 - p, 1e-6, 1 - 1e-6);
  return clamp(p, 1e-6, 1 - 1e-6);
}

function alphaProb(maps, id, marketKey) {
  if (marketKey === "mc") {
    const p = maps.make_cut.get(id);
    return Number.isFinite(p) ? 1 - p : NaN;
  }
  const m = maps[marketKey];
  return m ? maps[marketKey].get(id) : NaN;
}

function displayName(raw) {
  const s = String(raw || "").trim();
  const m = s.match(/^([^,]+),\s*(.+)$/);
  return m ? `${m[2].trim()} ${m[1].trim()}` : s;
}

const markets = ["win", "top_5", "top_10", "top_20", "make_cut"];
const winPack = proj.outrights?.win?.rows || [];
const alpha = runTournamentMcFromProjections(proj, { nSims: 500, seed: 42 });

const rows = [];
for (const or of winPack) {
  const id = Math.round(num(or.dg_id));
  const name = displayName(or.player_name);
  for (const mk of markets) {
    const pOld = oldModelProb(id, mk);
    const pNew = alphaProb(alpha.maps, id, mk);
    if (!Number.isFinite(pOld) && !Number.isFinite(pNew)) continue;
    rows.push({
      id,
      name,
      market: mk,
      pOld,
      pNew,
      amOld: pOld,
      amNew: pNew,
      deltaPp: Number.isFinite(pOld) && Number.isFinite(pNew) ? (pNew - pOld) * 100 : NaN,
    });
  }
}

rows.sort((a, b) => {
  if (a.market !== b.market) return markets.indexOf(a.market) - markets.indexOf(b.market);
  return (b.pNew || 0) - (a.pNew || 0);
});

const event = proj.event_name || proj.meta?.event_name || "Event";
console.log(`\n${event} @ ${proj.course_used || ""} (par ${par})`);
console.log("DG reference: preds placement (win / top_* / make_cut on export)");
console.log(
  `Alpha model: composite μ_SG + round_sd, shifted log-normal MC (${alpha.nSims} sims, sdlog≈0.45)\n`,
);

for (const mk of markets) {
  const slice = rows.filter((r) => r.market === mk).slice(0, 12);
  if (!slice.length) continue;
  const label =
    mk === "win"
      ? "Outright Win"
      : mk === "top_5"
        ? "Top 5"
        : mk === "top_10"
          ? "Top 10"
          : mk === "top_20"
            ? "Top 20"
            : "Make Cut";
  console.log(`── ${label} (sorted by Alpha model) ──`);
  console.log(
    "Golfer".padEnd(22) +
      "DG %".padStart(8) +
      "DG".padStart(8) +
      "Alpha %".padStart(8) +
      "Alpha".padStart(8) +
      "Δ pp".padStart(8),
  );
  console.log("-".repeat(62));
  for (const r of slice) {
    console.log(
      r.name.slice(0, 21).padEnd(22) +
        fmtPct(r.pOld).padStart(8) +
        fmtAm(r.amOld).padStart(8) +
        fmtPct(r.pNew).padStart(8) +
        fmtAm(r.amNew).padStart(8) +
        (Number.isFinite(r.deltaPp) ? `${r.deltaPp >= 0 ? "+" : ""}${r.deltaPp.toFixed(2)}` : "—").padStart(8),
    );
  }
  console.log("");
}

const winRows = rows.filter((r) => r.market === "win" && Number.isFinite(r.pNew));
winRows.sort((a, b) => (b.pNew || 0) - (a.pNew || 0));
console.log("── Alpha model win favorites ──");
for (const r of winRows.slice(0, 8)) {
  console.log(`  ${r.name.padEnd(22)} ${fmtPct(r.pNew).padStart(7)}  (DG ${fmtPct(r.pOld)})`);
}

const oldSum = winRows.reduce((s, r) => s + (r.pOld || 0), 0);
const newSum = winRows.reduce((s, r) => s + (r.pNew || 0), 0);
console.log("\n── Field summary (Win market) ──");
console.log(`Players compared: ${winRows.length}`);
console.log(`Sum of win probs — DG: ${(oldSum * 100).toFixed(1)}%  Alpha: ${(newSum * 100).toFixed(1)}%\n`);
