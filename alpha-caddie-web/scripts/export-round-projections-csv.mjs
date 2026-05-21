#!/usr/bin/env node
/**
 * Snapshot CSV for the Round projections tab: one row per DraftKings O/U side (Over/Under).
 *
 *   npm run export:round-projections-csv
 *
 * Reads projections.json (or GOLF_PROJECTIONS_JSON). Output (overwrite):
 *   alpha-caddie-web/data/round_projections.csv
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";
import { formatCourseLabelForDisplay } from "./course-name-key.mjs";
import { matchPlayerByGolferLabel } from "./golfer-name-match.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const DEFAULT_OUT = join(WEB_ROOT, "data", "round_projections.csv");

const HEADER =
  "exported_at,projections_updated_at,dk_round_props_refreshed_at,event_name,course_used,display_round,round_num," +
  "dg_id,player_name,country,market,side,line,american_odds,over_odds,under_odds,book," +
  "model_projection,p_model,p_implied,edge_pct,mu_sg\n";

const MARKET_FIELD = {
  "Total Score": "total_score",
  Birdies: "birdies",
  Pars: "pars",
  Bogeys: "bogeys",
  GIR: "gir",
  "Fairways hit": "fairways",
  Putts: "putts",
};

const FAIRWAY_HOLES = 14;

function num(v, d = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : d;
}

function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

function csvCell(v) {
  const s = v == null ? "" : String(v);
  if (/[",\n\r]/.test(s)) return `"${s.replace(/"/g, '""')}"`;
  return s;
}

function fmt(v, digits = 4) {
  if (!Number.isFinite(v)) return "";
  if (digits === 0) return String(Math.round(v));
  return String(Math.round(v * 10 ** digits) / 10 ** digits);
}

function erf(x) {
  const sign = x < 0 ? -1 : 1;
  const a1 = 0.254829592;
  const a2 = -0.284496736;
  const a3 = 1.421413741;
  const a4 = -1.453152027;
  const a5 = 1.061405429;
  const p = 0.3275911;
  const t = 1 / (1 + p * Math.abs(x));
  const y = 1 - ((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t * Math.exp(-x * x);
  return sign * y;
}

function normalCdf(z) {
  return 0.5 * (1 + erf(z / Math.SQRT2));
}

function impliedProbFromAmerican(am) {
  const v = Math.round(num(am, NaN));
  if (!Number.isFinite(v) || v === 0) return NaN;
  if (v > 0) return 100 / (v + 100);
  return -v / (-v + 100);
}

function sigmaForMarket(market, mu, roundSd) {
  const m = String(market || "").trim();
  const muAbs = Math.abs(num(mu, NaN));
  if (m === "Total Score") {
    const s = num(roundSd, NaN);
    return Number.isFinite(s) && s > 0.05 ? s : 2.75;
  }
  if (!Number.isFinite(muAbs) || muAbs <= 0) return 2.4;
  if (m === "GIR") {
    const n = 18;
    const p = clamp(muAbs / n, 0.07, 0.93);
    return Math.max(1.05, Math.sqrt(n * p * (1 - p)));
  }
  if (m === "Fairways hit") {
    const n = FAIRWAY_HOLES;
    const p = clamp(muAbs / n, 1e-6, 1 - 1e-6);
    return Math.sqrt(n * p * (1 - p));
  }
  if (m === "Putts") return clamp(Math.sqrt(muAbs * 1.15), 2.35, 5.85);
  if (m === "Birdies" || m === "Bogeys") return clamp(Math.sqrt(muAbs * 1.08), 1.05, 3.15);
  if (m === "Pars") return clamp(Math.sqrt(muAbs * 1.06), 1.15, 3.35);
  return Math.max(0.55, Math.sqrt(Math.max(muAbs, 0.2)) * 0.9);
}

function modelProbOver(market, mu, line, roundSd) {
  if (!Number.isFinite(mu) || !Number.isFinite(line)) return NaN;
  const sig = sigmaForMarket(market, mu, roundSd);
  if (!Number.isFinite(sig) || sig < 0.06) return NaN;
  const z = (line - mu) / sig;
  return 1 - normalCdf(z);
}

function displayRoundFromPayload(payload) {
  const r = Math.round(num(payload?.display_round, NaN));
  if (Number.isFinite(r) && r >= 1 && r <= 4) return r;
  return 1;
}

function playerRowForRound(players, dgId, name, rnd) {
  const id = Math.round(num(dgId, NaN));
  const r = Math.round(num(rnd, NaN));
  if (Number.isFinite(id) && id > 0) {
    for (const p of players || []) {
      if (Math.round(num(p?.dg_id, NaN)) === id && Math.round(num(p?.round, NaN)) === r) return p;
    }
  }
  const matched = matchPlayerByGolferLabel(players, name);
  if (matched && Math.round(num(matched?.round, NaN)) === r) return matched;
  return null;
}

function modelProjectionForMarket(pl, market) {
  const field = MARKET_FIELD[market];
  if (!field || !pl) return NaN;
  return num(pl[field], NaN);
}

function dkPropsForRound(props, wantRound) {
  return (Array.isArray(props) ? props : []).filter((r) => {
    if (String(r?.source || "").trim().toLowerCase() !== "draftkings") return false;
    const pr = Math.round(num(r?.round_num, NaN));
    if (Number.isFinite(pr) && pr >= 1 && pr <= 4 && pr !== wantRound) return false;
    const L = num(r?.line, NaN);
    const lo = Math.round(L - 0.5) + 0.5;
    const line = Number.isFinite(L) ? (L === Math.floor(L) ? lo : L) : NaN;
    const o = num(r?.over_odds, NaN);
    const u = num(r?.under_odds, NaN);
    return Number.isFinite(line) && Number.isFinite(o) && Number.isFinite(u);
  });
}

/**
 * @param {object} payload
 * @param {{ outPath?: string, displayRound?: number }} [opts]
 */
export function writeRoundProjectionsCsv(payload, opts = {}) {
  const outPath = opts.outPath || DEFAULT_OUT;
  const displayRound =
    Number.isFinite(opts.displayRound) && opts.displayRound >= 1 && opts.displayRound <= 4
      ? Math.round(opts.displayRound)
      : displayRoundFromPayload(payload);

  const exportedAt = new Date().toISOString().replace(/\.\d{3}Z$/, "Z");
  const projAt = String(payload?.updated_at || "").trim();
  const dkAt = String(payload?.dk_round_props_refreshed_at || "").trim();
  const event = String(payload?.event_name || "").trim();
  const course = formatCourseLabelForDisplay(String(payload?.course_used || "").trim());
  const players = Array.isArray(payload?.players) ? payload.players : [];
  const dkRows = dkPropsForRound(payload?.props, displayRound);

  const lines = [HEADER];
  for (const pr of dkRows) {
    const market = String(pr?.market || "").trim();
    const lineRaw = num(pr?.line, NaN);
    const line = Number.isFinite(lineRaw)
      ? lineRaw === Math.floor(lineRaw)
        ? Math.round(lineRaw - 0.5) + 0.5
        : lineRaw
      : NaN;
    const overAm = num(pr?.over_odds, NaN);
    const underAm = num(pr?.under_odds, NaN);
    const dgId = Math.round(num(pr?.dg_id, NaN));
    const name = String(pr?.player_name || "").trim();
    const pl = playerRowForRound(players, dgId, name, displayRound);
    const mu = modelProjectionForMarket(pl, market);
    const roundSd = pl ? num(pl.round_sd, NaN) : NaN;
    const pOver = modelProbOver(market, mu, line, roundSd);
    const pUnder = Number.isFinite(pOver) ? 1 - pOver : NaN;
    const pImpOver = impliedProbFromAmerican(overAm);
    const pImpUnder = impliedProbFromAmerican(underAm);
    const edgeOver = Number.isFinite(pOver) && Number.isFinite(pImpOver) ? pOver - pImpOver : NaN;
    const edgeUnder = Number.isFinite(pUnder) && Number.isFinite(pImpUnder) ? pUnder - pImpUnder : NaN;
    const roundNum = Math.round(num(pr?.round_num, displayRound));

    for (const [side, am, pModel, pImp, edge] of [
      ["over", overAm, pOver, pImpOver, edgeOver],
      ["under", underAm, pUnder, pImpUnder, edgeUnder],
    ]) {
      const row = [
        exportedAt,
        projAt,
        dkAt,
        event,
        course,
        displayRound,
        roundNum,
        Number.isFinite(dgId) && dgId > 0 ? dgId : "",
        name,
        pl ? String(pl.country || "").trim() : "",
        market,
        side,
        fmt(line, 1),
        am,
        overAm,
        underAm,
        "draftkings",
        fmt(mu, 2),
        fmt(pModel, 4),
        fmt(pImp, 4),
        fmt(edge * 100, 2),
        pl ? fmt(num(pl.mu_sg, NaN), 3) : "",
      ];
      lines.push(row.map(csvCell).join(",") + "\n");
    }
  }

  mkdirSync(dirname(outPath), { recursive: true });
  writeFileSync(outPath, lines.join(""), "utf8");
  const sides = Math.max(0, lines.length - 1);
  const golfers = new Set(dkRows.map((r) => String(r.player_name || "").trim()).filter(Boolean));
  return {
    path: outPath,
    dkProps: dkRows.length,
    golfers: golfers.size,
    rows: sides,
    displayRound,
  };
}

function main() {
  const projPath = process.env.GOLF_PROJECTIONS_JSON?.trim()
    ? resolve(process.env.GOLF_PROJECTIONS_JSON.trim())
    : join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("Missing projections file:", projPath);
    process.exit(1);
  }
  const payload = JSON.parse(readFileSync(projPath, "utf8"));
  const roundEnv = Math.round(num(process.env.GOLF_ROUND_PROJECTIONS_CSV_ROUND, NaN));
  const opts = Number.isFinite(roundEnv) && roundEnv >= 1 && roundEnv <= 4 ? { displayRound: roundEnv } : {};
  const out = writeRoundProjectionsCsv(payload, opts);
  console.log(
    `[round-projections-csv] ${out.rows} rows (${out.dkProps} DK lines, ${out.golfers} golfers, R${out.displayRound}) -> ${out.path}`,
  );
}

const isMain =
  Boolean(process.argv[1]) &&
  resolve(fileURLToPath(import.meta.url)) === resolve(process.argv[1]);
if (isMain) main();
