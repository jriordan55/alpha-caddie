/**
 * Fit the hierarchical round model, validate round-forward, and compare
 * Baycurrent Classic R1 projections to posted DraftKings lines.
 *
 *   node scripts/run-bayesian-round-lines.mjs
 */
import { readFileSync, writeFileSync, existsSync } from "fs";
import { dirname, join, resolve } from "path";
import { fileURLToPath, pathToFileURL } from "url";
import { normCourseNameKey } from "./course-name-key.mjs";
import { impliedProbFromAmerican } from "./round-projection-mu.mjs";
import { hourlySliceWeatherSnapshot } from "./open-meteo-forecast.mjs";
import {
  IX_NAMES,
  MARKETS,
  WX_NAMES,
  fitRoundForward,
  loadCourseMoments,
  loadRounds,
  probOver,
  projectPlayer,
  weatherDesign,
  yokohamaTraits,
} from "./bayesian-round-model.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB = join(__dirname, "..");
const REPO = resolve(WEB, "..");
const HIST = join(REPO, "data", "historical_rounds_all.csv");
const WEATHER = join(WEB, "data", "historical_round_weather.json");
const PROJ = join(WEB, "projections.json");
const LINES = join(WEB, "data", "live_event_book_props.json");
const OUT = join(WEB, "data", "bayesian_round_baycurrent.json");

const BOOK = [
  { book: "Total Score", id: "score" },
  { book: "Birdies", id: "birdies" },
  { book: "Bogeys", id: "bogeys" },
  { book: "Pars", id: "pars" },
  { book: "Putts", id: "putts" },
  { book: "GIR", id: "gir" },
  { book: "Fairways", id: "fairways" },
  { book: "Fairways Hit", id: "fairways" },
];

/** R1 Thursday 8 Oct 2026, Yokohama local. One morning wave, split tees. PGA.com / Golf Digest. */
const R1_TEES = [
  ["08:45", "Beau Hossler", "Pierceson Coody", "Ren Yonezawa"],
  ["08:45", "Keith Mitchell", "Michael Kim", "Ben Kohles"],
  ["08:56", "Denny McCarthy", "Max Greyserman", "Keita Nakajima"],
  ["08:56", "Mac Meissner", "Kristoffer Ventura", "Sang-hee Lee"],
  ["09:07", "Michael Thorbjornsen", "Matt McCarty", "Stephan Jaeger"],
  ["09:07", "Michael Brennan", "Ricky Castillo", "Billy Horschel"],
  ["09:18", "Nico Echavarria", "Maverick McNealy", "Alex Smalley"],
  ["09:18", "Min Woo Lee", "Jordan Spieth", "Sungjae Im"],
  ["09:29", "Aldrich Potgieter", "Nick Taylor", "Taylor Pendrith"],
  ["09:29", "Jacob Bridgeman", "Collin Morikawa", "Xander Schauffele"],
  ["09:40", "Jackson Suber", "Takumi Kanaya", "Kosuke Sunagawa"],
  ["09:40", "Matt Wallace", "John Parry", "Jinichiro Kozuma"],
  ["09:51", "Kevin Roy", "Johnny Keefer", "Yoshinori Fujimoto"],
  ["09:51", "Christiaan Bezuidenhout", "Rasmus Neergaard-Petersen", "Hiroshi Iwata"],
  ["10:02", "Taylor Moore", "Ryo Hisatsune", "Yusaku Hosono"],
  ["10:02", "Patrick Rodgers", "Chandler Phillips", "Aguri Iwasaki"],
  ["10:13", "Steven Fisk", "Kurt Kitayama", "Max Homa"],
  ["10:13", "Sahith Theegala", "Jordan Smith", "Tomohiro Ishizaka"],
  ["10:24", "Wyndham Clark", "Justin Thomas", "Adam Scott"],
  ["10:24", "Ryan Gerard", "Kevin Yu", "Tony Finau"],
  ["10:35", "Keegan Bradley", "Hideki Matsuyama", "Rickie Fowler"],
  ["10:35", "Brian Harman", "Davis Thompson", "Tom Hoge"],
  ["10:46", "Doug Ghim", "Zach Bauchou", "Koshin Nagasaki"],
  ["10:46", "Lee Hodges", "Andrew Putnam", "David Lipsky"],
];

function r3(x) {
  return Number.isFinite(x) ? Math.round(x * 1000) / 1000 : null;
}
function normName(s) {
  return String(s || "")
    .normalize("NFD")
    .replace(/\p{M}/gu, "")
    .toLowerCase()
    .replace(/\([^)]*\)/g, " ")
    .replace(/[^a-z]+/g, " ")
    .trim();
}
function nameKey(s) {
  const parts = normName(s).split(" ").filter(Boolean);
  return parts.sort().join(" ");
}

function teeIndex(hourly, hhmm) {
  const [hh, mm] = hhmm.split(":").map((v) => Number(v));
  const startH = mm >= 30 ? hh + 1 : hh;
  const want = `2026-10-08T${String(startH).padStart(2, "0")}:00`;
  const times = hourly.time || [];
  for (let i = 0; i < times.length; i++) {
    if (String(times[i]).slice(0, 16) >= want.slice(0, 16)) return i;
  }
  return -1;
}

async function loadForecast() {
  const url =
    "https://api.open-meteo.com/v1/forecast?latitude=35.446&longitude=139.549&hourly=temperature_2m,relativehumidity_2m,precipitation,precipitation_probability,windspeed_10m,windgusts_10m,weathercode&windspeed_unit=mph&temperature_unit=fahrenheit&forecast_days=3&timezone=Asia%2FTokyo";
  const res = await fetch(url);
  if (!res.ok) throw new Error(`forecast http ${res.status}`);
  return res.json();
}

async function priorPrecipMm() {
  const url =
    "https://archive-api.open-meteo.com/v1/archive?latitude=35.446&longitude=139.549&start_date=2026-10-05&end_date=2026-10-07&hourly=precipitation&timezone=Asia%2FTokyo";
  try {
    const res = await fetch(url);
    if (!res.ok) return 0;
    const j = await res.json();
    const t = j?.hourly?.time || [];
    const p = j?.hourly?.precipitation || [];
    let s = 0;
    for (let i = 0; i < t.length; i++) {
      if (String(t[i]) >= "2026-10-06T18:00") s += Number(p[i]) || 0;
    }
    return Math.round(s * 100) / 100;
  } catch {
    return 0;
  }
}

function devig(overAm, underAm) {
  const o = impliedProbFromAmerican(overAm);
  const u = impliedProbFromAmerican(underAm);
  if (!Number.isFinite(o) || !Number.isFinite(u) || o + u <= 0) return NaN;
  return o / (o + u);
}

function displayMu(id, mu) {
  if (!Number.isFinite(mu)) return NaN;
  if (id === "fairways") return (mu / 14) * 15;
  return mu;
}
function displaySigma(id, sigma) {
  if (!Number.isFinite(sigma)) return NaN;
  if (id === "fairways") return (sigma / 14) * 15;
  return sigma;
}

function applyModelToProjections(proj, rows, fit, par) {
  const byDg = new Map(rows.map((r) => [r.dg_id, r]));
  const birdR = fit.dispersion.birdies?.r;
  const bogR = fit.dispersion.bogeys?.r;
  let n = 0;
  for (const p of proj.players || []) {
    if (Math.round(Number(p.round)) !== 1) continue;
    const row = byDg.get(Math.round(Number(p.dg_id)));
    if (!row || !Number.isFinite(row.score)) continue;
    p.total_score = row.score;
    p.score_to_par = Math.round((row.score - par) * 100) / 100;
    const sg = Math.round((par - row.score) * 1000) / 1000;
    p.mu_sg = sg;
    p.implied_mu_sg = sg;
    p.birdies = row.birdies;
    p.bogeys = row.bogeys;
    p.pars = row.pars;
    p.gir = row.gir;
    p.fairways = row.fairways;
    if (Number.isFinite(row.score_sigma)) p.round_sd = row.score_sigma;
    p.projection_recipe = "hierarchical_mu";
    p.score_source = "bayesian_hierarchical_round";
    p.hierarchical_weather_stp = row.score_weather;
    p.hierarchical_interaction_stp = row.score_ix;
    p.negbin_birdies_r = birdR;
    p.negbin_bogeys_r = bogR;
    p.weather_counts_baked = true;
    p.weather_difficulty_delta = row.score_weather;
    p._pre_weather_counts = {
      total_score: p.total_score,
      score_to_par: p.score_to_par,
      birdies: p.birdies,
      pars: p.pars,
      bogeys: p.bogeys,
      gir: p.gir,
      fairways: p.fairways,
      putts: p.putts,
      mu_sg: p.mu_sg,
      implied_mu_sg: p.implied_mu_sg,
      round_sd: p.round_sd,
    };
    n++;
  }
  proj.updated_at = new Date().toISOString();
  proj.projection_recipe = "hierarchical_mu";
  proj.projection_recipe_note =
    "Bayesian hierarchical round model: shrunk baseline + course + skill×course traits + tee-window weather + small form update. Birdies/Bogeys/Pars are NegBin. No player-course random effect. Putts omitted (historical putts are NA).";
  proj.hierarchical_mu = {
    model: "bayesian_hierarchical_round",
    applied_at: proj.updated_at,
    n_players: n,
    owns_weather: true,
    negbin: { birdies_r: birdR, bogeys_r: bogR },
  };
  proj.projection_counts_weather_baked = n > 0;
  proj.projection_counts_weather_baked_round = 1;
  proj.projection_counts_weather_baked_at = proj.updated_at;
  if (!proj.meta || typeof proj.meta !== "object") proj.meta = {};
  proj.meta.projection_recipe = "hierarchical_mu";
  proj.meta.hierarchical_mu = proj.hierarchical_mu;
  proj.meta.projection_counts_weather_baked = n > 0;
  proj.meta.projection_counts_weather_baked_round = 1;
  proj.meta.projection_counts_weather_baked_at = proj.updated_at;
  delete proj.both_side_bias_applied;
  return n;
}

async function main() {
  console.log("[bayes-round] loading history…");
  const wxFile = existsSync(WEATHER) ? JSON.parse(readFileSync(WEATHER, "utf8")) : { byKey: {} };
  const rounds = await loadRounds(HIST, wxFile.byKey || {});
  const { byKey, moments } = loadCourseMoments(WEB);
  const traits = yokohamaTraits(moments);
  const courseKey = normCourseNameKey("Yokohama Country Club West Course");
  byKey.set(courseKey, traits);
  const yokohamaRounds = rounds.filter((r) => r.ck === courseKey);
  console.log(
    `[bayes-round] course key "${courseKey}" · ${yokohamaRounds.length} historical rounds`,
  );
  console.log(
    `[bayes-round] ${rounds.length} rounds · traits yardage_z=${traits.yardage_z.toFixed(2)} narrow_z=${traits.narrow_z.toFixed(2)} firm_z=${traits.firm_hold_z.toFixed(2)} putt_z=${traits.putt_demand_z.toFixed(2)}`,
  );
  console.log("[bayes-round] round-forward fit…");
  const t0 = Date.now();
  const fit = fitRoundForward(rounds, byKey, { holdoutEvents: 12 });
  console.log(`[bayes-round] fit ${(Date.now() - t0) / 1000}s · events ${fit.n_events} players ${fit.n_players}`);
  for (const m of MARKETS) {
    const v = fit.validation[m.id];
    if (!v.n) continue;
    console.log(
      `  ${m.id.padEnd(9)} n=${v.n} MAE ${v.mae.toFixed(3)} vs baseline ${v.mae_baseline.toFixed(3)} bias ${v.bias.toFixed(3)} RMSE ${v.rmse.toFixed(3)} cover68 ${(v.cover68 * 100).toFixed(1)}%`,
    );
  }
  console.log(
    `[bayes-round] residual PM−AM ${fit.wave.shrunk_strokes.toFixed(3)} stp on ${fit.wave.n_rounds} split rounds (not applied unless the forecast wave is real)`,
  );
  console.log("[bayes-round] holdout score bias by event (actual − predicted):");
  for (const ev of fit.holdout_events) {
    console.log(`  ${(ev.bias >= 0 ? "+" : "") + ev.bias.toFixed(2)}  n=${ev.n}  ${ev.name}`);
  }
  const wxBeta = fit.coef.weather.score;
  console.log(
    `[bayes-round] score weather β wind/mph=${wxBeta[0].toFixed(3)} rain=${wxBeta[1].toFixed(3)} temp/10F=${wxBeta[2].toFixed(3)} hum=${wxBeta[3].toFixed(3)}`,
  );
  console.log(
    `[bayes-round] score interactions ${IX_NAMES.map((n, i) => `${n}=${fit.coef.ix.score[i].toFixed(3)}`).join(" ")} formW=${fit.coef.formW.score.toFixed(3)}`,
  );

  const hourly = (await loadForecast()).hourly;
  const priorMm = await priorPrecipMm();
  const teeWx = new Map();
  for (const row of R1_TEES) {
    const hhmm = row[0];
    const ix = teeIndex(hourly, hhmm);
    const snap = ix >= 0 ? hourlySliceWeatherSnapshot(hourly, ix, 5) : null;
    for (const name of row.slice(1)) {
      teeWx.set(nameKey(name), { hhmm, snap, priorMm });
    }
  }

  const proj = JSON.parse(readFileSync(PROJ, "utf8"));
  const players = (proj.players || []).filter((p) => Math.round(Number(p.round)) === 1);
  const lines = JSON.parse(readFileSync(LINES, "utf8"));
  const dk = lines.live_dk || {};
  const ck = normCourseNameKey(proj.course_used || "");
  const courseEff = fit.coef.course?.score?.get(ck);
  console.log(
    `[bayes-round] joined course "${ck}" score effect ${Number.isFinite(courseEff) ? courseEff.toFixed(3) : "missing"}`,
  );
  const par = Math.round(Number(proj.course_par_18) || 71);

  const snaps = [...teeWx.values()].map((t) => t.snap).filter(Boolean);
  const gusts = snaps.map((s) => s.windMphGust || s.windMph);
  const winds = snaps.map((s) => s.windMph);
  const minW = Math.min(...winds);
  const maxW = Math.max(...winds);
  const meaningfulWave = maxW - minW >= 3.5;
  console.log(
    `[bayes-round] R1 model wind (sustained) ${minW.toFixed(1)}–${maxW.toFixed(1)} mph, gust ${Math.min(...gusts).toFixed(1)}–${Math.max(...gusts).toFixed(1)}. AM/PM wave: ${meaningfulWave ? "yes" : "no — single morning wave"}. prior precip ${priorMm} mm`,
  );

  const rows = [];
  let unmatchedTee = 0;
  for (const p of players) {
    const dg = Math.round(Number(p.dg_id));
    const tee = teeWx.get(nameKey(p.player_name));
    if (!tee) unmatchedTee++;
    const snap = tee?.snap;
    const designSnap = snap
      ? {
          windMph: snap.windMph,
          windMphGust: snap.windMphGust,
          tempF: snap.tempF,
          humidityPct: snap.humidityPct,
          condition: snap.condition,
          precipMm: 0,
          priorPrecipMm: tee.priorMm,
        }
      : null;
    const wx = weatherDesign(designSnap);
    const pred = projectPlayer(fit, {
      dg,
      traits,
      wx,
      ck,
      par,
      projSkill: {
        ott: Number(p.sg_ott),
        app: Number(p.sg_app),
        arg: Number(p.sg_arg),
        putt: Number(p.sg_putt),
      },
    });
    const markets = {};
    for (const spec of BOOK) {
      const slot = dk[`${dg}|1|${spec.book}`];
      if (!slot || !Number.isFinite(Number(slot.line))) continue;
      const part = pred[spec.id];
      const mu = displayMu(spec.id, part.mu);
      const sigma = displaySigma(spec.id, part.sigma);
      const r = fit.dispersion[spec.id]?.r;
      if (!Number.isFinite(mu)) continue;
      const pOver = probOver(spec.id, mu, sigma, Number(slot.line), r);
      const fair = devig(slot.over, slot.under);
      markets[spec.book] = {
        mu: r3(mu),
        sigma: r3(sigma),
        line: Number(slot.line),
        over_am: slot.over,
        under_am: slot.under,
        p_over: r3(pOver),
        fair_over: r3(fair),
        edge: Number.isFinite(pOver) && Number.isFinite(fair) ? r3(pOver - fair) : null,
        board_mu: r3(
          spec.id === "score"
            ? Number(p.total_score)
            : spec.id === "fairways"
              ? Number(p.fairways)
              : Number(p[spec.id]),
        ),
      };
    }
    rows.push({
      dg_id: dg,
      player_name: p.player_name,
      tee: tee?.hhmm || null,
      rounds_n: pred.score.n,
      skill_n: pred.score.skillN,
      score: r3(pred.score.mu),
      score_base: r3(pred.score.base),
      score_form: r3(pred.score.form),
      score_ix: r3(pred.score.ix),
      score_weather: r3(pred.score.weather),
      score_sigma: r3(pred.score.sigma),
      birdies: r3(pred.birdies.mu),
      bogeys: r3(pred.bogeys.mu),
      pars: r3(pred.pars.mu),
      gir: r3(pred.gir.mu),
      fairways: r3(displayMu("fairways", pred.fairways.mu)),
      putts: r3(pred.putts.mu),
      board_score: r3(Number(p.total_score)),
      board_birdies: r3(Number(p.birdies)),
      board_bogeys: r3(Number(p.bogeys)),
      wind_sustained: snap ? r3(snap.windMph) : null,
      wind_gust: snap ? r3(snap.windMphGust) : null,
      temp_f: snap ? r3(snap.tempF) : null,
      markets,
    });
  }

  const edges = [];
  for (const row of rows) {
    for (const [market, m] of Object.entries(row.markets)) {
      if (!Number.isFinite(m.edge)) continue;
      edges.push({
        player_name: row.player_name,
        tee: row.tee,
        market,
        mu: m.mu,
        line: m.line,
        p_over: m.p_over,
        fair_over: m.fair_over,
        edge: m.edge,
        side: m.edge >= 0 ? "over" : "under",
        board_mu: m.board_mu,
      });
    }
  }
  edges.sort((a, b) => Math.abs(b.edge) - Math.abs(a.edge));

  const coefOut = {
    score_weather: Object.fromEntries(WX_NAMES.map((n, i) => [n, r3(fit.coef.weather.score[i])])),
    score_interactions: Object.fromEntries(IX_NAMES.map((n, i) => [n, r3(fit.coef.ix.score[i])])),
    birdie_weather: Object.fromEntries(WX_NAMES.map((n, i) => [n, r3(fit.coef.weather.birdies[i])])),
    bogey_weather: Object.fromEntries(WX_NAMES.map((n, i) => [n, r3(fit.coef.weather.bogeys[i])])),
    form_w: Object.fromEntries(MARKETS.map((m) => [m.id, r3(fit.coef.formW[m.id])])),
    negbin_r: {
      birdies: r3(fit.dispersion.birdies?.r),
      bogeys: r3(fit.dispersion.bogeys?.r),
      pars: r3(fit.dispersion.pars?.r),
    },
    score_sigma_oos: r3(fit.dispersion.score?.sigma),
    k_player: fit.coef.kPlayer,
    course_tau_score: r3(fit.coef.courseTau.score),
  };

  const payload = {
    event_name: proj.event_name,
    course: proj.course_used,
    round: 1,
    generated_at: new Date().toISOString(),
    model:
      "Score ~ Normal(player baseline + course + skill×traits + tee-window weather + form, σ_player). Birdies/Bogeys/Pars ~ NegBin(same inputs). No player-course random effect.",
    traits,
    wave: {
      structure: "R1 is a single morning wave, tees 1 and 10, 8:45–10:46 JST. There is no AM/PM wave.",
      meaningful_weather_split: meaningfulWave,
      course_key: ck,
      wind_feature: "mean sustained mph over the tee window (archive windMph, gusts not blended)",
      sustained_mph: [r3(minW), r3(maxW)],
      gust_mph: [r3(Math.min(...gusts)), r3(Math.max(...gusts))],
      prior_precip_mm: priorMm,
      historical_residual_pm_minus_am: r3(fit.wave.shrunk_strokes),
      historical_split_rounds: fit.wave.n_rounds,
    },
    validation: fit.validation,
    coefficients: coefOut,
    unmatched_tee: unmatchedTee,
    players: rows,
    largest_edges: edges.slice(0, 40),
  };
  writeFileSync(OUT, JSON.stringify(payload, null, 2));
  console.log(`[bayes-round] wrote ${OUT} · players ${rows.length} unmatched tee ${unmatchedTee} priced sides ${edges.length}`);
  applyModelToProjections(proj, rows, fit, par);
  writeFileSync(PROJ, `${JSON.stringify(proj, null, 2)}\n`);
  console.log(`[bayes-round] wrote round-1 μ onto ${PROJ}`);
  console.log("--- largest |edge| vs de-vigged DK ---");
  for (const e of edges.slice(0, 18)) {
    console.log(
      `${e.player_name.padEnd(28)} ${e.market.padEnd(12)} μ ${String(e.mu).padStart(6)} line ${String(e.line).padStart(4)} P(over) ${(e.p_over * 100).toFixed(1)}% fair ${(e.fair_over * 100).toFixed(1)}% ${e.side} ${(e.edge * 100).toFixed(1)} pts  board ${e.board_mu}`,
    );
  }
}

const isDirect = process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href;
if (isDirect) {
  main().catch((err) => {
    console.error(err);
    process.exit(1);
  });
}

export { main as runBayesianRoundLines };
