/**
 * Parlay Pro — correlated DK round O/U parlay EV vs model joint probability.
 */
(function () {
  "use strict";

  /** @type {Record<string, unknown> | null} */
  let corrData = null;
  /** @type {object | null} */
  let api = null;

  const BOOK_HOLD = 0.048;

  const DEFAULT_RHO = {
    same_player_same_market: 0.55,
    same_player_cross_market: 0.28,
    same_wave_same_market: 0.18,
    same_wave_cross_market: 0.1,
    same_round_same_market: 0.125,
    different_wave: 0.04,
    different_player_diff_round: 0.02,
  };

  function normalInv(p) {
    const clamped = Math.max(1e-6, Math.min(1 - 1e-6, p));
    const a = [
      -3.969683028665376e1, 2.209460984245205e2, -2.759285104469687e2, 1.383577518672690e2,
      -3.066479806614716e1, 2.506628277459239e0,
    ];
    const b = [
      -5.447609879822406e1, 1.615858368580409e2, -1.556989798598866e2, 6.680131188771972e1,
      -1.328681158446784e1,
    ];
    const c = [
      -7.784894002430293e-3, -3.223964580411365e-1, -2.400758277161838e0, -2.549732539343734e0,
      4.374664141464968e0, 2.938163982698783e0,
    ];
    const d = [7.784695709041462e-3, 3.222443800069044e-1, 2.445134137142996e0, 3.754408661907416e0];
    const plow = 0.02425;
    const phigh = 1 - plow;
    let q;
    if (clamped < plow) {
      q = Math.sqrt(-2 * Math.log(clamped));
      return (
        (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
        ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1)
      );
    }
    if (clamped > phigh) {
      q = Math.sqrt(-2 * Math.log(1 - clamped));
      return -(
        (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
        ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1)
      );
    }
    q = clamped - 0.5;
    const r = q * q;
    return (
      ((((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q) /
      (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1)
    );
  }

  function normalCdf(x) {
    const t = 1 / (1 + 0.2316419 * Math.abs(x));
    const d = 0.3989423 * Math.exp((-x * x) / 2);
    let p = d * t * (0.3193815 + t * (-0.3565638 + t * (1.781478 + t * (-1.821256 + t * 1.330274))));
    if (x > 0) p = 1 - p;
    return p;
  }

  /** Gaussian copula P(both hit) with margins p1, p2 — Genz / Drezner pbivnorm. */
  function bivariateCopulaProb(p1, p2, rho) {
    if (Math.abs(rho) < 1e-8) return p1 * p2;
    const h = normalInv(p1);
    const k = normalInv(p2);
    const r = Math.max(-0.99, Math.min(0.99, rho));
    const x = [0.325303, 0.4215811, 0.1333955, 0.0063742];
    const y = [0, -0.392837, -0.202691, -0.052294];
    const asr = Math.asin(r);
    let bvn = 0;
    for (let i = 0; i < 4; i++) {
      const sn = Math.sin(y[i] * asr);
      bvn += x[i] * Math.exp((sn * sn * (h * h + k * k - 2 * r * h * k)) / (2 * (1 - r * r)));
    }
    bvn = (bvn * asr) / (2 * Math.PI) + normalCdf(h) * normalCdf(k);
    return Math.max(0.0005, Math.min(0.9995, bvn));
  }


  function legPairKey(a, b) {
    const ka = `${a.market}|${a.side}`;
    const kb = `${b.market}|${b.side}`;
    return ka < kb ? `${ka}+${kb}` : `${kb}+${ka}`;
  }

  function legRelation(a, b) {
    if (a.dgId === b.dgId && a.roundNum === b.roundNum) return "same_player";
    if (a.roundNum === b.roundNum && a.teeWave && b.teeWave && a.teeWave === b.teeWave) return "same_wave";
    if (a.roundNum === b.roundNum) return "same_round";
    return "other";
  }

  function pairRho(a, b) {
    const rel = legRelation(a, b);
    const defs = corrData?.default_rho || DEFAULT_RHO;
    const pk = legPairKey(a, b);
    const bucket =
      rel === "same_player" ? corrData?.same_player : rel === "same_wave" ? corrData?.same_tee_wave : null;
    const emp = bucket?.[pk];
    if (emp && Number.isFinite(emp.rho) && emp.n >= 20) return emp.rho;
    if (emp && emp.indep > 1e-6 && emp.n >= 12) {
      const lift = emp.co_hit / emp.indep;
      return Math.max(-0.5, Math.min(0.75, (lift - 1) * 0.35));
    }
    const sameMkt = a.market === b.market;
    const sameSide = a.side === b.side;
    if (rel === "same_player") return sameMkt ? defs.same_player_same_market : defs.same_player_cross_market;
    if (rel === "same_wave") return sameMkt && sameSide ? defs.same_wave_same_market : defs.same_wave_cross_market;
    if (rel === "same_round") {
      if (sameMkt && sameSide) return defs.same_round_same_market;
      return defs.different_wave;
    }
    return defs.different_player_diff_round;
  }

  function avgPairRho(legs) {
    if (legs.length < 2) return 0;
    let sum = 0;
    let n = 0;
    for (let i = 0; i < legs.length; i++) {
      for (let j = i + 1; j < legs.length; j++) {
        sum += pairRho(legs[i], legs[j]);
        n++;
      }
    }
    return n ? sum / n : 0;
  }

  function jointProbForLegs(legs, probOf) {
    if (!legs.length) return NaN;
    if (legs.length === 1) return probOf(legs[0]);
    const ps = legs.map(probOf);
    let p = ps.reduce((s, x) => s * x, 1);
    for (let i = 0; i < legs.length; i++) {
      for (let j = i + 1; j < legs.length; j++) {
        const rho = pairRho(legs[i], legs[j]);
        const pij = bivariateCopulaProb(ps[i], ps[j], rho);
        const indep = ps[i] * ps[j];
        if (indep > 1e-9) p *= pij / indep;
      }
    }
    const lower = Math.max(0, ps.reduce((s, x) => s + x, 0) - (ps.length - 1));
    const upper = Math.min(...ps);
    return Math.max(lower, Math.min(upper, Math.max(0.0005, Math.min(0.9995, p))));
  }

  function jointWinProb(legs) {
    return jointProbForLegs(legs, (l) => l.pWin);
  }

  function combinations(arr, k, maxOut = 80000) {
    const out = [];
    const n = arr.length;
    if (k > n || k < 1) return out;
    const idx = Array.from({ length: k }, (_, i) => i);
    const push = () => {
      if (out.length >= maxOut) return false;
      out.push(idx.map((i) => arr[i]));
      return true;
    };
    if (!push()) return out;
    while (true) {
      let i = k - 1;
      while (i >= 0 && idx[i] === i + n - k) i--;
      if (i < 0) break;
      idx[i]++;
      for (let j = i + 1; j < k; j++) idx[j] = idx[j - 1] + 1;
      if (!push()) break;
    }
    return out;
  }

  function parlayComboType(legs) {
    const players = new Set(legs.map((l) => l.dgId));
    const mk = legs.map((l) => `${l.market}|${l.side}`);
    const sameMarket = mk.every((m) => m === mk[0]);
    const waves = legs.map((l) => l.teeWave || "").filter(Boolean);
    const sameWave = waves.length === legs.length && new Set(waves).size === 1;
    if (players.size === 1 && legs.length > 1) return "same_player";
    if (sameMarket && players.size > 1) return sameWave ? "same_market_wave" : "same_market";
    return "mixed";
  }

  function buildCandidateLegs() {
    if (!api) return [];
    const props = api.draftKingsRoundPropsOnly();
    const elim = api.dgIdsEliminatedFromEventPostCut?.() || new Set();
    const round = api.getOuRound();
    const legs = [];

    for (const pr of props) {
      const marketCanon = api.ouPropsCanonicalMarket(pr.market);
      const mKey = marketCanon === "Total Score" ? "Total score" : marketCanon;
      const L = api.enforceHalfLine(api.num(pr.line, NaN));
      const oAm = Math.round(api.num(pr.over_odds, NaN));
      const uAm = Math.round(api.num(pr.under_odds, NaN));
      if (!Number.isFinite(L) || !Number.isFinite(oAm) || !Number.isFinite(uAm)) continue;
      const rnd = Math.round(api.num(pr.round_num, round));
      const prow = api.projectionRowForPropPlayerSource(pr, rnd);
      if (!prow) continue;
      const dgId = Math.round(api.num(prow.dg_id, NaN));
      if (elim.size && elim.has(dgId)) continue;
      const pOver = api.clampProb01(api.modelProbOverMarket(mKey, prow, L));
      if (!Number.isFinite(pOver)) continue;
      const pUnder = api.clampProb01(1 - pOver);
      const dO = api.decimalFromAmerican(oAm);
      const dU = api.decimalFromAmerican(uAm);
      const qOver = api.propsNoVigOverProb(oAm, uAm);
      const fairDecO = Number.isFinite(qOver) && qOver > 0 ? 1 / qOver : dO;
      const fairDecU = Number.isFinite(qOver) && qOver < 1 ? 1 / (1 - qOver) : dU;
      const name = api.displayGolferName(String(prow.player_name || pr.player_name || ""));
      const teeWave = String(prow.dg_tee_wave || "").trim().toLowerCase();

      const base = {
        dgId,
        playerName: name,
        market: marketCanon,
        mKey,
        line: L,
        roundNum: rnd,
        teeWave,
        overAm: oAm,
        underAm: uAm,
      };

      legs.push({
        ...base,
        side: "over",
        oddsAm: oAm,
        decimal: dO,
        fairDecimal: fairDecO,
        pWin: pOver,
        modelFairAm: api.americanFromDecimal(1 / pOver),
        modelEv: pOver * dO - 1,
      });
      legs.push({
        ...base,
        side: "under",
        oddsAm: uAm,
        decimal: dU,
        fairDecimal: fairDecU,
        pWin: pUnder,
        modelFairAm: api.americanFromDecimal(1 / pUnder),
        modelEv: pUnder * dU - 1,
      });
    }
    return legs;
  }

  function scoreParlay(legs) {
    const dkNaiveDecimal = legs.reduce((p, l) => p * l.decimal, 1);
    const dkFairNaiveDecimal = legs.reduce((p, l) => p * l.fairDecimal, 1);
    const dkIndepProb = legs.reduce((p, l) => p * (1 / l.decimal), 1);
    const dkJointProb = jointProbForLegs(legs, (l) => 1 / l.decimal);
    const dkDecimal = 1 / dkJointProb;
    const dkFairJointProb = jointProbForLegs(legs, (l) => 1 / l.fairDecimal);
    const dkFairDecimal = 1 / dkFairJointProb;
    const indepProb = legs.reduce((p, l) => p * l.pWin, 1);
    const jointProb = jointWinProb(legs);
    const avgRho = avgPairRho(legs);
    const modelFairDecimal = 1 / jointProb;
    const vigMult = 1 / (1 + BOOK_HOLD);
    const modelVigDecimal = modelFairDecimal * vigMult;
    const indepFairDecimal = 1 / indepProb;
    const modelEv = jointProb * dkDecimal - 1;
    const indepEv = indepProb * dkDecimal - 1;
    const dkImplied = dkJointProb;
    const dkFairImplied = dkFairJointProb;
    const edgeVsDk = jointProb - dkJointProb;
    const edgeVsDkFair = jointProb - dkFairJointProb;
    const corrUplift = indepProb > 1e-9 ? jointProb / indepProb : 1;
    const dkCorrUplift = dkIndepProb > 1e-9 ? dkJointProb / dkIndepProb : 1;
    const comboType = parlayComboType(legs);
    return {
      legs,
      comboType,
      avgRho,
      dkDecimal,
      dkNaiveDecimal,
      dkFairDecimal,
      dkJointProb,
      dkIndepProb,
      dkCorrUplift,
      dkAmerican: api.americanFromDecimal(dkDecimal),
      dkNaiveAmerican: api.americanFromDecimal(dkNaiveDecimal),
      dkFairAmerican: api.americanFromDecimal(dkFairDecimal),
      modelFairDecimal,
      modelFairAmerican: api.americanFromDecimal(modelFairDecimal),
      modelVigDecimal,
      modelVigAmerican: api.americanFromDecimal(modelVigDecimal),
      indepFairDecimal,
      indepFairAmerican: api.americanFromDecimal(indepFairDecimal),
      jointProb,
      indepProb,
      modelEv,
      indepEv,
      edgeVsDk,
      edgeVsDkFair,
      corrUplift,
      evGainVsIndep: modelEv - indepEv,
      isNegCorr: avgRho < -0.04 && corrUplift < 0.98,
    };
  }

  function comboAllowed(combo, style) {
    const key = new Set(combo.map((l) => `${l.dgId}|${l.market}|${l.side}|${l.line}`));
    if (key.size < combo.length) return false;
    const type = parlayComboType(combo);
    if (style === "same_player") return type === "same_player";
    if (style === "same_market") return type === "same_market" || type === "same_market_wave";
    if (style === "neg_corr") {
      const s = scoreParlay(combo);
      return s.isNegCorr && s.modelEv > 0;
    }
    return true;
  }

  function buildPools(legs, minSingleEv) {
    const filtered = legs.filter((l) => l.modelEv >= minSingleEv);
    const byPlayer = new Map();
    const byMarketWave = new Map();
    for (const l of filtered) {
      if (!byPlayer.has(l.dgId)) byPlayer.set(l.dgId, []);
      byPlayer.get(l.dgId).push(l);
      const mk = `${l.market}|${l.side}|${l.teeWave || "any"}`;
      if (!byMarketWave.has(mk)) byMarketWave.set(mk, []);
      byMarketWave.get(mk).push(l);
    }
    for (const arr of byPlayer.values()) arr.sort((a, b) => b.modelEv - a.modelEv);
    for (const arr of byMarketWave.values()) arr.sort((a, b) => b.modelEv - a.modelEv);
    return { filtered, byPlayer, byMarketWave };
  }

  function searchPool(pool, nLegs, style, maxCombos) {
    if (pool.length < nLegs) return [];
    const combos = combinations(pool, nLegs, maxCombos);
    const scored = [];
    for (const combo of combos) {
      if (!comboAllowed(combo, style)) continue;
      scored.push(scoreParlay(combo));
    }
    return scored;
  }

  function searchBestParlays(legs, nLegs, minSingleEv, style) {
    const { filtered, byPlayer, byMarketWave } = buildPools(legs, minSingleEv);
    const maxCombos = nLegs <= 3 ? 100000 : nLegs === 4 ? 50000 : 20000;
    const seen = new Set();
    const merged = [];

    const add = (rows) => {
      for (const r of rows) {
        const k = r.legs
          .map((l) => `${l.dgId}|${l.market}|${l.side}|${l.line}`)
          .sort()
          .join(";");
        if (seen.has(k)) continue;
        seen.add(k);
        merged.push(r);
      }
    };

    if (style === "all" || style === "same_player") {
      const playerPools = [...byPlayer.values()]
        .filter((a) => a.length >= nLegs)
        .map((a) => a.slice(0, 14));
      for (const pool of playerPools) {
        add(searchPool(pool, nLegs, "same_player", Math.floor(maxCombos / Math.max(1, playerPools.length))));
      }
    }

    if (style === "all" || style === "same_market") {
      const wavePools = [...byMarketWave.values()]
        .filter((a) => a.length >= nLegs)
        .map((a) => a.slice(0, 14));
      for (const pool of wavePools) {
        add(searchPool(pool, nLegs, "same_market", Math.floor(maxCombos / Math.max(1, wavePools.length))));
      }
    }

    if (style === "all" || style === "neg_corr") {
      const top = filtered.sort((a, b) => b.modelEv - a.modelEv).slice(0, 32);
      add(searchPool(top, nLegs, "neg_corr", maxCombos));
    }

    if (style === "all") {
      const top = filtered.sort((a, b) => b.modelEv - a.modelEv).slice(0, 34);
      add(searchPool(top, nLegs, "all", maxCombos));
    } else if (style !== "same_player" && style !== "same_market" && style !== "neg_corr") {
      const top = filtered.sort((a, b) => b.modelEv - a.modelEv).slice(0, 34);
      add(searchPool(top, nLegs, style, maxCombos));
    }

    merged.sort((a, b) => b.modelEv - a.modelEv);
    return merged.slice(0, 25);
  }

  function pct(x) {
    return Number.isFinite(x) ? `${(x * 100).toFixed(1)}%` : "—";
  }

  function evPct(x) {
    return Number.isFinite(x) ? `${x >= 0 ? "+" : ""}${(x * 100).toFixed(1)}%` : "—";
  }

  function typeLabel(t) {
    if (t === "same_player") return "Same player";
    if (t === "same_market_wave") return "Same market · wave";
    if (t === "same_market") return "Same market";
    return "Mixed";
  }

  function renderResults(rows, nLegs) {
    const el = document.getElementById("parlay-pro-results");
    if (!el) return;
    if (!rows.length) {
      el.innerHTML = `<p class="note text-muted">No ${nLegs}-leg parlays at these filters. Try a lower min leg EV or a different combo style.</p>`;
      return;
    }
    const best = rows[0];
    let html = `<div class="parlay-pro-hero">
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">Best EV @ DK</span>
        <span class="parlay-pro-hero-value ${best.modelEv >= 0 ? "ev-pos" : "ev-neg"}">${evPct(best.modelEv)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">Your odds (fair)</span>
        <span class="parlay-pro-hero-value">${api.formatAmerican(best.modelFairAmerican)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">Your odds (+ vig)</span>
        <span class="parlay-pro-hero-value">${api.formatAmerican(best.modelVigAmerican)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">DK parlay</span>
        <span class="parlay-pro-hero-value">${api.formatAmerican(best.dkAmerican)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">Win chance</span>
        <span class="parlay-pro-hero-value">${pct(best.jointProb)}</span>
      </div>
    </div>`;

    html += `<table class="data-table parlay-pro-table"><thead><tr>
      <th>#</th><th>Type</th><th>Legs</th>
      <th>Your fair</th><th>You + vig</th><th>DK</th>
      <th>EV</th><th>Win edge</th><th>ρ</th>
    </tr></thead><tbody>`;

    rows.forEach((r, i) => {
      const legHtml = r.legs
        .map((l) => {
          const m = l.market.replace("Fairways hit", "FW").replace("Total Score", "Score");
          const w = l.teeWave ? ` · ${l.teeWave.slice(0, 3)}` : "";
          const dk = api.formatAmerican(l.oddsAm);
          const mf = api.formatAmerican(l.modelFairAm);
          return `<span class="parlay-leg-chip" title="DK ${dk} · model fair ${mf} · win ${pct(l.pWin)}">${l.playerName.split(",")[0]} ${m} ${l.side === "over" ? "O" : "U"}${l.line}${w} <span class="parlay-leg-odds">${dk}</span></span>`;
        })
        .join("");
      const edgeLabel = `${r.edgeVsDk >= 0 ? "+" : ""}${(r.edgeVsDk * 100).toFixed(1)}%`;
      html += `<tr>
        <td>${i + 1}</td>
        <td class="parlay-type-cell">${typeLabel(r.comboType)}${r.isNegCorr ? ' <span class="parlay-neg-tag">neg ρ</span>' : ""}</td>
        <td class="parlay-legs-cell">${legHtml}</td>
        <td>${api.formatAmerican(r.modelFairAmerican)}</td>
        <td>${api.formatAmerican(r.modelVigAmerican)}</td>
        <td>${api.formatAmerican(r.dkAmerican)}${r.legs.length > 1 && r.dkCorrUplift > 1.02 ? `<span class="parlay-dk-indep-hint" title="Uncorrelated leg product would be ${api.formatAmerican(r.dkNaiveAmerican)}">*</span>` : ""}</td>
        <td class="${r.modelEv >= 0 ? "ev-pos" : "ev-neg"}">${evPct(r.modelEv)}</td>
        <td class="${r.edgeVsDk >= 0 ? "ev-pos" : "ev-neg"}" title="Your win % minus DK posted implied %">${edgeLabel}</td>
        <td>${r.avgRho >= 0 ? "+" : ""}${r.avgRho.toFixed(2)}</td>
      </tr>`;
    });
    html += "</tbody></table>";
    el.innerHTML = html;
  }

  function runSearch() {
    const nLegs = Math.round(Number(document.getElementById("parlay-pro-legs")?.value) || 2);
    const minEv = Number(document.getElementById("parlay-pro-min-ev")?.value) / 100 || 0;
    const marketFilter = String(document.getElementById("parlay-pro-market")?.value || "");
    const style = String(document.getElementById("parlay-pro-style")?.value || "all");
    const status = document.getElementById("parlay-pro-status");
    if (status) status.textContent = "Searching…";

    const all = buildCandidateLegs();
    let legs = all;
    if (marketFilter) legs = legs.filter((l) => l.market === marketFilter);

    const n = Math.max(2, Math.min(6, nLegs));
    setTimeout(() => {
      const rows = searchBestParlays(legs, n, minEv, style);
      renderResults(rows, n);
      if (status) {
        status.textContent = rows.length
          ? `${rows.length} ${n}-leg combos · style: ${style.replace("_", " ")}`
          : "No combos found.";
      }
    }, 10);
  }

  async function loadCorrelations() {
    if (corrData) return corrData;
    try {
      const r = await fetch("data/parlay_correlations.json?v=" + Date.now());
      if (r.ok) corrData = await r.json();
    } catch {
      /* offline */
    }
    if (!corrData) corrData = { default_rho: DEFAULT_RHO, same_player: {}, same_tee_wave: {} };
    return corrData;
  }

  function wireControls() {
    document.getElementById("parlay-pro-search")?.addEventListener("click", runSearch);
    document.getElementById("parlay-pro-legs")?.addEventListener("change", runSearch);
    document.getElementById("parlay-pro-min-ev")?.addEventListener("change", runSearch);
    document.getElementById("parlay-pro-market")?.addEventListener("change", runSearch);
    document.getElementById("parlay-pro-style")?.addEventListener("change", runSearch);
  }

  window.ParlayPro = {
    init(deps) {
      api = deps;
      wireControls();
    },
    async render() {
      await loadCorrelations();
      runSearch();
    },
  };
})();
