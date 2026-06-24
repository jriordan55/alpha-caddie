/**
 * Parlay Pro — correlated DK round O/U parlay EV vs model joint probability.
 */
(function () {
  "use strict";

  /** @type {Record<string, unknown> | null} */
  let corrData = null;
  /** @type {object | null} */
  let api = null;

  const MARKETS = ["Total Score", "Birdies", "Pars", "Bogeys", "GIR", "Fairways hit"];

  const DEFAULT_RHO = {
    same_player_same_market: 0.55,
    same_player_cross_market: 0.28,
    same_wave_same_market: 0.18,
    same_wave_cross_market: 0.1,
    different_wave: 0.04,
    different_player_diff_round: 0.02,
  };

  function normalInv(p) {
    const clamped = Math.max(1e-6, Math.min(1 - 1e-6, p));
    // Beasley-Springer-Moro approximation
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
    let q, r;
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
    r = q * q;
    return (
      ((((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q) /
      (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1)
    );
  }

  function bivariateCopulaProb(p1, p2, rho) {
    const z1 = normalInv(p1);
    const z2 = normalInv(p2);
    const r = Math.max(-0.85, Math.min(0.85, rho));
    // P(Z1<=z1, Z2<=z2) for standard bivariate normal — Drezner-Wesolowsky
    const h = z1;
    const k = z2;
    const hk = h * k;
    const sum = Math.exp(-hk / 2);
    let acc = 0;
    const terms = [
      [0.325303, 0],
      [0.4215811, -0.392837],
      [0.1333955, -0.202691],
      [0.0063742, -0.052294],
    ];
    for (const [w, t] of terms) {
      const sh = Math.sin(t * Math.PI * 0.5);
      const sk = Math.sin(t * Math.PI * 0.5 * r);
      acc += w * Math.exp((sh * sh * h * h + sk * sk * k * k - 2 * r * sh * sk * h * k) / (2 * (1 - r * r)));
    }
    const phi2 = acc / (2 * Math.PI * Math.sqrt(1 - r * r)) + 0.25;
    return Math.max(0.001, Math.min(0.999, phi2));
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
    if (rel === "same_player") return sameMkt ? defs.same_player_same_market : defs.same_player_cross_market;
    if (rel === "same_wave") return sameMkt ? defs.same_wave_same_market : defs.same_wave_cross_market;
    if (rel === "same_round") return defs.different_wave;
    return defs.different_player_diff_round;
  }

  function jointWinProb(legs) {
    if (!legs.length) return NaN;
    if (legs.length === 1) return legs[0].pWin;
    let p = legs.reduce((s, l) => s * l.pWin, 1);
    for (let i = 0; i < legs.length; i++) {
      for (let j = i + 1; j < legs.length; j++) {
        const rho = pairRho(legs[i], legs[j]);
        const pij = bivariateCopulaProb(legs[i].pWin, legs[j].pWin, rho);
        const indep = legs[i].pWin * legs[j].pWin;
        if (indep > 1e-9) p *= pij / indep;
      }
    }
    return Math.max(0.0005, Math.min(0.9995, p));
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
      const name = api.displayGolferName(String(prow.player_name || pr.player_name || ""));
      const teeWave = String(prow.dg_tee_wave || "").trim().toLowerCase();
      const teeMin = api.parseTeetimeMinutes?.(prow.dg_teetime_local) ?? NaN;

      const base = {
        dgId,
        playerName: name,
        market: marketCanon,
        mKey,
        line: L,
        roundNum: rnd,
        teeWave,
        teeMin,
      };

      legs.push({
        ...base,
        side: "over",
        oddsAm: oAm,
        decimal: dO,
        pWin: pOver,
        modelEv: pOver * dO - 1,
        label: `${name} — ${marketCanon} O ${L}`,
      });
      legs.push({
        ...base,
        side: "under",
        oddsAm: uAm,
        decimal: dU,
        pWin: pUnder,
        modelEv: pUnder * dU - 1,
        label: `${name} — ${marketCanon} U ${L}`,
      });
    }
    return legs;
  }

  function scoreParlay(legs) {
    const dkDecimal = legs.reduce((p, l) => p * l.decimal, 1);
    const indepProb = legs.reduce((p, l) => p * l.pWin, 1);
    const jointProb = jointWinProb(legs);
    const modelEv = jointProb * dkDecimal - 1;
    const indepEv = indepProb * dkDecimal - 1;
    const dkImplied = 1 / dkDecimal;
    const edgeVsDk = jointProb - dkImplied;
    const corrUplift = indepProb > 1e-9 ? jointProb / indepProb : 1;
    return {
      legs,
      dkDecimal,
      dkAmerican: api.americanFromDecimal(dkDecimal),
      jointProb,
      indepProb,
      modelEv,
      indepEv,
      edgeVsDk,
      corrUplift,
      evGainVsIndep: modelEv - indepEv,
    };
  }

  function searchBestParlays(legs, nLegs, minSingleEv, topK = 40) {
    const pool = legs
      .filter((l) => l.modelEv >= minSingleEv)
      .sort((a, b) => b.modelEv - a.modelEv)
      .slice(0, Math.max(nLegs + 2, Math.min(topK, 36)));

    if (pool.length < nLegs) return [];

    const maxCombos = nLegs <= 3 ? 120000 : nLegs === 4 ? 60000 : 25000;
    const combos = combinations(pool, nLegs, maxCombos);
    const scored = [];
    for (const combo of combos) {
      const dgKeys = new Set(combo.map((l) => `${l.dgId}|${l.market}|${l.side}|${l.line}`));
      if (dgKeys.size < combo.length) continue;
      scored.push(scoreParlay(combo));
    }
    scored.sort((a, b) => b.modelEv - a.modelEv);
    return scored.slice(0, 25);
  }

  function pct(x) {
    return Number.isFinite(x) ? `${(x * 100).toFixed(1)}%` : "—";
  }

  function evPct(x) {
    return Number.isFinite(x) ? `${x >= 0 ? "+" : ""}${(x * 100).toFixed(1)}%` : "—";
  }

  function renderResults(rows, nLegs) {
    const el = document.getElementById("parlay-pro-results");
    if (!el) return;
    if (!rows.length) {
      el.innerHTML = `<p class="note text-muted">No ${nLegs}-leg parlays found at your filters. Lower min leg EV or add DK lines.</p>`;
      return;
    }
    const best = rows[0];
    let html = `<div class="parlay-pro-hero">
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">Best model EV</span>
        <span class="parlay-pro-hero-value ${best.modelEv >= 0 ? "ev-pos" : "ev-neg"}">${evPct(best.modelEv)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">DK parlay odds</span>
        <span class="parlay-pro-hero-value">${api.formatAmerican(best.dkAmerican)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">Joint win prob</span>
        <span class="parlay-pro-hero-value">${pct(best.jointProb)} <small class="text-muted">(indep ${pct(best.indepProb)})</small></span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">vs DK implied</span>
        <span class="parlay-pro-hero-value ${best.edgeVsDk >= 0 ? "ev-pos" : "ev-neg"}">${best.edgeVsDk >= 0 ? "+" : ""}${(best.edgeVsDk * 100).toFixed(1)} pp</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">Corr uplift</span>
        <span class="parlay-pro-hero-value">${(best.corrUplift * 100).toFixed(0)}% of indep</span>
      </div>
    </div>`;

    html += `<table class="data-table parlay-pro-table"><thead><tr>
      <th>#</th><th>Legs</th><th>DK odds</th><th>Model EV</th><th>Indep EV</th><th>Joint P</th><th>vs DK</th>
    </tr></thead><tbody>`;

    rows.forEach((r, i) => {
      const legHtml = r.legs
        .map(
          (l) =>
            `<span class="parlay-leg-chip">${l.side === "over" ? "O" : "U"} ${l.line} · ${l.playerName.split(",")[0]} · ${l.market.replace("Fairways hit", "FW")}${l.teeWave ? ` · ${l.teeWave}` : ""}</span>`,
        )
        .join("");
      html += `<tr>
        <td>${i + 1}</td>
        <td class="parlay-legs-cell">${legHtml}</td>
        <td>${api.formatAmerican(r.dkAmerican)}</td>
        <td class="${r.modelEv >= 0 ? "ev-pos" : "ev-neg"}">${evPct(r.modelEv)}</td>
        <td>${evPct(r.indepEv)}</td>
        <td>${pct(r.jointProb)}</td>
        <td class="${r.edgeVsDk >= 0 ? "ev-pos" : "ev-neg"}">${r.edgeVsDk >= 0 ? "+" : ""}${(r.edgeVsDk * 100).toFixed(1)}pp</td>
      </tr>`;
    });
    html += "</tbody></table>";
    el.innerHTML = html;
  }

  function renderLegPool(legs, minEv) {
    const el = document.getElementById("parlay-pro-leg-pool");
    if (!el) return;
    const filtered = legs.filter((l) => l.modelEv >= minEv).sort((a, b) => b.modelEv - a.modelEv);
    el.textContent = `${filtered.length} DK legs with model EV ≥ ${(minEv * 100).toFixed(0)}% (${legs.length} total sides)`;
  }

  function runSearch() {
    const nLegs = Math.round(Number(document.getElementById("parlay-pro-legs")?.value) || 2);
    const minEv = Number(document.getElementById("parlay-pro-min-ev")?.value) / 100 || 0;
    const marketFilter = String(document.getElementById("parlay-pro-market")?.value || "");
    const status = document.getElementById("parlay-pro-status");
    if (status) status.textContent = "Searching…";

    const all = buildCandidateLegs();
    let legs = all;
    if (marketFilter) legs = legs.filter((l) => l.market === marketFilter);
    renderLegPool(legs, minEv);

    const n = Math.max(2, Math.min(6, nLegs));
    setTimeout(() => {
      const rows = searchBestParlays(legs, n, minEv);
      renderResults(rows, n);
      if (status) {
        status.textContent = rows.length
          ? `Top ${rows.length} ${n}-leg combos (correlation-adjusted vs DK product odds).`
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
      /* offline / file protocol */
    }
    if (!corrData) corrData = { default_rho: DEFAULT_RHO, same_player: {}, same_tee_wave: {} };
    return corrData;
  }

  function wireControls() {
    document.getElementById("parlay-pro-search")?.addEventListener("click", runSearch);
    document.getElementById("parlay-pro-legs")?.addEventListener("change", runSearch);
    document.getElementById("parlay-pro-min-ev")?.addEventListener("change", runSearch);
    document.getElementById("parlay-pro-market")?.addEventListener("change", runSearch);
  }

  window.ParlayPro = {
    init(deps) {
      api = deps;
      wireControls();
    },
    async render() {
      await loadCorrelations();
      const meta = document.getElementById("parlay-pro-meta");
      if (meta && corrData?.generated_at) {
        meta.textContent = `Correlation data: ${corrData.rows_scored || "?"} historical player-rounds · updated ${String(corrData.generated_at).slice(0, 10)}`;
      }
      runSearch();
    },
  };
})();
