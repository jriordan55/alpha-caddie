/**
 * Parlay Pro — correlated DK round O/U parlay EV vs model joint probability.
 */
(function () {
  "use strict";

  /** @type {Record<string, unknown> | null} */
  let corrData = null;
  /** @type {object | null} */
  let api = null;

  const EXCLUDED_MARKETS = new Set(["GIR"]);

  function modelLegPrices(pOver, side) {
    const pWin = side === "over" ? pOver : api.clampProb01(1 - pOver);
    const fairDec = 1 / pWin;
    const { do: dO, du: dU } = api.viggedDecimalsForOverUnder(pOver);
    const vigDec = side === "over" ? dO : dU;
    return {
      pWin,
      fairDec,
      vigDec,
      fairAm: api.americanFromDecimal(fairDec),
      vigAm: api.americanFromDecimal(vigDec),
    };
  }

  const DEFAULT_RHO = {
    same_player_same_market: 0.55,
    same_player_cross_market: 0.28,
    /** Different players, same market + side, same tee wave — small co-movement from weather/conditions. */
    same_wave_same_market: 0.05,
    /** Different players, same market + side, same round, different waves. */
    same_round_same_market: 0.03,
    /** Cross-market: good-day legs together (U score, O birdies, O FW, U bogeys). */
    cross_market_good_good: 0.15,
    /** Cross-market: bad-day legs together (O score, O bogeys, U birdies, U FW). */
    cross_market_bad_bad: 0.15,
    /** Cross-market: good leg + bad leg (mixed round script). */
    cross_market_good_bad: -0.13,
    cross_market_neutral: 0,
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
    if (!Number.isFinite(p1) || !Number.isFinite(p2)) return NaN;
    if (Math.abs(rho) < 1e-8 || !Number.isFinite(rho)) return p1 * p2;
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


  function defaultRhoDefs() {
    return { ...DEFAULT_RHO, ...(corrData?.default_rho || {}) };
  }

  function finiteProb(x) {
    return Number.isFinite(x) ? Math.max(0.0005, Math.min(0.9995, x)) : NaN;
  }

  function clampJointProb(p, ps) {
    const q = finiteProb(p);
    if (!Number.isFinite(q)) return NaN;
    const lo = Math.max(0, ps.reduce((s, x) => s + x, 0) - (ps.length - 1));
    const hi = Math.min(...ps);
    if (!Number.isFinite(hi)) return NaN;
    if (lo > hi) return Math.min(hi, q);
    return Math.max(lo, Math.min(hi, q));
  }

  function legPairKey(a, b) {
    const ka = `${a.market}|${a.side}`;
    const kb = `${b.market}|${b.side}`;
    return ka < kb ? `${ka}+${kb}` : `${kb}+${ka}`;
  }

  /**
   * Round-quality script for one leg: good day vs bad day (for cross-market ρ sign).
   * Good: U score, U bogeys, O birdies, O fairways. Bad: the opposite sides.
   */
  function legScoringSentiment(leg) {
    const m = String(leg.market || "");
    const over = leg.side === "over";
    if (m === "Total Score" || m === "Bogeys" || m === "Putts") return over ? "bad" : "good";
    if (m === "Birdies" || m === "Fairways hit") return over ? "good" : "bad";
    return null;
  }

  function sentimentBaseRho(a, b, defs) {
    const sa = legScoringSentiment(a);
    const sb = legScoringSentiment(b);
    if (!sa || !sb) return defs.cross_market_neutral ?? 0;
    if (sa === sb) {
      return sa === "good"
        ? defs.cross_market_good_good ?? 0.15
        : defs.cross_market_bad_bad ?? 0.15;
    }
    return defs.cross_market_good_bad ?? -0.13;
  }

  function roundScopeScale(sameRound, sameWave) {
    if (!sameRound) return 0.35;
    if (sameWave) return 1;
    return 0.78;
  }

  function clampRho(r) {
    if (!Number.isFinite(r)) return NaN;
    return Math.max(-0.95, Math.min(0.95, r));
  }

  /** Historical pair ρ from a co-hit bucket; lift shrinkage when sample is thin. */
  function empiricalPairRho(bucket, pk, minN) {
    const emp = bucket?.[pk];
    if (!emp) return null;
    if (Number.isFinite(emp.rho) && emp.n >= minN) return clampRho(emp.rho);
    if (emp.n >= 12 && emp.indep > 1e-6 && Number.isFinite(emp.co_hit)) {
      const lift = emp.co_hit / emp.indep;
      return clampRho((lift - 1) * 0.35);
    }
    return null;
  }

  function sentimentBucketRho(a, b, defs) {
    const sa = legScoringSentiment(a);
    const sb = legScoringSentiment(b);
    const buckets = corrData?.sentiment_buckets;
    if (buckets) {
      if (!sa || !sb) {
        const n = buckets.neutral;
        if (n && Number.isFinite(n.rho) && n.n >= 40) return n.rho;
        return defs.cross_market_neutral ?? 0;
      }
      if (sa === sb) {
        const bkt = sa === "good" ? buckets.good_good : buckets.bad_bad;
        if (bkt && Number.isFinite(bkt.rho) && bkt.n >= 40) return bkt.rho;
        return sa === "good"
          ? defs.cross_market_good_good ?? 0.15
          : defs.cross_market_bad_bad ?? 0.15;
      }
      const opp = buckets.good_bad;
      if (opp && Number.isFinite(opp.rho) && opp.n >= 40) return opp.rho;
      return defs.cross_market_good_bad ?? -0.13;
    }
    return sentimentBaseRho(a, b, defs);
  }

  function pairRho(a, b) {
    const defs = defaultRhoDefs();
    const pk = legPairKey(a, b);
    const sameMkt = a.market === b.market;
    const sameSide = a.side === b.side;
    const sameRound = a.roundNum === b.roundNum;
    const samePlayer = a.dgId === b.dgId && sameRound;
    const sameWave =
      sameRound && a.teeWave && b.teeWave && a.teeWave === b.teeWave;
    const scope = roundScopeScale(sameRound, sameWave);

    if (samePlayer) {
      const r = empiricalPairRho(corrData?.same_player, pk, 20);
      if (r !== null) return r;
      if (sameMkt) return defs.same_player_same_market ?? 0.55;
      return defs.same_player_cross_market ?? 0.28;
    }

    if (sameWave) {
      const r = empiricalPairRho(corrData?.same_tee_wave, pk, 15);
      if (r !== null) return r;
    }

    if (sameRound) {
      const r = empiricalPairRho(corrData?.same_round, pk, 20);
      if (r !== null) return r * (sameWave ? 1 : scope);
    }

    if (sameMkt && sameSide) {
      if (sameWave) return defs.same_wave_same_market ?? 0.05;
      if (sameRound) return (defs.same_round_same_market ?? 0.03) * scope;
      return 0;
    }

    const sent = sentimentBucketRho(a, b, defs);
    return sent * scope;
  }

  function maxPairRho(legs) {
    if (legs.length < 2) return 0;
    let max = 0;
    for (let i = 0; i < legs.length; i++) {
      for (let j = i + 1; j < legs.length; j++) {
        const r = Math.abs(pairRho(legs[i], legs[j]));
        if (r > max) max = r;
      }
    }
    return max;
  }

  function legMarketProb(leg) {
    const p = 1 / leg.fairDecimal;
    return Number.isFinite(p) && p > 0 ? p : 1 / leg.decimal;
  }

  function legPostedProb(leg) {
    return 1 / leg.decimal;
  }

  /** Blend raw model win % toward DK devigged implied so parlay prices track DK's correlated stack. */
  function legCalibMarginal(leg, usePostedVig) {
    const w = api.marketBookBlendWeight?.(leg.mKey) ?? 0.65;
    const pMkt = usePostedVig ? legPostedProb(leg) : legMarketProb(leg);
    const pMod = usePostedVig ? 1 / leg.modelVigDec : leg.pWin;
    if (!Number.isFinite(pMkt) || !Number.isFinite(pMod)) return leg.pWin;
    return api.clampProb01((1 - w) * pMod + w * pMkt);
  }

  function jointCalibProb(legs, usePostedVig) {
    return jointProbForLegs(legs, (l) => legCalibMarginal(l, usePostedVig));
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
    if (legs.length === 1) return finiteProb(probOf(legs[0]));
    const ps = legs.map(probOf).map(finiteProb);
    if (ps.some((x) => !Number.isFinite(x))) return NaN;
    let p = ps.reduce((s, x) => s * x, 1);
    for (let i = 0; i < legs.length; i++) {
      for (let j = i + 1; j < legs.length; j++) {
        const rho = pairRho(legs[i], legs[j]);
        const pij = bivariateCopulaProb(ps[i], ps[j], rho);
        if (!Number.isFinite(pij)) return NaN;
        const indep = ps[i] * ps[j];
        if (indep > 1e-9) p *= pij / indep;
      }
    }
    return clampJointProb(p, ps);
  }

  function jointWinProb(legs) {
    return jointCalibProb(legs, false);
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
      if (EXCLUDED_MARKETS.has(marketCanon)) continue;
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
      const overPx = modelLegPrices(pOver, "over");
      const underPx = modelLegPrices(pOver, "under");
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
        pWin: overPx.pWin,
        modelFairDec: overPx.fairDec,
        modelVigDec: overPx.vigDec,
        modelFairAm: overPx.fairAm,
        modelVigAm: overPx.vigAm,
        modelEv: overPx.pWin * dO - 1,
      });
      legs.push({
        ...base,
        side: "under",
        oddsAm: uAm,
        decimal: dU,
        fairDecimal: fairDecU,
        pWin: underPx.pWin,
        modelFairDec: underPx.fairDec,
        modelVigDec: underPx.vigDec,
        modelFairAm: underPx.fairAm,
        modelVigAm: underPx.vigAm,
        modelEv: underPx.pWin * dU - 1,
      });
    }
    return legs;
  }

  function scoreParlay(legs) {
    const dkNaiveDecimal = legs.reduce((p, l) => p * l.decimal, 1);
    const dkIndepProb = legs.reduce((p, l) => p * legPostedProb(l), 1);
    const dkJointProb = jointProbForLegs(legs, legPostedProb);
    const useDkCorr = maxPairRho(legs) > 0.001;
    const dkDecimal = useDkCorr
      ? Number.isFinite(dkJointProb) && dkJointProb > 0
        ? 1 / dkJointProb
        : dkNaiveDecimal
      : dkNaiveDecimal;
    const rawIndepProb = legs.reduce((p, l) => p * l.pWin, 1);
    const rawJointProb = jointProbForLegs(legs, (l) => l.pWin);
    const indepProb = legs.reduce((p, l) => p * legCalibMarginal(l, false), 1);
    const jointProb = jointCalibProb(legs, false);
    const avgRho = avgPairRho(legs);
    const modelLegStackDecimal = legs.reduce((p, l) => p * l.modelFairDec, 1);
    const modelVigJointProb = jointCalibProb(legs, true);
    const modelFairDecimal =
      Number.isFinite(jointProb) && jointProb > 0 ? 1 / jointProb : modelLegStackDecimal;
    const modelVigDecimal =
      Number.isFinite(modelVigJointProb) && modelVigJointProb > 0
        ? 1 / modelVigJointProb
        : legs.reduce((p, l) => p * l.modelVigDec, 1);
    const indepFairDecimal = modelLegStackDecimal;
    const modelEv = Number.isFinite(jointProb) && Number.isFinite(dkDecimal) ? jointProb * dkDecimal - 1 : NaN;
    const indepEv = Number.isFinite(indepProb) && Number.isFinite(dkDecimal) ? indepProb * dkDecimal - 1 : NaN;
    const edgeVsDk = Number.isFinite(jointProb) && Number.isFinite(dkJointProb) ? jointProb - dkJointProb : NaN;
    const rawEdgeVsDk =
      Number.isFinite(rawJointProb) && Number.isFinite(dkJointProb) ? rawJointProb - dkJointProb : NaN;
    const corrUplift = indepProb > 1e-9 && Number.isFinite(jointProb) ? jointProb / indepProb : 1;
    const dkCorrUplift = dkIndepProb > 1e-9 && Number.isFinite(dkJointProb) ? dkJointProb / dkIndepProb : 1;
    const comboType = parlayComboType(legs);
    return {
      legs,
      comboType,
      avgRho,
      dkDecimal,
      dkNaiveDecimal,
      dkJointProb,
      dkIndepProb,
      dkCorrUplift,
      dkAmerican: api.americanFromDecimal(dkDecimal),
      dkNaiveAmerican: api.americanFromDecimal(dkNaiveDecimal),
      modelFairDecimal,
      modelFairAmerican: api.americanFromDecimal(modelFairDecimal),
      modelVigDecimal,
      modelVigAmerican: api.americanFromDecimal(modelVigDecimal),
      indepFairDecimal,
      indepFairAmerican: api.americanFromDecimal(indepFairDecimal),
      jointProb,
      indepProb,
      rawJointProb,
      rawIndepProb,
      rawEdgeVsDk,
      modelEv,
      indepEv,
      edgeVsDk,
      corrUplift,
      evGainVsIndep: Number.isFinite(modelEv) && Number.isFinite(indepEv) ? modelEv - indepEv : NaN,
      isNegCorr: avgRho < -0.04 && corrUplift < 0.98,
    };
  }

  function isValidParlayScore(r) {
    return (
      Number.isFinite(r.jointProb) &&
      Number.isFinite(r.dkJointProb) &&
      Number.isFinite(r.modelEv) &&
      Number.isFinite(r.modelFairDecimal) &&
      r.modelFairDecimal > 1 &&
      Number.isFinite(r.modelVigDecimal) &&
      r.modelVigDecimal > 1 &&
      Number.isFinite(r.dkAmerican) &&
      Number.isFinite(r.edgeVsDk) &&
      Number.isFinite(r.avgRho)
    );
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
      const s = scoreParlay(combo);
      if (isValidParlayScore(s)) scored.push(s);
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

  function fmtModelDec(dec) {
    if (!Number.isFinite(dec) || dec <= 1) return "N/A";
    const s = api.modelAmericanFromProb(1 / dec);
    return s === "—" ? "N/A" : s;
  }

  function fmtAm(x) {
    if (!Number.isFinite(x)) return "N/A";
    const f = api.formatAmerican(x);
    return f === "—" ? "N/A" : f;
  }

  function fmtPct(x) {
    return Number.isFinite(x) ? `${(x * 100).toFixed(1)}%` : "N/A";
  }

  function fmtEv(x) {
    return Number.isFinite(x) ? `${x >= 0 ? "+" : ""}${(x * 100).toFixed(1)}%` : "N/A";
  }

  function fmtEdge(x) {
    if (!Number.isFinite(x)) return "N/A";
    return `${x >= 0 ? "+" : ""}${(x * 100).toFixed(1)}%`;
  }

  function fmtRho(x) {
    if (!Number.isFinite(x)) return "N/A";
    return `${x >= 0 ? "+" : ""}${x.toFixed(2)}`;
  }

  function typeLabel(t) {
    if (t === "same_player") return "Same player";
    if (t === "same_market_wave") return "Same market · wave";
    if (t === "same_market") return "Same market";
    return "Mixed";
  }

  function sentimentLabel(leg) {
    const s = legScoringSentiment(leg);
    if (s === "good") return "good-day";
    if (s === "bad") return "bad-day";
    return "neutral";
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
        <span class="parlay-pro-hero-value ${best.modelEv >= 0 ? "ev-pos" : "ev-neg"}">${fmtEv(best.modelEv)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label" title="Fair parlay price from calibrated leg win rates + same correlation structure as DK">Model odds (fair)</span>
        <span class="parlay-pro-hero-value">${fmtModelDec(best.modelFairDecimal)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">Model odds (+ vig)</span>
        <span class="parlay-pro-hero-value">${fmtModelDec(best.modelVigDecimal)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label">DK parlay</span>
        <span class="parlay-pro-hero-value">${api.formatAmerican(best.dkAmerican)}</span>
      </div>
      <div class="parlay-pro-hero-stat">
        <span class="parlay-pro-hero-label" title="Correlation-adjusted win % (model blended toward DK devigged at each leg, same copula as DK)">Model win %</span>
        <span class="parlay-pro-hero-value">${fmtPct(best.jointProb)}</span>
      </div>
    </div>`;

    html += `<table class="data-table parlay-pro-table"><thead><tr>
      <th>#</th><th>Type</th><th>Legs</th>
      <th>Model fair</th><th>Model + vig</th><th>DK</th>
      <th>EV</th><th>Win edge</th><th>ρ</th>
    </tr></thead><tbody>`;

    rows.forEach((r, i) => {
      const legHtml = r.legs
        .map((l) => {
          const m = l.market.replace("Fairways hit", "FW").replace("Total Score", "Score");
          const w = l.teeWave ? ` · ${l.teeWave.slice(0, 3)}` : "";
          const dk = api.formatAmerican(l.oddsAm);
          const mv = fmtAm(l.modelVigAm);
          const legFair = fmtAm(l.modelFairAm);
          const dkPct = fmtPct(1 / l.decimal);
          const modelPct = fmtPct(l.pWin);
          return `<span class="parlay-leg-chip" title="${sentimentLabel(l)} · historical ρ when available · Model ${legFair} (${modelPct}) · DK ${dk} (${dkPct}) · model +vig ${mv}">${l.playerName.split(",")[0]} ${m} ${l.side === "over" ? "O" : "U"}${l.line}${w} <span class="parlay-leg-model">${mv}</span> <span class="parlay-leg-odds">${dk}</span></span>`;
        })
        .join("");
      html += `<tr>
        <td>${i + 1}</td>
        <td class="parlay-type-cell">${typeLabel(r.comboType)}${r.isNegCorr ? ' <span class="parlay-neg-tag">neg ρ</span>' : ""}</td>
        <td class="parlay-legs-cell">${legHtml}</td>
        <td>${fmtModelDec(r.modelFairDecimal)}</td>
        <td>${fmtModelDec(r.modelVigDecimal)}</td>
        <td>${fmtAm(r.dkAmerican)}${r.legs.length > 1 && r.dkCorrUplift > 1.02 ? `<span class="parlay-dk-indep-hint" title="Uncorrelated leg product would be ${fmtAm(r.dkNaiveAmerican)}">*</span>` : ""}</td>
        <td class="${r.modelEv >= 0 ? "ev-pos" : "ev-neg"}">${fmtEv(r.modelEv)}</td>
        <td class="${r.edgeVsDk >= 0 ? "ev-pos" : "ev-neg"}" title="Calibrated model win % minus DK parlay implied %${Number.isFinite(r.rawEdgeVsDk) ? ` · raw model edge ${fmtEdge(r.rawEdgeVsDk)}` : ""}">${fmtEdge(r.edgeVsDk)}</td>
        <td>${fmtRho(r.avgRho)}</td>
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
