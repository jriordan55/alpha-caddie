/** Browser-safe summary of live projection factors from projections.json meta. */

export function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function liveMeta(projections) {
  if (!projections || typeof projections !== "object") return {};
  return projections.meta && typeof projections.meta === "object"
    ? { ...projections, ...projections.meta }
    : projections;
}

function courseBasis(projections) {
  const meta = liveMeta(projections);
  return meta.projection_course_basis || projections?.projection_course_basis || {};
}

export function formatDominantSg(key) {
  const k = String(key || "").replace("sg_", "").toUpperCase();
  const labels = { OTT: "Off the tee", APP: "Approach", ARG: "Around green", PUTT: "Putting" };
  return labels[k] || k || "—";
}

export function formatSgWeightPct(w) {
  const v = num(w, NaN);
  return Number.isFinite(v) ? `${Math.round(v * 100)}%` : "—";
}

/** Tags from per-player course tailoring shifts baked into projections.json. */
export function courseTailoringTags(player) {
  const shifts = player?.course_tailoring_shifts;
  if (!shifts || typeof shifts !== "object") return [];
  const tags = [];
  const dom = String(shifts.dominant_sg || "").replace("sg_", "").toUpperCase();
  if (dom) tags.push(`Course fit: ${dom}`);
  const form = num(shifts.form, NaN);
  if (Number.isFinite(form) && Math.abs(form) >= 0.04) {
    tags.push(form < 0 ? "Hot recent form" : "Cold recent form");
  }
  const skill = num(shifts.skill, NaN);
  if (Number.isFinite(skill) && Math.abs(skill) >= 0.05) {
    tags.push(skill < 0 ? "Course-skill edge" : "Course-skill fade");
  }
  const bird = num(shifts.bird_count, NaN);
  if (Number.isFinite(bird) && Math.abs(bird) >= 0.15) {
    tags.push(bird > 0 ? "Birdie-heavy boost" : "Birdie-heavy trim");
  }
  const tee = num(shifts.tee_wave, NaN);
  if (Number.isFinite(tee) && Math.abs(tee) >= 0.03) {
    tags.push(tee > 0 ? "Afternoon wave harder" : "Morning wave harder");
  }
  return tags;
}

export function buildLiveProjectionFactorsSummary(projections) {
  const meta = liveMeta(projections);
  const basis = courseBasis(projections);
  const sg = basis.course_sg_importance;
  const unified = meta.projection_unified_factors;
  const teeDelta = num(unified?.tee_wave_bias?.deltaAfternoonMinusMorning, NaN);

  /** @type {{ label: string, value: string, tone?: string }[]} */
  const chips = [];

  const event = String(projections?.event_name || meta.event_name || "").trim();
  const course = String(projections?.course_used || meta.course_used || "").trim();
  if (event) chips.push({ label: "Event", value: event });
  if (course) chips.push({ label: "Course", value: course });

  if (sg?.dominant_sg) {
    chips.push({
      label: "Venue emphasizes",
      value: formatDominantSg(sg.dominant_sg),
      tone: "accent",
    });
  }

  const stp = num(basis.venue_avg_score_to_par, NaN);
  if (Number.isFinite(stp)) {
    chips.push({
      label: "Venue scoring",
      value: `${stp >= 0 ? "+" : ""}${stp.toFixed(2)} vs par`,
      tone: stp < 0 ? "good" : stp > 1 ? "warn" : "",
    });
  }

  const bird = num(basis.venue_avg_birdies, NaN);
  // Skip diluted stub-zero anchors (< ~1.85 on a real PGA venue is almost always contamination).
  if (Number.isFinite(bird) && bird >= 1.85) {
    chips.push({
      label: "Venue birdies",
      value: `~${bird.toFixed(1)}/rd`,
      tone: bird >= 4.5 ? "good" : "",
    });
  }

  if (meta.projection_counts_weather_baked) {
    const rnd = Math.round(num(meta.projection_counts_weather_baked_round, NaN));
    chips.push({
      label: "Weather",
      value: Number.isFinite(rnd) && rnd >= 1 ? `baked into R${rnd}` : "baked into counts",
    });
  } else {
    chips.push({ label: "Weather", value: "applied at pricing time" });
  }

  if (meta.pin_sheet && typeof meta.pin_sheet === "object") {
    chips.push({ label: "Pin sheet", value: "active" });
  }

  if (meta.projection_round_adjustments?.unified_factors_applied) {
    chips.push({ label: "Unified factors", value: "baked" });
  }

  if (Number.isFinite(teeDelta) && Math.abs(teeDelta) >= 0.04) {
    chips.push({
      label: "Tee wave",
      value: `afternoon ${teeDelta >= 0 ? "+" : ""}${teeDelta.toFixed(2)} stp vs morning`,
    });
  } else if (String(meta.forecast_wave_summary || "").trim()) {
    chips.push({ label: "Forecast", value: String(meta.forecast_wave_summary).trim() });
  }

  if (meta.in_play_affects_round_odds === true) {
    chips.push({ label: "In-round", value: "live scratch on" });
  }

  const sgBars = sg?.weights
    ? [
        { key: "OTT", pct: num(sg.weights.ott, 0) },
        { key: "APP", pct: num(sg.weights.app, 0) },
        { key: "ARG", pct: num(sg.weights.arg, 0) },
        { key: "PUTT", pct: num(sg.weights.putt, 0) },
      ].filter((b) => b.pct > 0.02)
    : [];

  return {
    chips,
    sgBars,
    sgSource: sg?.source || "",
    sgVenueRounds: Math.round(num(sg?.n_venue_rounds, 0)) || 0,
    recentFormWindow: "8–12 rounds vs stable baseline",
  };
}
