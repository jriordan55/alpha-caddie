/**
 * Pin-sheet setup difficulty from ShotLink-style hole diagrams (green depth, pin from front/side).
 */

export function num(v, fallback = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fallback;
}

export function clamp(x, lo, hi) {
  return Math.min(hi, Math.max(lo, x));
}

export function eventSlug(eventName) {
  return String(eventName || "")
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

/**
 * @param {{ hole: number, green_depth_yds?: number, pin_from_front_yds?: number, pin_from_side_yds?: number, pin_side?: string, near_hazard?: boolean, note?: string }} h
 */
export function holePinDifficulty(h) {
  const depth = num(h.green_depth_yds, NaN);
  const front = num(h.pin_from_front_yds, NaN);
  const side = num(h.pin_from_side_yds, NaN);
  if (!Number.isFinite(depth) || depth < 18) {
    return { hole: h.hole, score: 0, back: 0, tuck: 0, note: h.note || "missing depth" };
  }
  const back = clamp(front / depth, 0, 1);
  const tuck = Number.isFinite(side) ? clamp(1 - side / 9, 0, 1) : 0;
  const hazard = h.near_hazard ? 0.12 : 0;
  const score = clamp(0.52 * back + 0.38 * tuck + hazard, 0, 1.25);
  return {
    hole: Math.round(num(h.hole, 0)),
    score,
    back,
    tuck,
    note: h.note || "",
  };
}

/**
 * @param {Array<object>} holes — 18 entries with hole 1..18
 * @returns round-level projection deltas (strokes / counting stats)
 */
export function roundAdjustmentsFromPinSheet(holes) {
  const list = Array.isArray(holes) ? holes : [];
  const perHole = [];
  let sum = 0;
  let n = 0;
  for (const raw of list) {
    const d = holePinDifficulty(raw);
    if (d.hole >= 1 && d.hole <= 18) {
      perHole.push(d);
      sum += d.score;
      n++;
    }
  }
  const scores = perHole.map((h) => h.score).sort((a, b) => b - a);
  const meanAll = n ? sum / n : 0;
  const topK = Math.min(6, scores.length);
  const meanHardest = topK ? scores.slice(0, topK).reduce((a, b) => a + b, 0) / topK : meanAll;
  const avg = 0.45 * meanAll + 0.55 * meanHardest;
  const neutral = 0.4;
  const excess = avg - neutral;

  const totalScoreDelta = clamp(excess * 1.25, -0.4, 0.95);
  const birdiesDelta = clamp(-excess * 1.1, -1.0, 0.45);
  const bogeysDelta = clamp(excess * 0.95, -0.45, 1.0);
  const parsDelta = clamp(-excess * 0.15, -0.5, 0.35);
  const girDelta = clamp(-excess * 0.35, -1.2, 0.5);
  const fairwaysDelta = clamp(-excess * 0.12, -0.5, 0.25);

  const hard = perHole
    .filter((h) => h.score >= 0.52)
    .map((h) => h.hole)
    .sort((a, b) => a - b);
  const easy = perHole
    .filter((h) => h.score <= 0.28)
    .map((h) => h.hole)
    .sort((a, b) => a - b);

  return {
    avgDifficulty: avg,
    excess,
    totalScoreDelta,
    birdiesDelta,
    bogeysDelta,
    parsDelta,
    girDelta,
    fairwaysDelta,
    perHole,
    hardHoles: hard,
    easyHoles: easy,
    summary: buildPinSummary(avg, excess, totalScoreDelta, hard, easy, neutral),
  };
}

function buildPinSummary(avg, excess, totalDelta, hard, easy, neutral = 0.4) {
  const dir = excess > 0.08 ? "harder" : excess < -0.08 ? "easier" : "neutral";
  const parts = [
    `Pin setup ${dir} (${avg.toFixed(2)} avg vs ${neutral} neutral)`,
    `total ${totalDelta >= 0 ? "+" : ""}${totalDelta.toFixed(2)} strokes`,
  ];
  if (hard.length) parts.push(`tough holes ${hard.join(",")}`);
  if (easy.length) parts.push(`easier holes ${easy.join(",")}`);
  return parts.join("; ");
}
