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
    return { hole: h.hole, score: 0, frontTuck: 0, sideTuck: 0, note: h.note || "missing depth" };
  }
  /** Low pin_from_front = tucked front pin (hard); was inverted when using front/depth as "back". */
  const frontFrac = Number.isFinite(front) ? clamp(front / depth, 0, 1) : 0.5;
  let frontTuck = clamp(1 - frontFrac / 0.52, 0, 1);
  if (Number.isFinite(front) && front <= 10) {
    frontTuck = Math.max(frontTuck, clamp(0.52 + (10 - front) * 0.04, 0, 1));
  }
  const backPin = clamp((frontFrac - 0.78) / 0.22, 0, 1);
  const sideTuck = Number.isFinite(side) ? clamp(1 - side / 8, 0, 1) : 0;
  const hazard = h.near_hazard ? 0.18 : 0;
  const score = clamp(0.5 * frontTuck + 0.32 * sideTuck + 0.1 * backPin + hazard, 0, 1.4);
  return {
    hole: Math.round(num(h.hole, 0)),
    score,
    frontTuck,
    sideTuck,
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
  const topK = Math.min(5, scores.length);
  const meanHardest = topK ? scores.slice(0, topK).reduce((a, b) => a + b, 0) / topK : meanAll;
  const avg = 0.35 * meanAll + 0.65 * meanHardest;
  const neutral = 0.28;
  const excess = avg - neutral;

  const totalScoreDelta = clamp(excess * 3.0, -0.75, 1.85);
  const birdiesDelta = clamp(-excess * 2.4, -1.8, 0.85);
  const bogeysDelta = clamp(excess * 2.1, -0.85, 1.8);
  const parsDelta = clamp(-excess * 0.35, -0.75, 0.55);
  const girDelta = clamp(-excess * 0.95, -2.5, 1.0);
  const fairwaysDelta = clamp(-excess * 0.35, -0.85, 0.45);

  const hard = perHole
    .filter((h) => h.score >= 0.42)
    .map((h) => h.hole)
    .sort((a, b) => a - b);
  const easy = perHole
    .filter((h) => h.score <= 0.2)
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

function buildPinSummary(avg, excess, totalDelta, hard, easy, neutral = 0.28) {
  const dir =
    excess > 0.04 ? "Harder than typical" : excess < -0.04 ? "Easier than typical" : "Near-average";
  const sign = totalDelta >= 0 ? "+" : "";
  const parts = [
    dir,
    `${sign}${totalDelta.toFixed(2)} on projected total`,
  ];
  return parts.join(" · ");
}
