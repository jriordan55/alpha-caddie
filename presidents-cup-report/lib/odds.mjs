/** Convert model probability to American odds and edge vs book. */

export function probToAmerican(p) {
  const x = Number(p);
  if (!Number.isFinite(x) || x <= 0 || x >= 1) return null;
  if (x >= 0.5) return Math.round(-100 * (x / (1 - x)));
  return Math.round(100 * ((1 - x) / x));
}

export function americanToProb(american) {
  const a = Number(american);
  if (!Number.isFinite(a) || a === 0) return null;
  if (a > 0) return 100 / (a + 100);
  return -a / (-a + 100);
}

export function americanToDecimal(american) {
  const p = americanToProb(american);
  if (p == null || p <= 0) return null;
  return 1 / p;
}

export function decimalToAmerican(decimal) {
  const d = Number(decimal);
  if (!Number.isFinite(d) || d <= 1) return null;
  const p = 1 / d;
  return probToAmerican(p);
}

export function edgePct(modelProb, bookAmerican) {
  const bookP = americanToProb(bookAmerican);
  const m = Number(modelProb);
  if (!Number.isFinite(m) || bookP == null) return null;
  return (m - bookP) * 100;
}

export function evPerUnit(modelProb, bookAmerican) {
  const dec = americanToDecimal(bookAmerican);
  const m = Number(modelProb);
  if (!Number.isFinite(m) || dec == null) return null;
  return m * (dec - 1) - (1 - m);
}

export function formatAmerican(a) {
  if (a == null || !Number.isFinite(a)) return "—";
  return a > 0 ? `+${Math.round(a)}` : String(Math.round(a));
}

export function formatPct(p, digits = 1) {
  if (!Number.isFinite(p)) return "—";
  return `${(p * 100).toFixed(digits)}%`;
}
