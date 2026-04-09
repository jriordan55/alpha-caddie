/** American ↔ decimal and juiced two-way O/U (matches app.R spirit). */

const OU_HOLD = 0.048;

export function decimalToAmerican(decimal: number): number {
  if (!Number.isFinite(decimal) || decimal <= 1) return NaN;
  if (decimal >= 2) return (decimal - 1) * 100;
  return -100 / (decimal - 1);
}

export function americanToDecimal(american: number): number {
  if (!Number.isFinite(american)) return NaN;
  if (american > 0) return 1 + american / 100;
  return 1 + 100 / Math.abs(american);
}

export function formatAmerican(american: number): string {
  if (!Number.isFinite(american)) return "—";
  return american > 0 ? `+${Math.round(american)}` : String(Math.round(american));
}

export function juicedAmericanOuPair(pOver: number, pUnder: number, hold = OU_HOLD): [number, number] {
  let po = pOver;
  let pu = pUnder;
  if (!Number.isFinite(po) || !Number.isFinite(pu)) return [NaN, NaN];
  const s = po + pu;
  if (s <= 1e-12) return [NaN, NaN];
  po /= s;
  pu /= s;
  po = Math.max(1e-8, Math.min(1 - 1e-8, po));
  pu = Math.max(1e-8, Math.min(1 - 1e-8, pu));
  const s2 = po + pu;
  po /= s2;
  pu /= s2;
  const h = Number.isFinite(hold) && hold >= 0 ? hold : OU_HOLD;
  const invO = po * (1 + h);
  const invU = pu * (1 + h);
  const decO = 1 / invO;
  const decU = 1 / invU;
  return [decimalToAmerican(decO), decimalToAmerican(decU)];
}

export function modelTotalScoreOuHtml(mu: number, sigma: number, line: number): string {
  if (!Number.isFinite(mu)) return "—";
  let sig = Number.isFinite(sigma) && sigma > 0.25 ? sigma : 2.75;
  sig = Math.max(sig, 0.35);
  const pUnder = normalCdf(line + 0.5, mu, sig);
  let pOver = 1 - pUnder;
  pOver = Math.max(1e-8, Math.min(1 - 1e-8, pOver));
  const pU = Math.max(1e-8, Math.min(1 - 1e-8, pUnder));
  const [oa, ua] = juicedAmericanOuPair(pOver, pU);
  return `<div class="ou-prices"><span class="ou-o">O ${formatAmerican(oa)}</span><br/><span class="ou-u">U ${formatAmerican(ua)}</span></div>`;
}

export function modelCountOuHtml(lambda: number, line: number): string {
  if (!Number.isFinite(lambda) || lambda < 0) return "—";
  const k = Math.max(0, Math.floor(line));
  const pUnder = poissonCdf(k, lambda);
  if (!Number.isFinite(pUnder)) return "—";
  let pOver = 1 - pUnder;
  pOver = Math.max(1e-8, Math.min(1 - 1e-8, pOver));
  const pU = Math.max(1e-8, Math.min(1 - 1e-8, pUnder));
  const [oa, ua] = juicedAmericanOuPair(pOver, pU);
  return `<div class="ou-prices"><span class="ou-o">O ${formatAmerican(oa)}</span><br/><span class="ou-u">U ${formatAmerican(ua)}</span></div>`;
}

function normalCdf(x: number, mean: number, sd: number): number {
  const z = (x - mean) / (sd * Math.SQRT2);
  return 0.5 * (1 + erf(z));
}

function erf(x: number): number {
  const sign = x < 0 ? -1 : 1;
  const ax = Math.abs(x);
  const a1 = 0.254829592;
  const a2 = -0.284496736;
  const a3 = 1.421413741;
  const a4 = -1.453152027;
  const a5 = 1.061405429;
  const p = 0.3275911;
  const t = 1 / (1 + p * ax);
  const y = 1 - (((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t) * Math.exp(-ax * ax);
  return sign * y;
}

function poissonCdf(k: number, lambda: number): number {
  if (lambda <= 0) return k >= 0 ? 1 : 0;
  let sum = 0;
  for (let i = 0; i <= k; i++) {
    sum += (Math.exp(-lambda) * Math.pow(lambda, i)) / factorial(i);
  }
  return Math.min(1, sum);
}

function factorial(n: number): number {
  if (n <= 1) return 1;
  let r = 1;
  for (let i = 2; i <= n; i++) r *= i;
  return r;
}

export const LB_OU_LINES: Record<string, number[]> = {
  "Total score": Array.from({ length: 11 }, (_, i) => 64 + i),
  Birdies: Array.from({ length: 7 }, (_, i) => i),
  Pars: Array.from({ length: 11 }, (_, i) => 5 + i),
  Bogeys: Array.from({ length: 7 }, (_, i) => i),
  GIR: Array.from({ length: 11 }, (_, i) => 5 + i),
  "Fairways hit": Array.from({ length: 11 }, (_, i) => 5 + i),
};
