const OUTRIGHTS_MARKETS = ["win", "top_5", "top_10", "top_20", "make_cut", "mc", "frl"];
const OUTRIGHTS_ROW_SKIP_KEYS = new Set(["dg_id", "id", "player_name", "name"]);

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
}

function sleep(ms) {
  return new Promise((r) => setTimeout(r, ms));
}

/** Parse DataGolf 429 body: "Your suspension will end in 5 minutes." */
function suspensionWaitMsFromBody(text) {
  const m = String(text || "").match(/end in (\d+)\s*minute/i);
  if (m) return Math.min(600_000, Math.max(30_000, parseInt(m[1], 10) * 60_000));
  return null;
}

function rowsFromOutrightsResponse(raw) {
  if (raw == null) return [];
  if (Array.isArray(raw)) return raw;
  if (typeof raw !== "object") return [];
  for (const key of ["odds", "data", "field", "players"]) {
    if (Array.isArray(raw[key])) return raw[key];
  }
  return [];
}

function impliedPctFromOddsValue(value, oddsFormat) {
  const x = num(value, NaN);
  if (!Number.isFinite(x) || x <= 0) return NaN;
  const fmt = String(oddsFormat || "percent").trim().toLowerCase();
  if (fmt === "decimal") {
    if (x > 1 && x < 20000) return (1 / x) * 100;
    if (x > 0 && x <= 1) return x * 100;
    return NaN;
  }
  if (fmt === "american") {
    if (x > 0) return (100 / (x + 100)) * 100;
    if (x < 0) return (Math.abs(x) / (Math.abs(x) + 100)) * 100;
    return NaN;
  }
  if (fmt === "fraction") return NaN;
  return x > 1 ? x : x * 100;
}

function pickDataGolfModelPct(value, oddsFormat) {
  if (value == null) return NaN;
  if (typeof value === "object" && !Array.isArray(value)) {
    const preferred = value.baseline_history_fit ?? value.baseline ?? value.dg_baseline ?? value.model;
    return impliedPctFromOddsValue(preferred, oddsFormat);
  }
  return impliedPctFromOddsValue(value, oddsFormat);
}

function parseDataGolfOutrightsResponse(raw, oddsFormat = "percent") {
  const rows = [];
  const bookKeys = new Set();
  for (const row of rowsFromOutrightsResponse(raw)) {
    if (!row || typeof row !== "object") continue;
    const dg_id = Math.round(num(row.dg_id ?? row.id, NaN));
    const player_name = String(row.player_name ?? row.name ?? "").trim();
    if (!Number.isFinite(dg_id) || !player_name) continue;
    const out = { dg_id, player_name };
    const dgModelPct = pickDataGolfModelPct(row.datagolf, oddsFormat);
    if (Number.isFinite(dgModelPct) && dgModelPct > 0) out.dg_model = dgModelPct;
    for (const [rawKey, rawVal] of Object.entries(row)) {
      const key = String(rawKey || "").toLowerCase();
      if (OUTRIGHTS_ROW_SKIP_KEYS.has(key) || key === "datagolf") continue;
      let value = rawVal;
      if (value != null && typeof value === "object" && !Array.isArray(value)) {
        const vals = Object.values(value);
        value = vals.length ? vals[0] : null;
      }
      if (Array.isArray(value)) value = value[0];
      const pct = impliedPctFromOddsValue(value, oddsFormat);
      if (!Number.isFinite(pct) || pct <= 0) continue;
      out[key] = pct;
      bookKeys.add(key);
    }
    rows.push(out);
  }
  return { rows, bookKeys: [...bookKeys].sort() };
}

async function fetchDataGolfJson(path, params, apiKey) {
  const url = new URL(`https://feeds.datagolf.com${path}`);
  for (const [key, value] of Object.entries(params || {})) url.searchParams.set(key, String(value));
  url.searchParams.set("key", apiKey);

  const maxAttempts = Math.max(4, Math.min(20, Number(process.env.GOLF_DG_MAX_ATTEMPTS || 12)));
  let lastStatus;
  let lastBody = "";

  for (let attempt = 1; attempt <= maxAttempts; attempt++) {
    const res = await fetch(url.toString(), { headers: { Accept: "application/json" } });
    if (res.ok) return res.json();

    lastStatus = res.status;
    lastBody = await res.text().catch(() => "");

    if ([429, 500, 502, 503, 504].includes(res.status)) {
      let waitMs = Math.min(25_000 + attempt * 8_000, 120_000);
      const suspensionMs = suspensionWaitMsFromBody(lastBody);
      if (suspensionMs != null) waitMs = Math.max(waitMs, suspensionMs);
      const ra = res.headers.get("retry-after");
      if (ra) {
        const sec = parseInt(ra, 10);
        if (Number.isFinite(sec) && sec > 0) waitMs = Math.max(waitMs, sec * 1000);
      }
      console.warn(
        `[datagolf-outrights] ${path} HTTP ${res.status} retry ${attempt}/${maxAttempts}; waiting ${Math.round(waitMs / 1000)}s…`,
      );
      await sleep(waitMs);
      continue;
    }

    throw new Error(`${path} HTTP ${res.status}: ${lastBody}`);
  }

  throw new Error(`${path} HTTP ${lastStatus ?? "?"} after ${maxAttempts} attempts: ${lastBody}`);
}

/**
 * @param {{ apiKey: string, tour?: string, oddsFormat?: string, markets?: string[], delayMs?: number }} opts
 */
export async function fetchDataGolfOutrightsApi({
  apiKey,
  tour = "pga",
  oddsFormat = "percent",
  markets = OUTRIGHTS_MARKETS,
  delayMs,
} = {}) {
  if (!apiKey) throw new Error("Missing DataGolf API key");
  const pauseMs = Math.max(
    0,
    Number(delayMs ?? process.env.GOLF_DG_OUTRIGHTS_DELAY_MS ?? process.env.GOLF_DG_ROUNDS_DELAY_MS ?? 2000),
  );
  const outrights = {};
  const logs = [];
  const list = [...markets];

  for (let i = 0; i < list.length; i++) {
    const market = list[i];
    if (i > 0 && pauseMs > 0) await sleep(pauseMs);
    const raw = await fetchDataGolfJson(
      "/betting-tools/outrights",
      { tour, market, odds_format: oddsFormat, file_format: "json" },
      apiKey,
    );
    const pack = parseDataGolfOutrightsResponse(raw, oddsFormat);
    if (pack.rows.length) outrights[market] = pack;
    logs.push(`${market}: ${pack.rows.length} rows, books=${pack.bookKeys.join(",") || "(none)"}`);
  }
  return { outrights, logs };
}
