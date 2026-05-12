const OUTRIGHTS_MARKETS = ["win", "top_5", "top_10", "top_20", "make_cut", "mc", "frl"];
const OUTRIGHTS_ROW_SKIP_KEYS = new Set(["dg_id", "id", "player_name", "name"]);

function num(x, fallback = NaN) {
  const n = Number(x);
  return Number.isFinite(n) ? n : fallback;
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
  const res = await fetch(url.toString(), { headers: { Accept: "application/json" } });
  if (!res.ok) throw new Error(`${path} HTTP ${res.status}: ${await res.text().catch(() => "")}`);
  return res.json();
}

export async function fetchDataGolfOutrightsApi({ apiKey, tour = "pga", oddsFormat = "percent", markets = OUTRIGHTS_MARKETS } = {}) {
  if (!apiKey) throw new Error("Missing DataGolf API key");
  const outrights = {};
  const logs = [];
  for (const market of markets) {
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

