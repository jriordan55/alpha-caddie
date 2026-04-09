/**
 * In dev, Vite proxies /api/datagolf/* → feeds.datagolf.com with DATAGOLF_API_KEY.
 * In static production hosting, set same-origin proxy (nginx, Cloudflare Worker) or ship JSON only.
 */
const BASE = "/api/datagolf";

export async function fetchJson<T>(path: string, query: Record<string, string> = {}): Promise<T | null> {
  const q = new URLSearchParams(query);
  const url = `${BASE}/${path.replace(/^\//, "")}?${q.toString()}`;
  try {
    const r = await fetch(url);
    if (!r.ok) return null;
    return (await r.json()) as T;
  } catch {
    return null;
  }
}

export type FieldPlayer = {
  player_name?: string;
  name?: string;
  dg_id?: number;
  country?: string;
  teetime_upcoming?: string;
  r1_teetime?: string;
  r2_teetime?: string;
  r3_teetime?: string;
  r4_teetime?: string;
};

export type FieldUpdates = {
  event_name?: string;
  course_name?: string;
  date_start?: string;
  field?: FieldPlayer[] | Record<string, unknown>[];
};
