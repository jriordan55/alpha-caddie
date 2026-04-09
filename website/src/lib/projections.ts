export type ProjectionPlayer = {
  player_name: string;
  dg_id?: number;
  country?: string;
  round?: number;
  total_score?: number;
  round_sd?: number;
  score_to_par?: number;
  gir?: number;
  fairways?: number;
  eagles?: number;
  birdies?: number;
  pars?: number;
  bogeys?: number;
  doubles?: number;
  teetime_upcoming?: string;
  course_used?: string;
};

export type ProjectionsFile = {
  event_name?: string;
  round?: number;
  updated_at?: string;
  players: ProjectionPlayer[];
};

export async function loadProjections(): Promise<ProjectionsFile | null> {
  const paths = ["/data/projections.json", "/data/projections.example.json"];
  for (const p of paths) {
    try {
      const r = await fetch(p, { cache: "no-store" });
      if (!r.ok) continue;
      const j = (await r.json()) as ProjectionsFile;
      if (j && Array.isArray(j.players) && j.players.length > 0) return j;
    } catch {
      /* next */
    }
  }
  return null;
}

export function filterByRound(data: ProjectionsFile, round: number): ProjectionPlayer[] {
  const rows = data.players;
  const hasRound = rows.some((r) => r.round != null);
  if (!hasRound) return rows;
  return rows.filter((r) => (r.round ?? 1) === round);
}

/** Auto round 1–4: mirror app.R ou_display_round_auto (America/New_York default). */
export function displayRoundAuto(now = new Date(), tz = "America/New_York"): number {
  const fmt = new Intl.DateTimeFormat("en-US", {
    timeZone: tz,
    weekday: "short",
    hour: "numeric",
    hour12: false,
    minute: "numeric",
  });
  const parts = fmt.formatToParts(now);
  const wd = parts.find((p) => p.type === "weekday")?.value ?? "Thu";
  const hour = parseInt(parts.find((p) => p.type === "hour")?.value ?? "12", 10);
  const min = parseInt(parts.find((p) => p.type === "minute")?.value ?? "0", 10);
  const h = hour + min / 60;
  const after9 = h >= 21;
  const wday: Record<string, number> = { Sun: 0, Mon: 1, Tue: 2, Wed: 3, Thu: 4, Fri: 5, Sat: 6 };
  const w = wday[wd.slice(0, 3)] ?? 4;
  if (w === 0 && after9) return 1;
  if (w >= 1 && w <= 3) return 1;
  if (w === 4 && !after9) return 1;
  if (w === 4 && after9) return 2;
  if (w === 5 && !after9) return 2;
  if (w === 5 && after9) return 3;
  if (w === 6 && !after9) return 3;
  if (w === 6 && after9) return 4;
  if (w === 0 && !after9) return 4;
  return 1;
}

export function displayRoundLabel(round: number, tz: string): string {
  const lab =
    round === 1
      ? "R1 — next Thursday"
      : round === 2
        ? "R2 — Friday"
        : round === 3
          ? "R3 — Saturday"
          : round === 4
            ? "R4 — Sunday"
            : `R${round}`;
  return `${lab} (auto, ${tz})`;
}

export function nameDisplay(name: string): string {
  if (!name.includes(",")) return name.trim();
  const p = name.split(/,\s*/);
  if (p.length >= 2) return `${p[1].trim()} ${p[0].trim()}`;
  return name.trim();
}

const ISO2: Record<string, string> = {
  USA: "us",
  ENG: "gb",
  SCO: "gb",
  WAL: "gb",
  GBR: "gb",
  IRL: "ie",
  AUS: "au",
  KOR: "kr",
  JPN: "jp",
  CAN: "ca",
};

export function flagImg(country: string | undefined, h = 12): string {
  if (!country) return "";
  const c = country.trim().toUpperCase();
  const iso = ISO2[c] || (c.length === 2 ? c.toLowerCase() : "");
  if (!iso) return "";
  return `<img src="https://flagcdn.com/w40/${iso}.png" height="${h}" alt="" style="vertical-align:middle;border-radius:2px"/>`;
}
