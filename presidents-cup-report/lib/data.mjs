import { readFileSync, existsSync } from "fs";
import { join, dirname, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
export const REPO_ROOT = resolve(__dirname, "../..");
export const WEB_ROOT = join(REPO_ROOT, "alpha-caddie-web");

export const MEDINAH_HOLE_PARS = [4, 3, 4, 4, 5, 4, 5, 4, 4, 5, 3, 4, 3, 4, 4, 4, 3, 5];
export const COURSE_PAR = 72;
export const MEDINAH_ADJ_STP = -1.16;

/** Presidents Cup 2026 — 30 cup points (5+5+4+4 team + 12 singles), first to 15.5. */
export const SESSIONS = [
  { id: "fri_foursomes", label: "Friday — Foursomes", format: "foursomes", matches: 5 },
  { id: "fri_fourball", label: "Friday — Fourball", format: "fourball", matches: 5 },
  { id: "sat_foursomes", label: "Saturday — Foursomes", format: "foursomes", matches: 4 },
  { id: "sat_fourball", label: "Saturday — Fourball", format: "fourball", matches: 4 },
  { id: "sun_singles", label: "Sunday — Singles", format: "singles", matches: 12 },
];

export const CUP_WIN_TARGET = 15.5;
export const CUP_TOTAL_POINTS = 30;

/** Medinah home bump for Team USA (strokes-gained per round). */
export const USA_HOME_SG = Number(process.env.PC_USA_HOME_SG) || 0.32;

export function loadProjections(path = join(WEB_ROOT, "projections-pga.json")) {
  if (!existsSync(path)) throw new Error(`Missing projections: ${path}`);
  return JSON.parse(readFileSync(path, "utf8"));
}

export function loadPlayers(proj) {
  const holePars = Array.isArray(proj.hole_pars) && proj.hole_pars.length === 18 ? proj.hole_pars : MEDINAH_HOLE_PARS;
  const rows = (proj.players || []).filter((p) => Math.round(Number(p.round)) === 1);
  const byId = new Map();
  for (const p of rows) {
    const id = Math.round(Number(p.dg_id));
    if (!Number.isFinite(id)) continue;
    byId.set(id, {
      dg_id: id,
      name: String(p.player_name || "").trim(),
      country: String(p.country || "").trim(),
      team: String(p.country || "").trim() === "USA" ? "USA" : "INT",
      mu_sg: Number(p.mu_sg),
      birdies: Number(p.birdies),
      bogeys: Number(p.bogeys),
      pars: Number(p.pars),
      total_score: Number(p.total_score),
      sg_ott: Number(p.sg_ott),
      sg_app: Number(p.sg_app),
      sg_arg: Number(p.sg_arg),
      sg_putt: Number(p.sg_putt),
      round_sd: Number(p.round_sd) || 2.75,
    });
  }
  const players = [...byId.values()].sort((a, b) => b.mu_sg - a.mu_sg);
  const usa = players.filter((p) => p.team === "USA");
  const intl = players.filter((p) => p.team === "INT");
  return {
    event_name: proj.event_name || "Presidents Cup",
    course: proj.course_used || "Medinah Country Club",
    date_start: proj.datagolf_field_date_start || "",
    hole_pars: holePars,
    course_adj_stp: Number(proj.projection_course_basis?.course_adj_score_to_par) || MEDINAH_ADJ_STP,
    players,
    usa,
    intl,
  };
}
