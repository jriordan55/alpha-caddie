import { readFileSync } from "fs";
import { join } from "path";

const WEB = join(import.meta.dirname, "..");
const pga = JSON.parse(readFileSync(join(WEB, "data/pgatour_event_rounds.json"), "utf8"));
const rounds = pga.rounds || [];
console.log("meta", pga.meta);
console.log("total rounds", rounds.length, "from_pgatour", rounds.filter((r) => r._from_pgatour).length);
console.log("sample", rounds[0]);

// search Aberg / Ludvig
const hits = rounds.filter(
  (r) =>
    String(r.player_name || r.player || "").toLowerCase().includes("aberg") ||
    String(r.dg_id) === "23950",
);
console.log("aberg hits", hits.length, hits.slice(0, 3));

// unique players R1
const r1 = rounds.filter((r) => Number(r.round_num) === 1);
console.log("R1 count", r1.length, "with dg", r1.filter((r) => Number(r.dg_id) > 0).length);
console.log(
  "R1 without dg",
  r1
    .filter((r) => !(Number(r.dg_id) > 0))
    .slice(0, 20)
    .map((r) => ({ name: r.player_name || r.player, dg: r.dg_id })),
);

// names in pga not matching open field?
const names = r1.map((r) => String(r.player_name || r.player || "").trim()).filter(Boolean);
console.log("r1 names sample", names.slice(0, 10));
console.log("has Ludvig?", names.some((n) => /ludvig|aberg/i.test(n)));
console.log("has Scheffler?", names.some((n) => /scheffler/i.test(n)));
console.log("has McIlroy?", names.some((n) => /mcilroy/i.test(n)));
