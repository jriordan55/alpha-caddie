import { readFileSync, existsSync } from "fs";
import { join } from "path";

const WEB = join(import.meta.dirname, "..");
const live = JSON.parse(readFileSync(join(WEB, "live-in-play.json"), "utf8"));
console.log("live top keys", Object.keys(live));
const lhs = live.live_hole_stats || live["preds/live-hole-stats"] || null;
console.log("lhs", lhs ? Object.keys(lhs).slice(0, 20) : null);
if (lhs?.data) {
  console.log("lhs.data type", Array.isArray(lhs.data), Array.isArray(lhs.data) ? lhs.data.length : typeof lhs.data);
  console.log("lhs sample", JSON.stringify(lhs.data?.[0] || lhs.data).slice(0, 600));
}
// search for hole-level in live
for (const k of Object.keys(live)) {
  const v = live[k];
  if (v && typeof v === "object" && JSON.stringify(v).toLowerCase().includes("hole_score")) {
    console.log("key with hole_score", k);
  }
}

// check map for Aberg
const mapPath = join(WEB, "..", "data", "pga_datagolf_player_map.csv");
if (existsSync(mapPath)) {
  const txt = readFileSync(mapPath, "utf8");
  const lines = txt.split(/\r?\n/).filter((l) => /23950|aberg|ludvig/i.test(l));
  console.log("map hits", lines.slice(0, 10));
} else {
  console.log("no map at", mapPath);
  // find map
}

import { readdirSync } from "fs";
function walk(dir, depth = 0) {
  if (depth > 2) return;
  for (const name of readdirSync(dir, { withFileTypes: true })) {
    if (name.name.includes("pga_datagolf") || name.name.includes("player_map")) {
      console.log("found", join(dir, name.name));
    }
    if (name.isDirectory() && !name.name.startsWith(".") && name.name !== "node_modules") {
      try {
        walk(join(dir, name.name), depth + 1);
      } catch {}
    }
  }
}
walk(join(WEB, ".."));
