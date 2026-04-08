/**
 * Locate Rscript for Windows installs where R is not on PATH.
 * Order: RSCRIPT_PATH, R_HOME bin, then Program Files\R\R-x.x.x\bin (newest version), else "Rscript".
 */
import fs from "fs";
import path from "path";

function exists(p) {
  try {
    return p && fs.existsSync(p);
  } catch {
    return false;
  }
}

export function findRscriptSync() {
  const fromEnv = process.env.RSCRIPT_PATH || process.env.RSCRIPT;
  if (fromEnv && exists(fromEnv)) return fromEnv;

  const rhome = process.env.R_HOME;
  if (rhome) {
    const win = path.join(rhome, "bin", "Rscript.exe");
    if (exists(win)) return win;
    const unix = path.join(rhome, "bin", "Rscript");
    if (exists(unix)) return unix;
  }

  if (process.platform === "win32") {
    const scan = (root) => {
      const rRoot = path.join(root, "R");
      if (!exists(rRoot)) return null;
      let dirs;
      try {
        dirs = fs.readdirSync(rRoot).filter((d) => /^R-[\d.]+/i.test(d));
      } catch {
        return null;
      }
      dirs.sort((a, b) => b.localeCompare(a, undefined, { numeric: true }));
      for (const d of dirs) {
        const exe = path.join(rRoot, d, "bin", "Rscript.exe");
        if (exists(exe)) return exe;
      }
      return null;
    };
    const pf = process.env.ProgramFiles || "C:\\Program Files";
    const pf86 = process.env["ProgramFiles(x86)"] || "C:\\Program Files (x86)";
    const hit = scan(pf) || scan(pf86);
    if (hit) return hit;
  }

  return "Rscript";
}
