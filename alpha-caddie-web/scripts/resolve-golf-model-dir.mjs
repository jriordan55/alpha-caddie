/**
 * Repo root that holds `data/` (historical CSVs, etc.). Used when spawning fetch scripts.
 *
 * Priority:
 * 1. GOLF_MODEL_DIR env (set in Render if layouts differ)
 * 2. Parent when folder is `alpha-caddie-web` and `../data` exists (monorepo)
 * 3. `webRoot` when `./data` exists (standalone package)
 * 4. Parent when `../data` exists
 * 5. Parent if basename is `alpha-caddie-web` (monorepo before first CSV mkdir)
 * 6. Else `webRoot` (standalone — writes under package root)
 */
import fs from "fs";
import path from "path";

export function resolveGolfModelDir(webRoot) {
  const raw = process.env.GOLF_MODEL_DIR?.trim();
  if (raw) return path.resolve(raw);

  const wr = path.resolve(webRoot);
  const parent = path.resolve(wr, "..");
  const base = path.basename(wr).toLowerCase();

  const has = (p) => {
    try {
      return fs.existsSync(p);
    } catch {
      return false;
    }
  };

  if (base === "alpha-caddie-web" && has(path.join(parent, "data"))) return parent;
  if (has(path.join(wr, "data"))) return wr;
  if (has(path.join(parent, "data"))) return parent;
  if (base === "alpha-caddie-web") return parent;
  return wr;
}
