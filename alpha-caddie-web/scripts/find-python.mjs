/**
 * Resolve a Python 3+ launcher so we never run the aggregate script with the wrong interpreter
 * (e.g. `python` -> Rscript), which surfaces as R parse errors on `.py` files.
 */
import { spawnSync } from "child_process";

/**
 * @param {{ cwd?: string, env?: NodeJS.ProcessEnv }} [opts]
 * @returns {{ cmd: string; argsPrefix: string[] } | null}
 */
export function findPythonArgsPrefix(opts = {}) {
  const { cwd, env } = opts;
  const common = { cwd, env, encoding: "utf-8" };
  const attempts =
    process.platform === "win32"
      ? [
          ["py", ["-3"]],
          ["python3", []],
          ["python", []],
        ]
      : [
          ["python3", []],
          ["python", []],
        ];
  for (const [cmd, prefix] of attempts) {
    const probe = spawnSync(
      cmd,
      [...prefix, "-c", "import sys; v=sys.version_info; sys.exit(0 if v.major>=3 else 1)"],
      { ...common, stdio: ["ignore", "pipe", "pipe"] }
    );
    if (probe.error && probe.error.code === "ENOENT") continue;
    if ((probe.status ?? 1) !== 0) continue;
    const err = String(probe.stderr || "");
    if (/Rscript|R version|GNU R/i.test(err)) continue;
    return { cmd, argsPrefix: prefix };
  }
  return null;
}
