#!/usr/bin/env node
/**
 * Fast static server for paper-book only (no refresh pipeline on startup).
 *
 *   npm run paper-book
 *   → http://localhost:5174/paper-book/
 */
import { spawn } from "child_process";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PORT = process.env.PORT || "5174";
const url = `http://localhost:${PORT}/paper-book/`;

console.log(`[paper-book] Serving ${WEB} on port ${PORT}`);
console.log(`[paper-book] Open ${url}`);
console.log("[paper-book] Odds from paper-book/paper-book-lines.json (npm run push:live or bake:paper-book)");

const child = spawn(process.execPath, [join(WEB, "scripts", "serve-static-no-json-cache.mjs")], {
  cwd: WEB,
  stdio: "inherit",
  env: { ...process.env, PORT: String(PORT) },
});

child.on("exit", (code) => process.exit(code ?? 0));
