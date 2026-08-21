#!/usr/bin/env node
/**
 * Static server for paper-book (golf props paper trading).
 *
 *   npm run paper-book
 *   → http://localhost:5173/paper-book/
 */
import { spawn } from "child_process";
import { dirname, join } from "path";
import { fileURLToPath } from "url";

const WEB = join(dirname(fileURLToPath(import.meta.url)), "..");
const PORT = process.env.PORT || "5173";
const url = `http://localhost:${PORT}/paper-book/`;

console.log(`[paper-book] Serving ${WEB}`);
console.log(`[paper-book] Open ${url}`);
console.log("[paper-book] Odds: baked in paper-book-lines.json via npm run push:live or npm run bake:paper-book");

const child = spawn("npx", ["--yes", "serve", ".", "-p", PORT], {
  cwd: WEB,
  stdio: "inherit",
  shell: true,
});

child.on("exit", (code) => process.exit(code ?? 0));
