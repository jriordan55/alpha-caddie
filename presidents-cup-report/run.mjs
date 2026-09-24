#!/usr/bin/env node
/** @deprecated Use serve.mjs — launches local dashboard at http://localhost:3847 */
import { spawn } from "child_process";
import { join, dirname } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const child = spawn(process.execPath, [join(__dirname, "serve.mjs"), ...args], {
  stdio: "inherit",
  env: process.env,
});
child.on("exit", (c) => process.exit(c ?? 0));
