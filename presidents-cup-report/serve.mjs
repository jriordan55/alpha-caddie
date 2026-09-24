#!/usr/bin/env node
/**
 * Local Presidents Cup model dashboard (DataGolf-style, tab per market).
 *
 *   node presidents-cup-report/serve.mjs
 *   node presidents-cup-report/serve.mjs --fast
 */
import { createServer } from "http";
import { readFileSync, existsSync, statSync } from "fs";
import { join, dirname, extname } from "path";
import { fileURLToPath } from "url";
import { buildPresidentsCupModel } from "./lib/build-model.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const PUBLIC = join(__dirname, "public");
const OUT = join(__dirname, "output");
const PORT = Number(process.env.PC_PORT) || 3847;

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".webp": "image/webp",
  ".svg": "image/svg+xml",
  ".ico": "image/x-icon",
};

function send(res, status, body, type = "text/plain") {
  res.writeHead(status, { "Content-Type": type, "Cache-Control": "no-cache" });
  res.end(body);
}

async function main() {
  const fast = process.argv.includes("--fast");
  const skipDk = process.argv.includes("--skip-dk");
  if (fast) {
    process.env.PC_SIMS = process.env.PC_SIMS || "8000";
  }
  if (skipDk) {
    console.log("[presidents-cup] --skip-dk: model-only prices (no DraftKings scrape)");
  }

  await buildPresidentsCupModel({
    outDir: OUT,
    nSims: fast ? 8000 : 20000,
    skipDk,
  });

  const server = createServer((req, res) => {
    const url = (req.url || "/").split("?")[0];

    if (url === "/api/model.json") {
      const p = join(OUT, "model.json");
      if (!existsSync(p)) return send(res, 404, '{"error":"not built"}', MIME[".json"]);
      return send(res, 200, readFileSync(p), MIME[".json"]);
    }

    let file = url === "/" ? "/index.html" : url;
    const fp = join(PUBLIC, file.replace(/^\//, ""));
    if (!fp.startsWith(PUBLIC) || !existsSync(fp) || statSync(fp).isDirectory()) {
      return send(res, 404, "Not found");
    }
    send(res, 200, readFileSync(fp), MIME[extname(fp)] || "application/octet-stream");
  });

  server.listen(PORT, () => {
    console.log(`\n[presidents-cup] Dashboard → http://localhost:${PORT}\n`);
  });
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
