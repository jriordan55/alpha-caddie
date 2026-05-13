#!/usr/bin/env node
/**
 * Production static server for Alpha Caddie — replaces `serve` so *.json is never CDN/browser cached stale.
 * HTML/CSS/JS stay cache-friendly; projections.json / live-in-play.json always get no-store headers.
 */
import http from "http";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(__dirname, "..");

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".htm": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".svg": "image/svg+xml",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".gif": "image/gif",
  ".webp": "image/webp",
  ".ico": "image/x-icon",
  ".woff2": "font/woff2",
  ".woff": "font/woff",
  ".ttf": "font/ttf",
  ".map": "application/json; charset=utf-8",
  ".txt": "text/plain; charset=utf-8",
};

const port = parseInt(String(process.env.PORT || "5173"), 10);

function resolvedPathForUrlPathname(pathname) {
  const decoded = decodeURIComponent(pathname);
  const safe = path.normalize(decoded).replace(/^(\.\.(\/|\\|$))+/, "");
  const stripped = safe.replace(/^[\\/]+/, "");
  const abs = path.resolve(root, stripped);
  if (!abs.startsWith(root)) return null;
  return abs;
}

const server = http.createServer((req, res) => {
  let pathname = "/";
  try {
    pathname = new URL(req.url || "/", "http://127.0.0.1").pathname;
  } catch {
    res.statusCode = 400;
    res.end("Bad request");
    return;
  }
  if (pathname === "/" || pathname === "") pathname = "/index.html";

  const filePath = resolvedPathForUrlPathname(pathname);
  if (!filePath) {
    res.statusCode = 403;
    res.end("Forbidden");
    return;
  }

  fs.readFile(filePath, (err, data) => {
    if (err) {
      res.statusCode = 404;
      res.setHeader("Content-Type", "text/plain; charset=utf-8");
      res.end("Not found");
      return;
    }
    const ext = path.extname(filePath).toLowerCase();
    res.setHeader("Content-Type", MIME[ext] || "application/octet-stream");
    if (ext === ".json") {
      res.setHeader("Cache-Control", "no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0");
      res.setHeader("Pragma", "no-cache");
      res.setHeader("Expires", "0");
      res.setHeader("Surrogate-Control", "no-store");
    } else if (ext === ".html" || ext === ".htm") {
      /* Pick up new ?v= on app.js/styles without relying on CDN defaults (mobile Safari caches aggressively). */
      res.setHeader("Cache-Control", "max-age=0, must-revalidate");
    }
    res.end(data);
  });
});

server.listen(port, "0.0.0.0", () => {
  console.log(`[alpha-caddie-web] listening on 0.0.0.0:${port} (JSON: Cache-Control no-store)`);
});
