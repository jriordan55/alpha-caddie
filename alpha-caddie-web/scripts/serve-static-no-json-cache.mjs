#!/usr/bin/env node
/**
 * Production static server for Alpha Caddie — replaces `serve` so *.json is never CDN/browser cached stale.
 * HTML/CSS/JS stay cache-friendly; projections.json / live-in-play.json always get no-store headers.
 *
 * POST /api/admin/refresh-dk-round-props — remote DK scrape (npm run push:dk-round-projections on Render).
 * Requires GOLF_ADMIN_REFRESH_SECRET; send Authorization: Bearer <secret>.
 *
 * POST /api/betsperts-rh-ou — Rabbit Hole OU market stats for Round projections filters.
 * Body JSON: { timeframe, region, ctype, course }. Uses BETSPERTS_SESSION_KEY / betsperts.local.json.
 */
import http from "http";
import fs from "fs";
import path from "path";
import { spawn } from "child_process";
import { fileURLToPath } from "url";
import { resolveGolfModelDir } from "./resolve-golf-model-dir.mjs";
import { fetchRhOuStatsBundle, resolveBetspertsSessionKey } from "./betsperts-rh-ou.mjs";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(__dirname, "..");
const REPO_ROOT = resolveGolfModelDir(root);
let dkRefreshBusy = false;
let rhOuBusy = false;
const rhOuCache = new Map();
const RH_OU_CACHE_TTL_MS = 5 * 60 * 1000;

const MIME = {
  ".html": "text/html; charset=utf-8",
  ".htm": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json; charset=utf-8",
  ".csv": "text/csv; charset=utf-8",
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

function adminSecretOk(req) {
  const secret = String(process.env.GOLF_ADMIN_REFRESH_SECRET || "").trim();
  if (!secret) return false;
  const auth = String(req.headers.authorization || "").trim();
  if (auth === `Bearer ${secret}`) return true;
  return String(req.headers["x-admin-secret"] || "").trim() === secret;
}

function handleAdminRefreshDkRound(req, res) {
  if (req.method !== "POST") {
    res.statusCode = 405;
    res.setHeader("Allow", "POST");
    res.end("Method not allowed");
    return true;
  }
  if (!String(process.env.GOLF_ADMIN_REFRESH_SECRET || "").trim()) {
    res.statusCode = 503;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.end(JSON.stringify({ ok: false, error: "GOLF_ADMIN_REFRESH_SECRET not configured on server" }));
    return true;
  }
  if (!adminSecretOk(req)) {
    res.statusCode = 401;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.end(JSON.stringify({ ok: false, error: "unauthorized" }));
    return true;
  }
  if (dkRefreshBusy) {
    res.statusCode = 409;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.end(JSON.stringify({ ok: false, error: "refresh already in progress" }));
    return true;
  }
  const script = path.join(__dirname, "update-dk-round-projections.mjs");
  if (!fs.existsSync(script)) {
    res.statusCode = 500;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.end(JSON.stringify({ ok: false, error: "update-dk-round-projections.mjs missing" }));
    return true;
  }
  dkRefreshBusy = true;
  res.statusCode = 202;
  res.setHeader("Content-Type", "application/json; charset=utf-8");
  res.end(JSON.stringify({ ok: true, message: "DraftKings round props refresh started" }));
  const bg = spawn(process.execPath, [script], {
    cwd: root,
    stdio: ["ignore", "inherit", "inherit"],
    env: { ...process.env, GOLF_MODEL_DIR: REPO_ROOT, GOLF_SKIP_PROPS_CSV: "1" },
  });
  bg.on("error", (err) => {
    dkRefreshBusy = false;
    console.warn("[alpha-caddie-web] DK round refresh spawn error:", err.message);
  });
  bg.on("exit", (code) => {
    dkRefreshBusy = false;
    if (code !== 0 && code != null) {
      console.warn("[alpha-caddie-web] update-dk-round-projections exited", code);
    } else {
      console.log("[alpha-caddie-web] DraftKings round props refresh finished.");
    }
  });
  return true;
}

function readJsonBody(req, limit = 1e6) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    let size = 0;
    req.on("data", (c) => {
      size += c.length;
      if (size > limit) {
        reject(new Error("body too large"));
        req.destroy();
        return;
      }
      chunks.push(c);
    });
    req.on("end", () => {
      const raw = Buffer.concat(chunks).toString("utf8");
      if (!raw.trim()) {
        resolve({});
        return;
      }
      try {
        resolve(JSON.parse(raw));
      } catch (e) {
        reject(e);
      }
    });
    req.on("error", reject);
  });
}

async function handleBetspertsRhOu(req, res) {
  if (req.method !== "POST" && req.method !== "GET") {
    res.statusCode = 405;
    res.setHeader("Allow", "GET, POST");
    res.end("Method not allowed");
    return true;
  }
  if (!resolveBetspertsSessionKey()) {
    res.statusCode = 503;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.end(JSON.stringify({ ok: false, error: "BETSPERTS_SESSION_KEY not configured" }));
    return true;
  }
  let filters = { timeframe: "all" };
  try {
    if (req.method === "POST") {
      const body = await readJsonBody(req);
      filters = {
        timeframe: body.timeframe || body.time || "all",
        region: body.region || "",
        ctype: body.ctype || body.courseType || "",
        course: body.course || "",
      };
    } else {
      const u = new URL(req.url || "/", "http://127.0.0.1");
      filters = {
        timeframe: u.searchParams.get("timeframe") || "all",
        region: u.searchParams.get("region") || "",
        ctype: u.searchParams.get("ctype") || "",
        course: u.searchParams.get("course") || "",
      };
    }
  } catch {
    res.statusCode = 400;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.end(JSON.stringify({ ok: false, error: "invalid json body" }));
    return true;
  }

  const cacheKey = JSON.stringify(filters);
  const hit = rhOuCache.get(cacheKey);
  if (hit && Date.now() - hit.at < RH_OU_CACHE_TTL_MS) {
    res.statusCode = 200;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.setHeader("Cache-Control", "no-store");
    res.end(JSON.stringify(hit.payload));
    return true;
  }
  if (rhOuBusy) {
    res.statusCode = 409;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.end(JSON.stringify({ ok: false, error: "rabbit hole request already in progress" }));
    return true;
  }
  rhOuBusy = true;
  try {
    const bundle = await fetchRhOuStatsBundle(filters);
    const payload = bundle.ok
      ? {
          ok: true,
          generatedAt: bundle.generatedAt,
          filters: bundle.filters,
          n: bundle.n,
          players: bundle.players,
        }
      : { ok: false, error: bundle.error || "fetch_failed" };
    if (bundle.ok) rhOuCache.set(cacheKey, { at: Date.now(), payload });
    res.statusCode = bundle.ok ? 200 : 502;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.setHeader("Cache-Control", "no-store");
    res.end(JSON.stringify(payload));
  } catch (e) {
    res.statusCode = 500;
    res.setHeader("Content-Type", "application/json; charset=utf-8");
    res.end(JSON.stringify({ ok: false, error: String(e?.message || e) }));
  } finally {
    rhOuBusy = false;
  }
  return true;
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

  if (pathname === "/api/admin/refresh-dk-round-props") {
    handleAdminRefreshDkRound(req, res);
    return;
  }

  if (pathname === "/api/betsperts-rh-ou") {
    void handleBetspertsRhOu(req, res);
    return;
  }

  if (pathname === "/" || pathname === "") pathname = "/index.html";
  // Directory indexes (Projection Tracker lives at /projection-tracker/)
  if (pathname.endsWith("/")) pathname = `${pathname}index.html`;
  else if (pathname === "/projection-tracker") pathname = "/projection-tracker/index.html";

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
    if (ext === ".json" || ext === ".csv") {
      // Headshots map + slim season avgs are safe to cache briefly (big UX win on Round projections).
      const base = path.basename(filePath).toLowerCase();
      const cacheableSidecar =
        base === "player_headshots.json" || /^ou-season-avgs-\d{4}\.json$/.test(base);
      if (cacheableSidecar) {
        res.setHeader("Cache-Control", "public, max-age=300, stale-while-revalidate=86400");
      } else {
        res.setHeader("Cache-Control", "no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0");
        res.setHeader("Pragma", "no-cache");
        res.setHeader("Expires", "0");
        res.setHeader("Surrogate-Control", "no-store");
      }
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
