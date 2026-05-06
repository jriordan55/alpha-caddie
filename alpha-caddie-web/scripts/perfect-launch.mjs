#!/usr/bin/env node
/**
 * One-shot: refresh DataGolf data on disk, start the local server, open the app in your browser.
 *
 *   npm run perfect
 *
 * Requires DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json (same as fetch:dg).
 * Port: PORT env or 5173.
 *
 * Runs `draftkings-ou-props.mjs` (same as `npm run fetch:dk-ou`) before `fetch:book-odds` so DraftKings round
 * props are probed against `projections.json` first; book-odds then merges DK + books into JSON (Playwright twice).
 * Skip the standalone probe: PERFECT_SKIP_FETCH_DK_OU=1
 *
 * DK URL: set DK_LEAGUE_URL (e.g. Truist round page) or rely on slug from projections.event_name.
 */
import { spawn, spawnSync } from "child_process";
import net from "net";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = path.resolve(__dirname, "..");
const REPO_ROOT = path.resolve(WEB_ROOT, "..");
const env = { ...process.env, GOLF_MODEL_DIR: process.env.GOLF_MODEL_DIR?.trim() || REPO_ROOT };

function runNodeScript(rel, label) {
  const script = path.join(WEB_ROOT, "scripts", rel);
  console.log(`\n[perfect] ${label}…\n`);
  const r = spawnSync(process.execPath, [script], {
    cwd: WEB_ROOT,
    stdio: "inherit",
    env,
  });
  if (r.status !== 0) {
    console.error(`[perfect] ${label} failed (exit ${r.status ?? "?"}) — fix errors above (API key, network, Playwright for DK).`);
    process.exit(r.status ?? 1);
  }
}

function portNumber() {
  const p = Number(process.env.PORT || 5173);
  return Number.isFinite(p) && p > 0 && p < 65536 ? Math.floor(p) : 5173;
}

function tryConnect(port, host, ms) {
  return new Promise((resolve) => {
    const sock = net.connect({ port, host }, () => {
      clearTimeout(t);
      sock.end();
      resolve(true);
    });
    const t = setTimeout(() => {
      sock.destroy();
      resolve(false);
    }, ms);
    sock.on("error", () => {
      clearTimeout(t);
      resolve(false);
    });
  });
}

async function waitForServer(port, maxWaitMs) {
  const host = "127.0.0.1";
  const deadline = Date.now() + maxWaitMs;
  while (Date.now() < deadline) {
    if (await tryConnect(port, host, 1200)) return true;
    await new Promise((r) => setTimeout(r, 250));
  }
  console.warn(`[perfect] Server did not accept TCP on ${host}:${port} within ${maxWaitMs}ms — open the URL manually.`);
  return false;
}

function openBrowser(url) {
  const opts = { stdio: "ignore", detached: true, shell: false };
  try {
    if (process.platform === "win32") {
      spawn("cmd", ["/c", "start", "", url], { ...opts, shell: false });
    } else if (process.platform === "darwin") {
      spawn("open", [url], opts);
    } else {
      spawn("xdg-open", [url], opts);
    }
    console.log(`[perfect] Opened browser → ${url}`);
  } catch (e) {
    console.warn("[perfect] Could not launch browser:", e.message);
    console.log(`[perfect] Open manually: ${url}`);
  }
}

runNodeScript("fetch-datagolf.mjs", "Full projections (fetch:dg)");
runNodeScript("fetch-live-in-play.mjs", "Live / in-play JSON (fetch:in-play)");
if (String(process.env.PERFECT_SKIP_FETCH_DK_OU || "").trim() !== "1") {
  runNodeScript("draftkings-ou-props.mjs", "DraftKings round O/U probe (npm run fetch:dk-ou)");
}
runNodeScript("fetch-book-odds-into-projections.mjs", "Latest sportsbook odds + DK merge (fetch:book-odds)");

const port = portNumber();
const url = `http://127.0.0.1:${port}/`;

const serve = path.join(WEB_ROOT, "scripts", "serve-with-refresh.mjs");
console.log(`\n[perfect] Starting server on port ${port} (Ctrl+C to stop)…\n`);

const child = spawn(process.execPath, [serve], {
  cwd: WEB_ROOT,
  stdio: "inherit",
  env: { ...env, PORT: String(port) },
});

let opened = false;
void (async () => {
  if (await waitForServer(port, 120_000)) {
    if (opened) return;
    opened = true;
    openBrowser(url);
  }
})();

child.on("exit", (code, signal) => {
  if (signal) process.kill(process.pid, signal);
  process.exit(code ?? 0);
});
child.on("error", (err) => {
  console.error("[perfect] Failed to start server:", err.message);
  process.exit(1);
});
