#!/usr/bin/env node
/**
 * Refresh DraftKings round props on the **deployed** site — not a local git commit/push.
 *
 *   npm run push:dk-round-projections
 *
 * **Render / live Node server** (updates projections.json on the server; no git):
 *   GOLF_ALPHA_CADDIE_URL=https://your-app.onrender.com
 *   GOLF_ADMIN_REFRESH_SECRET=<same secret set on Render>
 *
 * **GitHub-hosted static site** (cloud workflow commits + pushes to origin):
 *   gh auth login   # once
 *   npm run push:dk-round-projections
 *   Or: GitHub → Actions → "DraftKings round projections" → Run workflow
 *
 * Optional GitHub repo secrets for the workflow: DK_LEAGUE_URL, DK_SITE_SEGMENT, DK_LEAGUE_ID
 */
import { spawnSync } from "child_process";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const REPO_ROOT = resolve(WEB_ROOT, "..");

function gitRemoteRepo() {
  const r = spawnSync("git", ["-C", REPO_ROOT, "remote", "get-url", "origin"], { encoding: "utf8" });
  if (r.status !== 0) return "";
  const raw = String(r.stdout || "").trim();
  const m = raw.match(/github\.com[:/](.+?)(?:\.git)?$/i);
  return m ? m[1] : "";
}

async function triggerRenderRefresh() {
  const base = String(process.env.GOLF_ALPHA_CADDIE_URL || process.env.RENDER_EXTERNAL_URL || "").trim().replace(/\/$/, "");
  const secret = String(process.env.GOLF_ADMIN_REFRESH_SECRET || "").trim();
  if (!base) return false;
  if (!secret) {
    console.error(
      "[push:dk-round-projections] Set GOLF_ADMIN_REFRESH_SECRET (and the same value on Render) to authorize the refresh endpoint.",
    );
    process.exit(1);
  }
  const url = `${base}/api/admin/refresh-dk-round-props`;
  console.log(`[push:dk-round-projections] POST ${url} …`);
  const res = await fetch(url, {
    method: "POST",
    headers: {
      Authorization: `Bearer ${secret}`,
      Accept: "application/json",
    },
  });
  const text = await res.text();
  if (!res.ok) {
    console.error(`[push:dk-round-projections] Server returned ${res.status}: ${text}`);
    process.exit(1);
  }
  console.log(text || "OK");
  console.log("[push:dk-round-projections] Server is refreshing DK props; reload the Round projections tab in ~1–2 min.");
  return true;
}

function hasGhCli() {
  const r = spawnSync(process.platform === "win32" ? "where" : "which", ["gh"], {
    encoding: "utf8",
    shell: process.platform === "win32",
  });
  return r.status === 0 && String(r.stdout || "").trim().length > 0;
}

function triggerGitHubWorkflow() {
  if (!hasGhCli()) return false;
  const repo = gitRemoteRepo();
  const args = ["workflow", "run", "dk-round-projections.yml"];
  if (repo) args.push("-R", repo);
  console.log(`[push:dk-round-projections] gh ${args.join(" ")} …`);
  const r = spawnSync("gh", args, { cwd: REPO_ROOT, stdio: "inherit", encoding: "utf8" });
  if (r.status !== 0) {
    console.error("[push:dk-round-projections] gh workflow run failed. Use GitHub → Actions → Run workflow manually.");
    process.exit(r.status ?? 1);
  }
  console.log(
    "[push:dk-round-projections] Workflow started on GitHub. When it finishes, the site redeploys from the new commit.",
  );
  return true;
}

async function main() {
  if (await triggerRenderRefresh()) return;
  if (String(process.env.GOLF_PUSH_DK_ROUND_VIA_GITHUB || "").trim() !== "0") {
    if (triggerGitHubWorkflow()) return;
  }
  console.error(`
[push:dk-round-projections] No remote target configured.

Render (live server, no git):
  GOLF_ALPHA_CADDIE_URL=https://your-app.onrender.com
  GOLF_ADMIN_REFRESH_SECRET=<secret>   # also set on Render dashboard

GitHub Actions (commit + push from the cloud):
  gh auth login
  npm run push:dk-round-projections

Or open GitHub → Actions → "DraftKings round projections" → Run workflow
`);
  process.exit(1);
}

main().catch((e) => {
  console.error(e);
  process.exit(1);
});
