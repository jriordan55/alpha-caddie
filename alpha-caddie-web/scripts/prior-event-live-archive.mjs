/**
 * Archive completed DataGolf preds/in-play weeks when projections roll to the next event.
 * Keeps R4 (etc.) actuals available for round_projection_vs_actual backfill after live-in-play.json
 * is cleared to a pre-event skeleton for the new week.
 */
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath } from "url";
import { eventsLikelySame } from "./dg-events-align.mjs";

const WEB_ROOT = join(dirname(fileURLToPath(import.meta.url)), "..");
export const PRIOR_EVENT_LIVE_ARCHIVE_PATH = join(WEB_ROOT, "data", "prior_event_live_archive.json");

function num(v, fb = NaN) {
  const n = Number(v);
  return Number.isFinite(n) ? n : fb;
}

function foldKey(eventName) {
  return String(eventName || "")
    .trim()
    .toLowerCase()
    .replace(/\s+/g, " ");
}

/** Count posted R1–R4 gross scores in an in-play bundle. */
export function countPostedRounds(bundle) {
  /** @type {Record<string, number>} */
  const out = { "1": 0, "2": 0, "3": 0, "4": 0 };
  for (const row of Array.isArray(bundle?.data) ? bundle.data : []) {
    for (let rnd = 1; rnd <= 4; rnd++) {
      if (num(row?.[`R${rnd}`], 0) > 0) out[String(rnd)]++;
    }
  }
  return out;
}

export function loadPriorEventLiveArchive(path = PRIOR_EVENT_LIVE_ARCHIVE_PATH) {
  if (!existsSync(path)) return { updated_at: null, events: {} };
  try {
    const j = JSON.parse(readFileSync(path, "utf8"));
    if (!j || typeof j !== "object") return { updated_at: null, events: {} };
    return {
      updated_at: j.updated_at || null,
      events: j.events && typeof j.events === "object" ? j.events : {},
    };
  } catch {
    return { updated_at: null, events: {} };
  }
}

/**
 * Persist a completed (or still-serving) prior-week live bundle for later vs-actual backfill.
 * Prefer the archive with more posted round slots when both exist.
 */
export function archivePriorEventLiveBundle(bundle, opts = {}) {
  if (!bundle || typeof bundle !== "object") return null;
  const eventName = String(
    bundle.info?.event_name || bundle.field_updates?.event_name || opts.eventName || "",
  ).trim();
  if (!eventName) return null;
  const posted = countPostedRounds(bundle);
  const postedN = Object.values(posted).reduce((a, b) => a + b, 0);
  if (postedN <= 0 && !opts.force) return null;

  const path = opts.path || PRIOR_EVENT_LIVE_ARCHIVE_PATH;
  const arch = loadPriorEventLiveArchive(path);
  const key = foldKey(eventName);
  const prev = arch.events[key];
  const prevPosted = prev?.posted_rounds || {};
  const prevN = Object.values(prevPosted).reduce((a, b) => a + Number(b || 0), 0);
  if (prev && prevN > postedN && !opts.force) {
    return { skipped: true, reason: "archive_richer", eventName, posted: prevPosted };
  }

  arch.events[key] = {
    event_name: eventName,
    archived_at: new Date().toISOString(),
    posted_rounds: posted,
    bundle,
  };
  arch.updated_at = new Date().toISOString();
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, `${JSON.stringify(arch, null, 2)}\n`, "utf8");
  return { ok: true, eventName, posted, path };
}

/** Find archived live bundle whose event matches `eventName`. */
export function findArchivedLiveBundleForEvent(eventName, path = PRIOR_EVENT_LIVE_ARCHIVE_PATH) {
  const want = String(eventName || "").trim();
  if (!want) return null;
  const arch = loadPriorEventLiveArchive(path);
  for (const rec of Object.values(arch.events || {})) {
    if (!rec?.bundle) continue;
    if (eventsLikelySame(String(rec.event_name || ""), want)) return rec.bundle;
  }
  return null;
}
