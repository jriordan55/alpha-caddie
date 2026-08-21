#!/usr/bin/env node
/**
 * Bake direct book odds for the paper trade book (server-side fetch — no browser CORS).
 * Runs at end of fetch:book-odds / push:live.
 *
 *   npm run bake:paper-book
 */
import { existsSync, readFileSync, writeFileSync } from "fs";
import { dirname, join } from "path";
import { fileURLToPath, pathToFileURL } from "url";
import { fetchDirectBookCards } from "../paper-book/book-api-fetch.mjs";
import { PAPER_BOOKS, liveTargetRound } from "../paper-book/live-book-options-core.mjs";

const __dirname = dirname(fileURLToPath(import.meta.url));
const WEB_ROOT = join(__dirname, "..");
const OUT_PATH = join(WEB_ROOT, "paper-book", "paper-book-lines.json");

/**
 * @param {object} projections — freshly merged projections.json payload
 */
export async function bakePaperBookLines(projections) {
  if (!projections || typeof projections !== "object") {
    throw new Error("bakePaperBookLines requires projections object");
  }

  const round = liveTargetRound(projections);
  const roundLabel =
    String(projections?.meta?.display_round_label || projections?.display_round_label || "").trim() ||
    `R${round}`;
  const eventName = String(projections?.event_name || projections?.meta?.event_name || "").trim();
  const updatedAt = new Date().toISOString();

  /** @type {Record<string, object>} */
  const books = {};

  for (const book of PAPER_BOOKS) {
    const built = await fetchDirectBookCards(projections, book.id, { force: true });
    books[book.id] = {
      cards: built.cards,
      fetchError: built.fetchError || "",
      count: built.cards.length,
      fetchedAt: built.fetchedAt || updatedAt,
    };
    console.log(
      `[bake:paper-book] ${book.label}: ${built.cards.length} props${built.fetchError ? ` — ${built.fetchError}` : ""}`,
    );
  }

  const payload = {
    updated_at: updatedAt,
    event_name: eventName,
    round,
    round_label: roundLabel,
    books,
  };

  writeFileSync(OUT_PATH, `${JSON.stringify(payload, null, 2)}\n`, "utf8");
  console.log("[bake:paper-book] Wrote", OUT_PATH);

  const websiteDir = join(WEB_ROOT, "..", "website", "public", "paper-book");
  if (existsSync(websiteDir)) {
    const websiteOut = join(websiteDir, "paper-book-lines.json");
    writeFileSync(websiteOut, `${JSON.stringify(payload, null, 2)}\n`, "utf8");
    console.log("[bake:paper-book] Wrote", websiteOut);
  }

  return payload;
}

async function main() {
  const projPath = join(WEB_ROOT, "projections.json");
  if (!existsSync(projPath)) {
    console.error("[bake:paper-book] missing projections.json — run fetch:dg first");
    process.exit(1);
  }
  const projections = JSON.parse(readFileSync(projPath, "utf8"));
  await bakePaperBookLines(projections);
}

const isMain = process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href;

if (isMain) {
  main().catch((e) => {
    console.error(e);
    process.exit(1);
  });
}
