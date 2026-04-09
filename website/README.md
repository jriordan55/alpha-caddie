# AlphaCaddie (static web)

This is a **vanilla TypeScript + Vite** front end that mirrors the Shiny `app.R` layout: tabs, dark theme, Model O/U table, lightweight props EV, and optional DataGolf fetches through a **dev-only proxy** (no R/Python runtime in the browser).

## Run locally

```bash
cd website
npm install
npm run dev
```

**DataGolf key (dev proxy)** — pick one:

1. **JSON file (simple on Windows):** copy `datagolf.local.example.json` to **`datagolf.local.json`** in this folder and set `"apiKey": "your_key"`. That file is gitignored.
2. **`.env`:** copy `.env.example` to `.env` and set `DATAGOLF_API_KEY=...`.

Open the URL Vite prints (usually `http://localhost:5173`).

## How this ties to `round_projections.R`

Projections for the static site are the same object as Shiny’s **`simulated_round_table`**, written to **`simulated_round_static.rds`** at the end of [`round_projections.R`](../round_projections.R) (raw mode with `GOLF_RAW_PROJECTIONS=1` uses DataGolf skill-ratings + optional fantasy defaults; full mode uses historical SG, course fit, shot MC, tournament sim for outrights, etc.).

After you run the pipeline, export JSON for the website:

```bash
Rscript scripts/export_projections_for_website.R
```

(from the **repo root**, or set `GOLF_MODEL_DIR` to that directory). That writes **`website/public/data/projections.json`**.

Then `npm run build` (or `npm run dev`) will serve those rows on Model O/U / props. The **`alpha-caddie-web`** folder is standalone HTML/JS with bundled demo data and optional `projections.json` override (no npm).

## Projections data

- **`public/data/projections.json`** is produced by the script above, or you can author it by hand. Shape:

```json
{
  "event_name": "Tournament",
  "round": 1,
  "players": [
    {
      "player_name": "Last, First",
      "dg_id": 123,
      "country": "USA",
      "round": 1,
      "total_score": 70.1,
      "round_sd": 2.7,
      "birdies": 4.0,
      "pars": 10.5,
      "bogeys": 2.8,
      "gir": 11.5,
      "fairways": 9.0,
      "win": 0.05,
      "top_5": 0.2,
      "eagles": 0.2,
      "doubles": 0.4
    }
  ],
  "props": [
    { "player_name": "Last, First", "line": 70.5, "over_odds": -110, "under_odds": -110, "market": "Total Score" }
  ]
}
```

- If `projections.json` is missing, the Vite app may fall back to **`projections.example.json`** (see that project). **`alpha-caddie-web`** uses a small built-in mock when fetch fails.
- Export from R however you like, e.g. `jsonlite::write_json()` on the same tibble you save as `simulated_round_static.rds`.

## Production hosting

- **`npm run build`** outputs static files to **`dist/`**. Deploy `dist/` to any static host (Netlify, Cloudflare Pages, S3, etc.).
- Browsers cannot call DataGolf directly with a secret key. For production API access, add a **small edge proxy** (Cloudflare Worker, Netlify Function, etc.) that appends `key=` server-side, or keep the site **JSON-only** and refresh `projections.json` on a schedule.

## Shiny parity

- **Model O/U**: Poisson / Normal pricing helpers ported to `src/lib/odds.ts`.
- **+EV / Hole Hangout / full DT tables**: not fully ported; extend with more JSON endpoints or client logic as needed.
- Copy **`../www/logos`** into **`public/logos`** if you want local sportsbook logos (optional).
