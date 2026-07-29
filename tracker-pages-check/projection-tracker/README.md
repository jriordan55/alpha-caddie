# Alpha Caddie · Projection Tracker

Static tracker UI for round projection vs actual, EV backtests, and live best bets.

## URLs

- **GitHub Pages (public):** https://jriordan55.github.io/alpha-caddie/projection-tracker/  
  (also https://jriordan55.github.io/alpha-caddie/ redirects here)
- **Render app:** `/projection-tracker/` on the live Alpha Caddie service
- **Local:** `npm run projection-tracker` → http://localhost:5173/projection-tracker/

Pages rebuilds automatically when `main` updates tracker files, projections, or tracker CSVs
(`push:live` / data pushes), after the DraftKings round-props workflow, and on a 2-hour schedule.

## One-time setup

In the GitHub repo: **Settings → Pages → Build and deployment → Source: GitHub Actions**.
Then run Actions → **Projection Tracker Pages** → **Run workflow** once.
