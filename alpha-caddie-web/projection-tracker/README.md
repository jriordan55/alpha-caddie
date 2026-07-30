# Alpha Caddie · Projection Tracker

Static tracker UI for round projection vs actual, EV backtests, and live best bets.

**Model μ (current):** skill-first origins — course / venue baseline + high keep of μ_SG (GIR/FW from course rates + skill; total score uses venue hist, with Detroit North/South club pooling + layout STP shift when exact-side hist is thin).

## URLs

- **GitHub Pages (public):** https://jriordan55.github.io/alpha-caddie/projection-tracker/  
  (also https://jriordan55.github.io/alpha-caddie/ redirects here)
- **Render app:** `/projection-tracker/` on the live Alpha Caddie service
- **Local:** `npm run projection-tracker` → http://localhost:5173/projection-tracker/

Pages rebuilds automatically when `main` updates tracker files, projections, or tracker CSVs
(`push:live` / data pushes), after the DraftKings round-props workflow, and on a 2-hour schedule.

## Refresh

```bash
# Live week rows from current projections.json
npm run export:round-projection-vs-actual
npm run promote:round-projection-vs-actual

# Full walk-forward μ rebuild (Bet log / OOS) + matchups + Pages assemble
npm run projection-tracker:refresh
npm run matchup-tracker:refresh
npm run projection-tracker:pages
```

## Sibling: Matchup tracker

Round matchups + 3-balls (DraftKings / FanDuel / BetMGM):  
https://jriordan55.github.io/alpha-caddie/matchup-tracker/
## One-time setup

In the GitHub repo: **Settings → Pages → Build and deployment → Source: GitHub Actions**.
Then run Actions → **Projection Tracker Pages** → **Run workflow** once.
