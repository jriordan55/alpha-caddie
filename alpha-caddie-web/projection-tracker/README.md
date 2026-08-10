# Model vs market tracker

Local UI: `http://localhost:5173/projection-tracker/`

## What it shows

- **Market report card** — OVER / UNDER / combined ROI for each O/U market at the recommended gap vs all sportsbooks
- **Analytics** — cumulative PnL, bankroll, ROI on graded projection bets
- **Live picks** — current-week model μ vs live book lines past the policy gap
- **Graded projection bets** — historical flat $100 results under those gaps

## Refresh

```bash
npm run bake:both-side-roi          # gap sweep on raw μ → both_side_roi.json + both_side_bets.json
npm run apply:both-side-bet-signals # YES/NO stamps on DK props
```

Or via `npm run push:live` / `refresh:live` (bake runs after hierarchical μ).
