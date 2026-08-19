# Alpha Caddie · Matchup Tracker

Identical sibling of the Projection tracker for **round matchups** and **3-balls**.

**Books:** DraftKings · FanDuel · BetMGM only (DataGolf historical + live odds).

## URLs

- **GitHub Pages:** https://jriordan55.github.io/alpha-caddie/matchup-tracker/
- **Local:** `npm run matchup-tracker` → http://localhost:5173/matchup-tracker/

## Refresh (keeps data current)

```bash
# Pull latest DG historical matchups (DK/FD/MGM) + rebuild walk-forward backtest
npm run matchup-tracker:refresh

# Also runs automatically on npm run push:live / refresh:live
```

## Model

- Round matchups: Gaussian win prob on walk-forward μ_SG (σ_diff = 2.85)
- 3-balls: softmax on μ_SG (T = 2.05), same as main app
