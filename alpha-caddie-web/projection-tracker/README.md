# Both-side edge tracker

Lean projection tracker: only markets where OVER and UNDER both print historically.

## What it shows
- **Market report card** — OVER and UNDER must both be ROI+ (≥40 bets/side)
- **Live picks** — only both-side+ markets, μ corrected by walk-forward chrono/loo bias, policy gap
- **Graded bets** — historical DK flat $100 under those policies

## Passing markets (current bake)
| Market | Gap | Bias | Over ROI | Under ROI | PnL |
|--------|-----|------|----------|-----------|-----|
| Fairways hit | 0.6/0.85 | loo | +10.4% | +6.4% | +$1,266 |
| Total score | 1.5 | chrono | +1.3% | +12.9% | +$2,750 |
| Birdies | 1/0.5 | none | +7.0% | +15.8% | +$1,331 |
| Pars | 0 | none | +2.7% | +5.3% | +$1,369 |
| Bogeys | 1/0.65 | chrono | +11.6% | +22.5% | +$3,473 |
| GIR | 0 | loo | +0.6% | +2.4% | +$966 |

**All six markets both-side+.** Pars μ = course anchor + rolling good SG:OTT × poor SG:PUTT (“par-machine”), blended with DG and lightly pulled toward `18 − Birdies − Bogeys`.

Combined (passing only): **+$11,155** on **1,893** bets.

Honest walk-forward OOS (Kelly, $10k): **−$219** — see `data/walkforward_oos_roi.json` (retune pending; both-side bake cleared first).

## Commands
```bash
npm run patch:birdie-fairway-mu   # recounting μ as-of each cutoff
npm run bake:both-side-roi
npm run report:walkforward-oos-roi
npm run apply:dg-methodology
npm run apply:both-side-bias
npm run projection-tracker        # serves UI (set GOLF_SKIP_TRACKER_REFRESH=1 to keep patched CSV)
```

Open `http://localhost:5173/projection-tracker/`
