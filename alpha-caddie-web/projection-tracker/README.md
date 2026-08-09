# Both-side edge tracker

Lean projection tracker: only markets where OVER and UNDER both print historically.

## What it shows
- **Market report card** — OVER and UNDER must both be ROI+ (≥40 bets/side)
- **Live picks** — raw hierarchical μ (weather + tee wave already baked); policy gap only — **no chrono/loo bias**
- **Graded bets** — historical flat $100 under gap policies on **raw model μ** (bias locked to `none`)

## Commands
```bash
npm run bake:both-side-roi          # gap sweep on raw μ (GOLF_BOTH_SIDE_BIAS_SWEEP=1 for old chrono/loo)
npm run apply:hierarchical-mu       # live board = hierarchical + weather
npm run apply:both-side-bet-signals # YES/NO on DK props (no live_bias subtract)
npm run projection-tracker          # serves UI (GOLF_SKIP_TRACKER_REFRESH=1 to skip re-bake)
```

Open `http://localhost:5173/projection-tracker/`
