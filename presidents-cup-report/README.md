# Presidents Cup 2026 — Match-Play Model Dashboard

Standalone Monte Carlo model for the **Presidents Cup at Medinah**, using the [DataGolf Ryder Cup methodology](https://datagolf.com/ryder-cup-blog/). Local web dashboard with **one tab per market**, model vs DraftKings implied probabilities, and edge.

## Run

From repo root:

```bash
npm run presidents-cup:report
```

Opens **http://localhost:3847** — simulates the cup, scrapes DK lines, caches official logos/headshots locally, and serves the dashboard.

Fast dev (8k sims):

```bash
npm run presidents-cup:report:fast
```

Skip DraftKings scrape (model-only):

```bash
node presidents-cup-report/serve.mjs --skip-dk
```

## Dashboard

- **DataGolf-style** cup win bar (USA red / International green)
- **Tabs:** Cup Winner, Top Points Scorer, each session, Final Score, All Edges
- Each row: Model %, **DK implied %** (from American odds), odds, edge, EV
- Player **headshots** and team badges served from `public/assets/` (cached from [presidentscup.com](https://www.presidentscup.com/))

## Env

| Variable | Default | Purpose |
|----------|---------|---------|
| `PC_SIMS` | `20000` | Monte Carlo iterations |
| `PC_SKIP_DK` | — | Set `1` when calling `build-model` directly to skip DK |
| `PC_PORT` | `3847` | Local server port |

## Output

- `output/model.json` — API payload for the dashboard
- `public/assets/` — cached badges + headshots (auto-downloaded)
