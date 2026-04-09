# Monday 6:00 AM pipeline

One scheduled job refreshes **everything** needed for shot-level modeling, DataGolf historical data, projections, and the Shiny app.

## What runs (in order)

| Step | What | Output / effect |
|------|------|------------------|
| 1 | `live_update_all.R` → `live_update_all_2026()` or `live_update_all()` | **DataGolf:** `historical_rounds_all.csv`, `historical_outrights_outcomes.csv`, `historical_matchups_outcomes.csv`. **Shots:** `append_latest_tournament_shots()` via `helper.R` → `data/all_shots_2021_2026.csv` (requires **pgatouR**) |
| 2 | `scripts/build_pga_datagolf_tournament_map.R` | `data/pga_datagolf_tournament_map.csv` |
| 3 | `scripts/build_pga_datagolf_player_map.R` | `data/pga_datagolf_player_map.csv` |
| 4 | `round_projections.R` (**pass 1**) | DataGolf field-updates → `golf_selected_course.txt`, `simulated_round_static.rds`, `data/current_course_holes.rds`, … |
| 5 | `scripts/fit_shot_transition_model.R` | `data/shot_transition_model.rds` for **SELECTED_COURSE** (from pass 1) |
| 6 | `round_projections.R` (**pass 2**) | Same outputs using **fresh** shot RDS |
| 7 | `rsconnect::deployApp` | Publishes **app.R** + project to shinyapps.io (bundles data + RDS) |

**Live DataGolf feeds** (skill ratings, in-play, pre-tournament, betting tools, etc.) are read **when the app runs** in the browser or on the server — they are not stored by this job. This job refreshes **historical** CSVs and **rebuilds** model artifacts.

## Requirements

- **`DATAGOLF_API_KEY`** — set in the environment for the scheduled task (or in user `.Renviron`). The task runs as your Windows user; put the key where R can see it.
- **`pgatouR`** — for shot append (inside step 1). If missing or `helper.R` errors, that part is skipped; refit still uses existing `all_shots` rows.
- **`rsconnect`** — for deploy (step 7). Not required for local-only runs.

## Register the Windows task (Monday 6:00 AM)

From PowerShell in the repo:

```powershell
cd C:\Users\student\Documents\golfModel
powershell -ExecutionPolicy Bypass -File .\scripts\register_monday_republish_task.ps1
```

Task name: **`GolfModel Monday Republish`**. Logs: **`logs/monday_republish.log`**.

## Skip deploy (e.g. local testing)

Set before the job runs:

```powershell
$env:SKIP_SHINY_DEPLOY = "1"
```

Or add that user environment variable in Windows.

## Manual run

```powershell
cd C:\Users\student\Documents\golfModel
powershell -ExecutionPolicy Bypass -File .\scripts\run_monday_republish.ps1
```

Or from R:

```r
setwd("C:/Users/student/Documents/golfModel")
Sys.setenv(DATAGOLF_API_KEY = "your_key")
source("scripts/monday_republish.R")  # only works if you fix shebang path; prefer Rscript:
```

```bash
Rscript scripts/monday_republish.R
```

## DataGolf years refreshed

`live_update_all.R` calls `datagolf_years_to_update()`: by default it does a **full DataGolf pull** for **this calendar year and the previous year** (so e.g. 2026 data stays complete every Monday during 2026, and still gets a refresh alongside 2027 in 2027).

To force extra seasons (e.g. re-pull only 2026), set:

`GOLF_DATAGOLF_YEARS=2026` (comma-separated for multiple years). Years are clipped to the rolling window used in `historical_rounds_all.csv` (last five seasons).
