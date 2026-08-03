param(
  [switch] $SkipPush,
  [switch] $NoFullHistory,
  [switch] $LiveWeekOnly,
  [switch] $ArtifactsOnly,
  [switch] $PullFirst,
  [string] $CommitMessage = ""
)

$ErrorActionPreference = "Stop"

function Resolve-GolfRepoRoot([string] $ScriptsDir) {
  $gitOut = & git -C $ScriptsDir rev-parse --show-toplevel 2>$null
  if ($LASTEXITCODE -eq 0 -and $gitOut) {
    $t = "$gitOut".Trim()
    if ($t -ne "" -and (Test-Path (Join-Path $t ".git"))) {
      return $t
    }
  }
  return [string](Resolve-Path (Join-Path $ScriptsDir "..")).Path
}

function Invoke-NpmCli([Parameter(ValueFromRemainingArguments = $true)][string[]] $NpmArgs) {
  $npmCmd = Get-Command npm.cmd -ErrorAction SilentlyContinue
  if (-not $npmCmd) {
    $npmCmd = Get-Command npm -ErrorAction Stop
  }
  $exe = if ($npmCmd.Path) { $npmCmd.Path } else { $npmCmd.Source }
  & $exe @NpmArgs
}

function Run-Npm([string] $Label, [Parameter(ValueFromRemainingArguments = $true)][string[]] $NpmArgs) {
  Write-Host $Label
  $global:LASTEXITCODE = 0
  Invoke-NpmCli @NpmArgs
  $code = $LASTEXITCODE
  if ($null -eq $code) { $code = 0 }
  # Windows sometimes reports killed/crashed children as negative NTSTATUS (-1, etc.).
  if ($code -ne 0) {
    throw "$Label failed with exit code $code"
  }
}
#
# Course fit tab - generated artifacts this pipeline must publish:
#   * course-table.json - built from data/course_table.csv (course mapping for Course Fit radar, similarity,
#     fit table, and static live-prop difficulty prior). Built by npm run build:course-table after each fetch:dg
#     and again in push:all after fetch:dg; mirrored to website/public/data/course-table.json.
#   * projections.json - field + meta.course_used (fetch:dg); outrights win/top5/10/20/cut after fetch:book-odds merge into DATA.outrights
#     (same Scratch API as datagolf.com/betting-tool-finish IMPLIED %). fetch:finish-tool re-merges that feed so the standalone script stays exercised;
#     set GOLF_FINISH_TOOL_PLAYWRIGHT=1 (+ optional DATAGOLF_PLAYWRIGHT_STORAGE_STATE) to capture browser JSON instead of direct API for missing markets.
#   * approach_skill_ytd.json - Predicted shot distance bins (fetch:dg preds/approach-skill); optional approach_skill_l12.json fallback if present.
#   * embedded-player-round-history.js (+ CSV / player_round_history.json / player-history shards) - Hole Hangout
#     hole-level priors and **Historical Trends** (update:rounds + build:history; live round rows merged when live-in-play.json exists).
#     Default push:all sets GOLF_HISTORICAL_ROUNDS_FULL_HISTORY=1; **update:rounds** (after fetch:in-play) refreshes
#     historical_rounds_all.csv + player_round_history / embed / shards once. fetch:dg skips that merge (faster).
#     fetch:in-play writes live-in-play.json (live_round_actuals_by_dg from preds/live-tournament-stats per round + R* gross)
#     before update:rounds so build-player-history merges the live week into exported history (Aronimink / current PGA rounds).
#     Course Fit uses course-table.json for layout vs field SG.
#
# Hole Hangout - hole pars per hole for the active course/event:
#   * fetch:dg calls preds/live-hole-stats and writes hole_pars / hole_pars_source (live_hole_stats when DG serves it).
#   * fetch:in-play -> live-in-play.json (bundled live_hole_stats); merge:live-hole-pars-into-projections (after fetch:book-odds)
#     re-aligns projections.json from that bundle so push:all publishes the same per-hole table as the live feed even when
#     fetch:book-odds runs inline fetch:dg and refreshes the JSON.
#   * course_holes.json - bundled overrides / gaps (committed); course_holes.local.json is gitignored for secrets.
#   * hole_pars_from_shots.json - fallback map from build:history (build-player-shots-web.mjs) after rounds CSV refresh.
#   * player-history/by-dg/*.json + manifest.json - hole-level score rows (hole_data.csv join in build-player-history.mjs);
#     Hole Hangout fetches shards at player-history/by-dg/{dg_id}.json. Staged below with other history artifacts.
#
# Alpha Caddie browser shell (props trends dates, cache-busted index.html, etc.): alpha-caddie-web/app.js +
#   index.html are listed in $artifacts so `npm run push:artifacts-only` still publishes UI edits alongside
#   data. Default `npm run push:all` also runs `git add -A`, which stages any other web changes.
#
# Matchup Analysis Tool stays fresh when these commands succeed:
#   fetch:dg  -> projections.players (SG pillars + merged preds/live-tournament-stats driving when DG serves it),
#               projections.matchups (betting-tools/matchups), approach_skill_ytd.json (Course Fit shot bins)
#   fetch:book-odds -> matchup/outright odds + DraftKings + PrizePicks + Sleeper + Underdog + FanDuel + Kalshi + Caesars round O/U props
#     merged into projections.json; appends alpha-caddie-web/data/dk_round_projection_audit.csv
#     (+ pp_/sl_/ud_ round audit CSVs when those books return lines).
#   export:round-projection-vs-actual - alpha-caddie-web/data/round_projection_vs_actual.csv (model vs actual per player×round).
#   fetch:in-play -> live-in-play.json (live_round_actuals_by_dg) -> build-player-history merges into player_round_history + shards;
#     browser also merges live-in-play on Historical Trends open (app.js ensureLiveTournamentHistoryMerged).
#     fetch-live-in-play.mjs carries forward dropped R1-R3 gross columns when the field advances.
#   merge:live-hole-pars-into-projections runs AFTER fetch:book-odds + fetch:finish-tool so an inline fetch:dg inside
#   book-odds does not leave projections without live_hole_stats hole_pars (tee-adjacent UI + consistency with Hole Hangout).
#   merge:live-round-meta-into-projections bumps display_round from live-in-play + reapplies μ_SG prior-round
#   strokes (blend vs historical CSV); projections keep the full tournament field for Historical Trends. Before update:rounds.
# Mirrors below copy projections + live + approach_skill *.json into website/public/data/ so both apps ship the same JSON.
#
# Round-projections / +EV: refresh:live finishes weather/unified, then rebuilds
# round_projection_vs_actual (prior walkforward + current week) + matchup backtest +
# walk-forward OOS report so projection-tracker always matches the published model.
# Market rating: fetch:dg writes pga_tour_market_benchmarks; refresh:live re-runs refresh:market-benchmarks after the
# post-live historical CSV merge so 2025-2026 μ/σ stay current in published projections.json.

$scriptsDir = $PSScriptRoot
$repoRoot = Resolve-GolfRepoRoot $scriptsDir
$webRoot = Join-Path $repoRoot "alpha-caddie-web"

if (-not (Test-Path (Join-Path $webRoot "package.json"))) {
  throw "Missing alpha-caddie-web/package.json. Run from the project checkout."
}

Set-Location $webRoot

# OOS winner: day/form + skill-36 (no soft book μ align — that hurt Birdies ROI).
# Skill-first total score: full μ_SG keep; tiny player-course residual (Detroit club pool was flattening).
$env:GOLF_FLAT_VENUE_PLAYER_SCORE = "0"
$env:GOLF_FLAT_VENUE_MAX_PLAYER_SCORE_WEIGHT = "0.06"
$env:GOLF_SCORE_SKILL_KEEP = "1"
$env:GOLF_SCORE_PLAYER_COURSE_MAX_W = "0.06"
$env:GOLF_COURSE_PRIOR_ROUND_DIFFICULTY = "1"
$env:GOLF_WITHIN_EVENT_FORM_CARRY = "0.1"
$env:GOLF_WITHIN_EVENT_FORM_CAP = "0.75"
$env:GOLF_UNIFIED_BOUNCE_BACK_K = "0.12"
$env:GOLF_WF_SKILL_MAX_ROUNDS = "36"
$env:GOLF_MARKET_BOOK_CALIBRATION = "0"
$env:GOLF_SKIP_MARKET_BOOK_CALIBRATION = "1"
# Round projections tab: require a fresh DraftKings scrape (no stale / empty publish).
$env:GOLF_REQUIRE_DK_OU = "1"
# DK / Caesars / FanDuel Nash-style APIs often block headless Playwright on desktop - headed Chromium.
# GitHub Actions has no interactive display — keep whatever headless flags CI set (usually 1).
$isCi = ($env:GITHUB_ACTIONS -eq "true") -or ($env:CI -eq "true")
if (-not $isCi -and ($IsWindows -or ($env:OS -match "Windows") -or $IsMacOS)) {
  $env:DK_HEADLESS = "0"
  $env:CZR_HEADLESS = "0"
  $env:FD_HEADLESS = "0"
}

if ($LiveWeekOnly) {
  # Lean mid-tournament publish: projections + book odds + prior-round Trends + tracker.
  # Live Open-Meteo tee-time weather bake always runs (archive backfill is what is skipped).
  # Not a full CSV/history/ROI rebuild (use push:all for that).
  Remove-Item Env:\GOLF_REFRESH_LIVE_FULL_REBUILD -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_HISTORICAL_ROUNDS_FULL_HISTORY -ErrorAction SilentlyContinue
  $env:GOLF_REFRESH_LIVE_SKIP_CSV_MERGE = "1"
  Remove-Item Env:\GOLF_REFRESH_LIVE_SKIP_POST_CSV_MERGE -ErrorAction SilentlyContinue
  $env:GOLF_REFRESH_LIVE_SKIP_HISTORY_REBUILD = "1"
  $env:GOLF_SKIP_ROUND_WEATHER_BACKFILL = "1"
  # Never allow lean mode to skip the live Open-Meteo bake.
  Remove-Item Env:\GOLF_ALLOW_MISSING_WEATHER_COORDS -ErrorAction SilentlyContinue
  $env:GOLF_REFRESH_LIVE_SKIP_FINISH_TOOL = "1"
  $env:GOLF_SKIP_MARKET_BOOK_CALIBRATION = "1"
  $env:GOLF_SKIP_BACKTEST_ODDS_MODEL_ROI = "1"
  # Incremental tracker updates on push:live (current-week O/U + matchups from last recorded date).
  # Full prior O/U rebuild: set GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS=1 or run projection-tracker:refresh.
  $env:GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS = "0"
  $env:GOLF_SKIP_ROUND_PROJECTION_VS_ACTUAL = "0"
  $env:GOLF_REQUIRE_TRACKER_REFRESH = "1"
  Remove-Item Env:\GOLF_MATCHUP_BACKTEST_SINCE -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_OU_BACKTEST_SINCE -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_ODDS_SINCE -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_MATCHUP_ODDS_UPDATE -ErrorAction SilentlyContinue
  $env:GOLF_SKIP_DK_ROUND_AUDIT_CSV = "0"
  $env:GOLF_SKIP_PP_ROUND_AUDIT_CSV = "0"
  $env:GOLF_SKIP_SL_ROUND_AUDIT_CSV = "0"
  $env:GOLF_SKIP_UD_ROUND_AUDIT_CSV = "0"
  $env:GOLF_SKIP_FD_ROUND_AUDIT_CSV = "0"
  $env:GOLF_SKIP_CZR_ROUND_AUDIT_CSV = "0"
  $env:GOLF_SKIP_KL_ROUND_AUDIT_CSV = "0"
  # Round O/U books: DK + PP + SL + UD + FanDuel + Kalshi + Caesars (never skip on push:live).
  Remove-Item Env:\GOLF_SKIP_DK_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_PP_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_SL_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_UD_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_FD_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_KL_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_CZR_OU -ErrorAction SilentlyContinue
  $env:GOLF_SKIP_SL_OU = "0"
  $env:GOLF_SKIP_UD_OU = "0"
  $env:GOLF_SKIP_PP_OU = "0"
  $env:GOLF_SKIP_FD_OU = "0"
  $env:GOLF_SKIP_KL_OU = "0"
  $env:GOLF_SKIP_CZR_OU = "0"
  # Soft: DK/Playwright flakiness and late optional steps must not kill the publish.
  $env:GOLF_LIVE_WEEK_SOFT = "1"
  $env:GOLF_REQUIRE_DK_OU = "0"
  $env:GOLF_REQUIRE_PP_OU = "0"
  $env:GOLF_REQUIRE_SL_OU = "0"
  $env:GOLF_REQUIRE_UD_OU = "0"
  $env:GOLF_REQUIRE_FD_OU = "0"
  $env:GOLF_REQUIRE_KL_OU = "0"
  $env:GOLF_REQUIRE_CZR_OU = "0"
  $env:GOLF_SKIP_DK_OU_VALIDATE = "1"
  $env:GOLF_FAIL_ON_PAR_MISMATCH = "0"
  $env:GOLF_LIVE_VALIDATE_SOFT = "1"
  $env:GOLF_UNIFIED_TEE_WAVE_W = "0.30"
  $env:GOLF_FIELD_DAY_COUNTING_LIFT_FRAC = "0"
  $env:GOLF_WITHIN_EVENT_COUNTING_BLEND = "0"
  Write-Host 'LiveWeekOnly (lean): projections + odds (DK/PP/SL/UD/FD/Kalshi/Caesars) + Trends patch + tracker.'
  Write-Host 'Live Open-Meteo weather bake always runs on push:live (tee-time forecast into scores).'
  Write-Host 'Both trackers incremental from last recorded date: O/U projection-tracker (DK/PP/SL/UD/FD/CZR/KL) + matchup-tracker.'
  Write-Host 'Skill-first score (keep 1.0) + Detroit club hist pool when North/South exact hist is thin.'
  Write-Host 'Skipped: full CSV merge, weather archive backfill, finish-tool, book-cal fit, odds ROI backtest, full prior O/U rebuild.'
  Write-Host 'LiveWeekOnly markets: Birdies = birdies+eagles (or better), Bogeys = bogeys+doubles (or worse), same as DK/PP/SL/UD/FD/CZR.'
} elseif (-not $NoFullHistory) {
  $env:GOLF_HISTORICAL_ROUNDS_FULL_HISTORY = "1"
  $env:GOLF_SKIP_HISTORY_ON_FETCH_DG = "1"
  Remove-Item Env:\GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_HISTORICAL_ROUNDS_FETCH_ALL_YEARS -ErrorAction SilentlyContinue
  Write-Host 'Historical Trends: one full CSV merge + build via update:rounds after in-play. fetch:dg skips duplicate history work.'
} else {
  $env:GOLF_SKIP_HISTORY_ON_FETCH_DG = "1"
  Remove-Item Env:\GOLF_HISTORICAL_ROUNDS_FULL_HISTORY -ErrorAction SilentlyContinue
  Write-Host 'NoFullHistory: update:rounds uses default year rules (not FULL_HISTORY=1).'
}

# When fetch output matches HEAD exactly, still bump app.js?v= so deploys pick up fresh HTML/JS.
function Bump-AlphaCaddieAppJsCache([string] $AlphaCaddieWebRoot) {
  $idx = Join-Path $AlphaCaddieWebRoot "index.html"
  if (-not (Test-Path $idx)) { return $false }
  $enc = New-Object System.Text.UTF8Encoding $false
  $c = [System.IO.File]::ReadAllText($idx, $enc)
  if ($c -notmatch 'app\.js\?v=(\d+)') { return $false }
  $n = [int]$Matches[1]
  $n2 = $n + 1
  $c2 = [regex]::Replace($c, 'app\.js\?v=\d+', "app.js?v=$n2")
  [System.IO.File]::WriteAllText($idx, $c2, $enc)
  Write-Host "Bumped alpha-caddie-web/index.html app.js cache version to v=$n2 (so push:all always has a deployable delta when data JSON matched HEAD)."
  return $true
}

function Promote-RoundProjectionVsActualCsv([string] $AlphaCaddieWebRoot) {
  $names = @(
    "round_projection_vs_actual.csv",
    "round_projection_vs_actual_summary.csv",
    "round_projection_vs_actual.xlsx"
  )
  foreach ($name in $names) {
    $csv = Join-Path $AlphaCaddieWebRoot "data\$name"
    $staged = Join-Path $AlphaCaddieWebRoot "data\$name.new"
    if (-not (Test-Path $staged)) { continue }
    Write-Host "Promoting data\$name.new -> data\$name (close Excel if this fails) ..."
    try {
      Copy-Item -Path $staged -Destination $csv -Force -ErrorAction Stop
      Remove-Item -Path $staged -Force -ErrorAction Stop
      Write-Host "Promoted data\$name"
    } catch {
      throw "Could not update data\$name - close Excel/editor and re-run push:live. Fresh data is in data\$name.new"
    }
  }
}

# During rebase, --theirs is the local refresh commit being replayed (not origin/main).
$script:RebaseDataConflictPaths = @(
  "alpha-caddie-web/data/dk_round_projection_audit.csv",
  "alpha-caddie-web/projections.json",
  "website/public/data/projections.json",
  "alpha-caddie-web/live-in-play.json",
  "website/public/data/live-in-play.json"
)

function Resolve-RebaseDataConflicts([string] $Root) {
  $conflicted = @(git -C $Root diff --name-only --diff-filter=U 2>$null | Where-Object { $_ -ne "" })
  if ($conflicted.Count -eq 0) { return $false }
  $touched = $false
  foreach ($p in $script:RebaseDataConflictPaths) {
    if ($conflicted -contains $p) {
      git -C $Root checkout --theirs -- $p
      if ($LASTEXITCODE -ne 0) { continue }
      git -C $Root add -- $p
      Write-Host "Rebase: kept local refresh for $p"
      $touched = $true
    }
  }
  return $touched
}

function Invoke-GitPullRebasePublish([string] $Root, [string] $Branch) {
  Write-Host "Pulling latest origin/$Branch with rebase ..."
  git -C $Root pull --rebase origin $Branch
  while ($LASTEXITCODE -ne 0) {
    if (Resolve-RebaseDataConflicts $Root) {
      git -C $Root -c core.editor=true rebase --continue
      continue
    }
    throw "git pull --rebase failed (resolve conflicts manually, then push)."
  }
}

function Invoke-GitPushPublish([string] $Root, [string] $Branch, [switch] $SyncFirst) {
  if ($SyncFirst) {
    Invoke-GitPullRebasePublish $Root $Branch
  }
  Write-Host "Pushing origin $Branch ..."
  git -C $Root push origin $Branch
  if ($LASTEXITCODE -ne 0) {
    Write-Host "Push rejected (remote has newer commits - often DraftKings GitHub Action). Syncing and retrying once ..."
    Invoke-GitPullRebasePublish $Root $Branch
    git -C $Root push origin $Branch
    if ($LASTEXITCODE -ne 0) {
      throw "git push failed with exit code $LASTEXITCODE"
    }
  }
}

if ($LiveWeekOnly) {
  Run-Npm "Live-week refresh (projections + odds + weather + Trends patch + tracker) ..." run refresh:live
  Promote-RoundProjectionVsActualCsv $webRoot
} else {
  Run-Npm "Running fetch:dg ..." run fetch:dg
  Run-Npm "Building course-table.json (course mapping) ..." run build:course-table
  Run-Npm "Running fetch:in-play ..." run fetch:in-play
  Run-Npm "Refreshing current-event PGA rounds from pgatouR ..." run refresh:pgatour-event
  Remove-Item Env:\GOLF_SKIP_DK_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_PP_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_SL_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_UD_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_FD_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_KL_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\GOLF_SKIP_CZR_OU -ErrorAction SilentlyContinue
  Remove-Item Env:\PERFECT_SKIP_FETCH_DK_OU -ErrorAction SilentlyContinue
  $env:GOLF_SKIP_FD_OU = "0"
  $env:GOLF_SKIP_KL_OU = "0"
  $env:GOLF_SKIP_CZR_OU = "0"
  $env:GOLF_DEFER_DK_ROUND_AUDIT_UNTIL_REPAIR = "1"
  # fetch:book-odds pulls DK round props (Birdies/Total Score/GIR/etc.) via Playwright - no separate fetch:dk-ou (would duplicate Chromium).
  Run-Npm "Running fetch:book-odds (matchups, outrights, DK + PP + SL + UD + FD + Kalshi + Caesars round O/U props) ..." run fetch:book-odds
  Run-Npm 'Running fetch:finish-tool - outrights, same Scratch feed as DG Finish Position; runs after book-odds ...' run fetch:finish-tool
  Run-Npm "Merging live_hole_stats into projections (after book odds; preserves pars if book-odds ran inline fetch:dg) ..." run merge:live-hole-pars-into-projections
  Run-Npm "Bundled course_holes.json -> projections when live pars missing/wrong ..." run sync:bundled-hole-pars
  Run-Npm "Fail fast if hole pars still generic (new venue needs course_holes.json) ..." run check:hole-pars
  Run-Npm "Merging tournament round + prior-round course difficulty from live-in-play -> projections ..." run merge:live-round-meta-into-projections
  Run-Npm "field-updates tee times (ET) -> projections.json dg_teetime_local ..." run merge:field-teetimes-into-projections
  Run-Npm "Open-Meteo weather for upcoming display_round -> projections.json (bake:weather) ..." run bake:weather
  Run-Npm "Venue player/course history + skill blend (repair:projection-course-basis) ..." run repair:projection-course-basis
  Run-Npm "Prior-round form from live-in-play (after venue repair) ..." run merge:within-event-form
  Run-Npm "Unified projection factors (course fit, tee wave - after venue repair) ..." run apply:unified-factors
  Run-Npm "Live scores into projections for +EV in-play pricing ..." run merge:live-in-play-scratch-into-projections
  Run-Npm "Tournament MC outright probs (bake:outright-sim) ..." run bake:outright-sim
  Run-Npm "Reconcile counts + venue field markets (book cal deferred until after vs-actual export) ..." run reconcile:projection-counts
  Run-Npm "DK round audit CSV with post-repair model lines ..." run export:dk-round-audit-csv
  Run-Npm "Running update:rounds (historical CSV + Historical Trends: player_round_history / embed / shards / shots web) ..." run update:rounds
  Run-Npm "Patching current-event rounds (pgatouR + live GIR/FW into history shards) ..." run patch:current-event-history
  $env:GOLF_REBUILD_PRIOR_BACKTEST_PROJECTIONS = "1"
  Run-Npm "Writing round_projection_vs_actual.csv (walkforward backtest + current week) ..." run export:round-projection-vs-actual
  Run-Npm "Fit DK book-alignment on prior events only (no outcome peek) ..." run fit:market-book-calibration
  Run-Npm "Walk-forward honest OOS ROI report ..." run report:walkforward-oos-roi
  Run-Npm "Apply book-alignment to live projections.json ..." run apply:market-book-calibration
  Run-Npm "Validate projections after book calibration ..." run validate:projections
  Run-Npm "Odds.csv model ROI backtest (walkforward venue-history projections) ..." run backtest:odds-model-roi
  Promote-RoundProjectionVsActualCsv $webRoot
}

$webDataDir = Join-Path $repoRoot "website/public/data"
if (-not (Test-Path $webDataDir)) {
  New-Item -ItemType Directory -Path $webDataDir -Force | Out-Null
}
$liveSrc = Join-Path $webRoot "live-in-play.json"
$liveDest = Join-Path $webDataDir "live-in-play.json"
if (Test-Path $liveSrc) {
  Copy-Item -Path $liveSrc -Destination $liveDest -Force
  Write-Host "Mirrored live-in-play.json -> website/public/data/live-in-play.json"
}

$projSrc = Join-Path $webRoot "projections.json"
$projDest = Join-Path $webDataDir "projections.json"
if (Test-Path $projSrc) {
  Copy-Item -Path $projSrc -Destination $projDest -Force
  Write-Host "Mirrored projections.json -> website/public/data/projections.json"
}

$courseTableSrc = Join-Path $webRoot "course-table.json"
$courseTableDest = Join-Path $webDataDir "course-table.json"
if (Test-Path $courseTableSrc) {
  Copy-Item -Path $courseTableSrc -Destination $courseTableDest -Force
  Write-Host "Mirrored course-table.json -> website/public/data/course-table.json"
}

$asSrc = Join-Path $webRoot "approach_skill_ytd.json"
$asDest = Join-Path $webDataDir "approach_skill_ytd.json"
if (Test-Path $asSrc) {
  Copy-Item -Path $asSrc -Destination $asDest -Force
  Write-Host "Mirrored approach_skill_ytd.json -> website/public/data/approach_skill_ytd.json"
}

$asL12Src = Join-Path $webRoot "approach_skill_l12.json"
$asL12Dest = Join-Path $webDataDir "approach_skill_l12.json"
if (Test-Path $asL12Src) {
  Copy-Item -Path $asL12Src -Destination $asL12Dest -Force
  Write-Host "Mirrored approach_skill_l12.json -> website/public/data/approach_skill_l12.json"
}

$parlayCorrSrc = Join-Path $webRoot "data/parlay_correlations.json"
$parlayCorrDest = Join-Path $webDataDir "parlay_correlations.json"
if (Test-Path $parlayCorrSrc) {
  Copy-Item -Path $parlayCorrSrc -Destination $parlayCorrDest -Force
  Write-Host "Mirrored parlay_correlations.json -> website/public/data/parlay_correlations.json"
}

Set-Location $repoRoot

$artifacts = @(
  "alpha-caddie-web/app.js",
  "alpha-caddie-web/index.html",
  "alpha-caddie-web/styles.css",
  "alpha-caddie-web/package.json",
  "alpha-caddie-web/projections.json",
  "alpha-caddie-web/live-in-play.json",
  "alpha-caddie-web/approach_skill_ytd.json",
  "alpha-caddie-web/approach_skill_l12.json",
  "alpha-caddie-web/course-table.json",
  "alpha-caddie-web/data/course_table.csv",
  "alpha-caddie-web/data/dk_round_projection_audit.csv",
  "alpha-caddie-web/data/pp_round_projection_audit.csv",
  "alpha-caddie-web/data/sl_round_projection_audit.csv",
  "alpha-caddie-web/data/ud_round_projection_audit.csv",
  "alpha-caddie-web/data/fd_round_projection_audit.csv",
  "alpha-caddie-web/data/czr_round_projection_audit.csv",
  "alpha-caddie-web/data/kl_round_projection_audit.csv",
  "alpha-caddie-web/data/pin_sheets/pin_sheet_active.json",
  "alpha-caddie-web/data/pin_sheets/pin_sheet.png",
  "alpha-caddie-web/data/pin_locations/index.json",
  "alpha-caddie-web/data/pin_locations/sheets",
  "data/pin_locations/index.json",
  "data/pin_locations/sheets",
  "alpha-caddie-web/data/round_projection_vs_actual.csv",
  "alpha-caddie-web/data/round_projection_vs_actual_summary.csv",
  "alpha-caddie-web/data/round_projection_vs_actual.xlsx",
  "alpha-caddie-web/data/matchup_backtest_detail.csv",
  "alpha-caddie-web/data/matchup_backtest_summary.csv",
  "alpha-caddie-web/data/market_book_calibration.json",
  "alpha-caddie-web/data/walkforward_oos_roi.json",
  "alpha-caddie-web/data/parlay_correlations.json",
  "alpha-caddie-web/data/odds_model_roi_summary.csv",
  "alpha-caddie-web/data/odds_model_roi_detail.csv",
  "alpha-caddie-web/data/odds_model_roi_lines.csv",
  "alpha-caddie-web/projection-tracker",
  "alpha-caddie-web/matchup-tracker",
  "alpha-caddie-web/data/pgatour_event_rounds.json",
  "alpha-caddie-web/data/historical_round_weather.json",
  "alpha-caddie-web/data/course_coordinates_cache.json",
  "alpha-caddie-web/hole_pars_from_shots.json",
  "alpha-caddie-web/player_shots_web.json",
  "alpha-caddie-web/player-history",
  "website/public/data/projections.json",
  "website/public/data/course-table.json",
  "website/public/data/live-in-play.json",
  "website/public/data/approach_skill_ytd.json",
  "website/public/data/approach_skill_l12.json",
  "website/public/data/parlay_correlations.json",
  "data/historical_rounds_all.csv",
  "alpha-caddie-web/data/historical_rounds_all.csv",
  "alpha-caddie-web/player_round_history.json"
)
# NOTE: embedded-player-round-history.js is intentionally NOT published. Render serves over HTTP and
# fetches player_round_history.json directly; the embed is only a file:// demo fallback. Committing it
# added ~52 MB to every deploy transfer (and per-push churn) for a file the live site never loads.

foreach ($rel in $artifacts) {
  $abs = Join-Path $repoRoot $rel
  if (Test-Path $abs) {
    git -C $repoRoot add -f -- "$rel"
  }
}

if ($ArtifactsOnly) {
  Write-Host "ArtifactsOnly enabled: staging only generated data artifacts."
} else {
  Write-Host "Staging all repo changes (plus forced data artifacts) ..."
  git -C $repoRoot add -A
}

git -C $repoRoot diff --cached --quiet
if ($LASTEXITCODE -eq 0) {
  if (Bump-AlphaCaddieAppJsCache $webRoot) {
    git -C $repoRoot add -f -- "alpha-caddie-web/index.html"
    git -C $repoRoot diff --cached --quiet
  }
  if ($LASTEXITCODE -eq 0) {
    Write-Host "No staged changes after cache-bust attempt; on-disk mirrors under website/public/data/ were still updated above."
    if (-not $SkipPush) {
      $branchEarly = git -C $repoRoot rev-parse --abbrev-ref HEAD
      Write-Host "Pushing origin $branchEarly (in case local commits were already present) ..."
      git -C $repoRoot push origin $branchEarly
    }
    exit 0
  }
}

if ([string]::IsNullOrWhiteSpace($CommitMessage)) {
  if ($LiveWeekOnly) {
    $CommitMessage = "chore(data): live-week refresh $(Get-Date -Format 'yyyy-MM-dd')"
  } else {
    $CommitMessage = "chore(data): full refresh + publish $(Get-Date -Format 'yyyy-MM-dd')"
  }
}

& "$PSScriptRoot/ensure-web-deploy-ready.ps1" -RepoRoot $repoRoot -WebRoot $webRoot

git -C $repoRoot commit -m $CommitMessage
if ($LASTEXITCODE -ne 0) {
  throw "git commit failed with exit code $LASTEXITCODE"
}

if ($SkipPush) {
  Write-Host "Committed locally (SkipPush enabled)."
  exit 0
}

$branch = git -C $repoRoot rev-parse --abbrev-ref HEAD
Invoke-GitPushPublish $repoRoot $branch -SyncFirst:$PullFirst

Write-Host "Done: refreshed artifacts pushed (no Results build)."
