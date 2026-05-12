param(
  [switch] $SkipPush,
  [switch] $NoFullHistory,
  [switch] $ArtifactsOnly,
  [switch] $PullFirst,
  [string] $CommitMessage = ""
)

$ErrorActionPreference = "Stop"
#
# Course fit tab — generated artifacts this pipeline must publish:
#   • projections.json — field + meta.course_used (fetch:dg); outrights win/top5/10/20/cut after fetch:book-odds merge into DATA.outrights
#     (same Scratch API as datagolf.com/betting-tool-finish IMPLIED %). fetch:finish-tool re-merges that feed so the standalone script stays exercised;
#     set GOLF_FINISH_TOOL_PLAYWRIGHT=1 (+ optional DATAGOLF_PLAYWRIGHT_STORAGE_STATE) to capture browser JSON instead of direct API for missing markets.
#   • approach_skill_ytd.json — Predicted shot distance bins (fetch:dg preds/approach-skill); optional approach_skill_l12.json fallback if present.
#   • embedded-player-round-history.js (+ CSV / player_round_history.json / player-history shards) — radar, venue SG,
#     similarity, and Hole Hangout hole-level priors (build:history after fetch:dg).
#
# Hole Hangout — hole pars per hole for the active course/event:
#   • fetch:dg calls preds/live-hole-stats and writes hole_pars / hole_pars_source (live_hole_stats when DG serves it).
#   • fetch:in-play → live-in-play.json (bundled live_hole_stats); then merge:live-hole-pars-into-projections re-aligns
#     projections.json from that bundle so push:all publishes the same per-hole table as the live feed.
#   • course_holes.json — bundled overrides / gaps (committed); course_holes.local.json is gitignored for secrets.
#   • hole_pars_from_shots.json — fallback map from build:history (build-player-shots-web.mjs) after rounds CSV refresh.
#   • player-history/by-dg/*.json + manifest.json — hole-level score rows (hole_data.csv join in build-player-history.mjs);
#     Hole Hangout fetches shards at player-history/by-dg/{dg_id}.json. Staged below with other history artifacts.
#
# Matchup Analysis Tool stays fresh when these commands succeed:
#   fetch:dg  → projections.players (SG pillars + merged preds/live-tournament-stats driving when DG serves it),
#               projections.matchups (betting-tools/matchups), approach_skill_ytd.json (Course Fit shot bins)
#   fetch:book-odds → refreshed matchup/outright odds on projections.json
#   fetch:in-play → live-in-play.json → browser overlays placement win probs + live_tournament_stats distance/accuracy onto DATA.players
# Mirrors below copy projections + live + approach_skill *.json into website/public/data/ so both apps ship the same JSON.
#
# Round-projections / +EV weather: tee times live in live-in-play.json (field_updates from fetch:in-play);
# venue hourly forecast + banners resolve in the browser (app.js). Previously unstaged UI files excluded weather from this push — UI ships here when changed.

$repoRoot = Split-Path -Parent $PSScriptRoot
$webRoot = Join-Path $repoRoot "alpha-caddie-web"

if (-not (Test-Path (Join-Path $webRoot "package.json"))) {
  throw "Missing alpha-caddie-web/package.json. Run from the project checkout."
}

Set-Location $webRoot

if (-not $NoFullHistory) {
  $env:GOLF_HISTORICAL_ROUNDS_FULL_HISTORY = "1"
  Remove-Item Env:\GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS -ErrorAction SilentlyContinue
  Write-Host "Using FULL history merge (2004->present PGA + LIV rules)."
} else {
  Write-Host "Running without FULL history override."
}

function Run-Step([string] $label, [scriptblock] $command) {
  Write-Host $label
  & $command
  if ($LASTEXITCODE -ne 0) {
    throw "$label failed with exit code $LASTEXITCODE"
  }
}

Run-Step "Running fetch:dg ..." { npm run fetch:dg }
Run-Step "Running fetch:in-play ..." { npm run fetch:in-play }
Run-Step "Merging live_hole_stats into projections (Hole Hangout pars) ..." { npm run merge:live-hole-pars-into-projections }
Remove-Item Env:\GOLF_SKIP_DK_OU -ErrorAction SilentlyContinue
Remove-Item Env:\PERFECT_SKIP_FETCH_DK_OU -ErrorAction SilentlyContinue
Run-Step "Running fetch:dk-ou ..." { npm run fetch:dk-ou }
Run-Step "Running fetch:book-odds ..." { npm run fetch:book-odds }
Run-Step 'Running fetch:finish-tool — outrights, same Scratch feed as DG Finish Position; runs after book-odds ...' { npm run fetch:finish-tool }
Run-Step "Running update:rounds ..." { npm run update:rounds }
Run-Step "Running build:history ..." { npm run build:history }

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

Set-Location $repoRoot

$artifacts = @(
  "alpha-caddie-web/projections.json",
  "alpha-caddie-web/live-in-play.json",
  "alpha-caddie-web/approach_skill_ytd.json",
  "alpha-caddie-web/approach_skill_l12.json",
  "alpha-caddie-web/course_holes.json",
  "alpha-caddie-web/hole_pars_from_shots.json",
  "alpha-caddie-web/player_shots_web.json",
  "alpha-caddie-web/player-history",
  "website/public/data/projections.json",
  "website/public/data/live-in-play.json",
  "website/public/data/approach_skill_ytd.json",
  "website/public/data/approach_skill_l12.json",
  "data/historical_rounds_all.csv",
  "alpha-caddie-web/data/historical_rounds_all.csv",
  "alpha-caddie-web/player_round_history.json",
  "alpha-caddie-web/embedded-player-round-history.js"
)

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
  Write-Host "No staged changes; nothing to commit."
  exit 0
}

if ([string]::IsNullOrWhiteSpace($CommitMessage)) {
  $CommitMessage = "chore(data): full refresh + publish $(Get-Date -Format 'yyyy-MM-dd')"
}

git -C $repoRoot commit -m $CommitMessage
if ($LASTEXITCODE -ne 0) {
  throw "git commit failed with exit code $LASTEXITCODE"
}

if ($SkipPush) {
  Write-Host "Committed locally (SkipPush enabled)."
  exit 0
}

$branch = git -C $repoRoot rev-parse --abbrev-ref HEAD
if ($PullFirst) {
  Write-Host "Pulling latest origin/$branch with rebase ..."
  git -C $repoRoot pull --rebase origin $branch
  if ($LASTEXITCODE -ne 0) {
    throw "git pull --rebase failed with exit code $LASTEXITCODE"
  }
}
Write-Host "Pushing origin $branch ..."
git -C $repoRoot push origin $branch
if ($LASTEXITCODE -ne 0) {
  throw "git push failed with exit code $LASTEXITCODE"
}

Write-Host "Done: refreshed artifacts pushed (no Results build)."
