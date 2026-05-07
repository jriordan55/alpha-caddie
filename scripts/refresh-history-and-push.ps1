param(
  [switch] $SkipPush,
  [switch] $NoFullHistory,
  [switch] $SkipResultsBuild,
  [switch] $ArtifactsOnly,
  [switch] $PullFirst,
  [string] $CommitMessage = ""
)

$ErrorActionPreference = "Stop"

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
Remove-Item Env:\GOLF_SKIP_DK_OU -ErrorAction SilentlyContinue
Remove-Item Env:\PERFECT_SKIP_FETCH_DK_OU -ErrorAction SilentlyContinue
Run-Step "Running fetch:dk-ou ..." { npm run fetch:dk-ou }
Run-Step "Running fetch:book-odds ..." { npm run fetch:book-odds }
Run-Step "Running update:rounds ..." { npm run update:rounds }
Run-Step "Running build:history ..." { npm run build:history }
if (-not $SkipResultsBuild) {
  $env:RESULTS_EXPORT_LAST_YEARS = "2"
  Run-Step "Running build:results ..." { npm run build:results }
} else {
  Write-Host "Skipping build:results (SkipResultsBuild enabled)."
}

Set-Location $repoRoot

$artifacts = @(
  "alpha-caddie-web/projections.json",
  "alpha-caddie-web/live-in-play.json",
  "website/public/data/projections.json",
  "data/historical_rounds_all.csv",
  "alpha-caddie-web/data/historical_rounds_all.csv",
  "alpha-caddie-web/player_round_history.json",
  "alpha-caddie-web/embedded-player-round-history.js",
  "alpha-caddie-web/data/results_backtest.json",
  "alpha-caddie-web/data/results_kelly_bets.json"
)

foreach ($rel in $artifacts) {
  $abs = Join-Path $repoRoot $rel
  if (Test-Path $abs) {
    # These artifacts are intentionally gitignored for normal development;
    # this publish script force-stages them for Render snapshot deploys.
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

Write-Host "Done: history artifacts refreshed and pushed."
