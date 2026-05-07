param(
  [switch] $SkipPush,
  [switch] $NoFullHistory,
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

Write-Host "Running update:rounds ..."
npm run update:rounds
if ($LASTEXITCODE -ne 0) {
  throw "npm run update:rounds failed with exit code $LASTEXITCODE"
}

Write-Host "Running build:history ..."
npm run build:history
if ($LASTEXITCODE -ne 0) {
  throw "npm run build:history failed with exit code $LASTEXITCODE"
}

Set-Location $repoRoot

$artifacts = @(
  "data/historical_rounds_all.csv",
  "alpha-caddie-web/data/historical_rounds_all.csv",
  "alpha-caddie-web/player_round_history.json",
  "alpha-caddie-web/embedded-player-round-history.js"
)

foreach ($rel in $artifacts) {
  $abs = Join-Path $repoRoot $rel
  if (Test-Path $abs) {
    git -C $repoRoot add -- "$rel"
  }
}

git -C $repoRoot diff --cached --quiet
if ($LASTEXITCODE -eq 0) {
  Write-Host "No staged history changes; nothing to commit."
  exit 0
}

if ([string]::IsNullOrWhiteSpace($CommitMessage)) {
  $CommitMessage = "chore(data): refresh historical rounds/history $(Get-Date -Format 'yyyy-MM-dd')"
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
Write-Host "Pushing origin $branch ..."
git -C $repoRoot push origin $branch
if ($LASTEXITCODE -ne 0) {
  throw "git push failed with exit code $LASTEXITCODE"
}

Write-Host "Done: history artifacts refreshed and pushed."
