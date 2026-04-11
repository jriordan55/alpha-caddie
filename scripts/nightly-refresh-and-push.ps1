# Nightly: R projections + export + live-in-play + build:history, then git commit & push.
#
# Prerequisites:
#   - Git remote configured; push works from this machine (Credential Manager, SSH agent, or PAT).
#   - DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json (not committed).
#   - R + Node as for update-website-full.ps1
#
# Optional env before run:
#   GOLF_NIGHTLY_MERGE_ROUNDS=1  — merge last 2 calendar years of DataGolf rounds into CSVs, then build:history
#
# Usage:
#   powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\nightly-refresh-and-push.ps1
#   powershell ... -File .\scripts\nightly-refresh-and-push.ps1 -SkipPush   # refresh only
#   powershell ... -File .\scripts\nightly-refresh-and-push.ps1 -PullFirst   # pull --rebase then push

param(
  [switch] $SkipPush,
  [switch] $PullFirst
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
$logDir = Join-Path $repoRoot "logs"
$logFile = Join-Path $logDir "nightly-push.log"
if (-not (Test-Path $logDir)) {
  New-Item -ItemType Directory -Path $logDir | Out-Null
}

function Write-Log([string] $msg) {
  $line = "$(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') $msg"
  Add-Content -LiteralPath $logFile -Value $line -Encoding UTF8
  Write-Host $line
}

Write-Log "===== start nightly-refresh-and-push (SkipPush=$SkipPush) ====="

$env:Path =
  [Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
  [Environment]::GetEnvironmentVariable("Path", "User")

Set-Location $repoRoot

$full = Join-Path $repoRoot "scripts\update-website-full.ps1"
if (-not (Test-Path $full)) {
  Write-Log "ERROR: missing $full"
  exit 1
}

Write-Log "Running update-website-full.ps1 ..."
& powershell -NoProfile -ExecutionPolicy Bypass -File $full
if ($LASTEXITCODE -ne 0) {
  Write-Log "ERROR: update-website-full.ps1 exit $LASTEXITCODE"
  exit $LASTEXITCODE
}

if ($env:GOLF_NIGHTLY_MERGE_ROUNDS -eq "1") {
  $web = Join-Path $repoRoot "alpha-caddie-web"
  $node = Join-Path $web "scripts\update-historical-rounds-node.mjs"
  if (Test-Path $node) {
    Write-Log "GOLF_NIGHTLY_MERGE_ROUNDS=1: merging recent DataGolf rounds (2y) ..."
    $env:GOLF_MODEL_DIR = $repoRoot
    $env:GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS = "2"
    Push-Location $web
    try {
      $out1 = & node $node 2>&1
      $code1 = $LASTEXITCODE
      $out1 | Add-Content -LiteralPath $logFile -Encoding UTF8
      if ($code1 -ne 0) {
        Write-Log "WARNING: update-historical-rounds-node.mjs exit $code1 (continuing)"
      }
      $out2 = & npm run build:history 2>&1
      $code2 = $LASTEXITCODE
      $out2 | Add-Content -LiteralPath $logFile -Encoding UTF8
      if ($code2 -ne 0) {
        Write-Log "WARNING: build:history exit $code2 (continuing)"
      }
    } finally {
      Pop-Location
    }
    Remove-Item Env:\GOLF_HISTORICAL_ROUNDS_RECENT_FETCH_YEARS -ErrorAction SilentlyContinue
  }
}

# Tracked artifacts we expect this pipeline to refresh (skip missing paths).
$toStage = @(
  "alpha-caddie-web/projections.json",
  "alpha-caddie-web/live-in-play.json",
  "alpha-caddie-web/embedded-player-round-history.js",
  "alpha-caddie-web/hole_pars_from_shots.json",
  "alpha-caddie-web/data/historical_rounds_all.csv",
  "data/historical_rounds_all.csv",
  "data/current_course_holes.rds",
  "website/public/data/projections.json"
)

foreach ($rel in $toStage) {
  $abs = Join-Path $repoRoot $rel
  if (Test-Path $abs) {
    git -C $repoRoot add -- "$rel"
  }
}

git -C $repoRoot diff --cached --quiet
if ($LASTEXITCODE -eq 0) {
  Write-Log "No staged changes; nothing to commit."
  Write-Log "===== done (no commit) ====="
  exit 0
}

$msg = "chore(data): nightly refresh $(Get-Date -Format 'yyyy-MM-dd HH:mm zzz')"
git -C $repoRoot commit -m $msg
if ($LASTEXITCODE -ne 0) {
  Write-Log "ERROR: git commit failed ($LASTEXITCODE)"
  exit $LASTEXITCODE
}

if ($SkipPush) {
  Write-Log "SkipPush: committed locally only."
  Write-Log "===== done ====="
  exit 0
}

$branch = git -C $repoRoot rev-parse --abbrev-ref HEAD
if ($PullFirst) {
  Write-Log "PullFirst: git pull --rebase origin $branch ..."
  $pullOut = git -C $repoRoot pull --rebase origin $branch 2>&1
  $pullCode = $LASTEXITCODE
  $pullOut | Add-Content -LiteralPath $logFile -Encoding UTF8
  if ($pullCode -ne 0) {
    Write-Log "ERROR: git pull --rebase failed ($pullCode)"
    exit $pullCode
  }
}

Write-Log "Pushing origin $branch ..."
$pushOut = git -C $repoRoot push origin $branch 2>&1
$pushCode = $LASTEXITCODE
$pushOut | Add-Content -LiteralPath $logFile -Encoding UTF8
if ($pushCode -ne 0) {
  Write-Log "ERROR: git push failed ($pushCode)"
  exit $pushCode
}

Write-Log "===== done (pushed) ====="
exit 0
