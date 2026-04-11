# Tournament pipeline: refresh book odds in projections.json, fetch live-in-play.json, then git commit + push if anything changed.
# Book lines come from betting-tools APIs plus optional DraftKings round props (Birdies/Pars/Bogeys) via
# Playwright in fetch-book-odds-into-projections.mjs (see alpha-caddie-web/scripts/draftkings-ou-props.mjs).
# Live model / cut signals come from preds/in-play (fetch-live-in-play.mjs).
#
# Prerequisites: Node on PATH; DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json; git push works.
#
# Usage:
#   powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\live-in-play-push-once.ps1
#   powershell ... -File .\scripts\live-in-play-push-once.ps1 -SkipBookOdds   # in-play only
#   powershell ... -File .\scripts\live-in-play-push-once.ps1 -SkipPush
#   powershell ... -File .\scripts\live-in-play-push-once.ps1 -PullFirst

param(
  [switch] $SkipPush,
  [switch] $PullFirst,
  [switch] $SkipBookOdds
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
$logDir = Join-Path $repoRoot "logs"
$logFile = Join-Path $logDir "live-in-play-push.log"
if (-not (Test-Path $logDir)) {
  New-Item -ItemType Directory -Path $logDir | Out-Null
}

function Write-Log([string] $msg) {
  $line = "$(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') $msg"
  Add-Content -LiteralPath $logFile -Value $line -Encoding UTF8
  Write-Host $line
}

Write-Log "===== start live-data-push-once (SkipPush=$SkipPush SkipBookOdds=$SkipBookOdds) ====="

$env:Path =
  [Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
  [Environment]::GetEnvironmentVariable("Path", "User")

Set-Location $repoRoot

$web = Join-Path $repoRoot "alpha-caddie-web"
$fetchLive = Join-Path $web "scripts\fetch-live-in-play.mjs"
$fetchBook = Join-Path $web "scripts\fetch-book-odds-into-projections.mjs"
if (-not (Test-Path $fetchLive)) {
  Write-Log "ERROR: missing $fetchLive"
  exit 1
}

$nodeCmd = Get-Command node -ErrorAction SilentlyContinue
if (-not $nodeCmd) {
  Write-Log "ERROR: node not on PATH"
  exit 1
}
$nodeExe = $nodeCmd.Source
if (-not $nodeExe) { $nodeExe = $nodeCmd.Path }

$env:GOLF_MODEL_DIR = $repoRoot

if (-not $SkipBookOdds -and (Test-Path $fetchBook)) {
  Write-Log "Running fetch-book-odds-into-projections.mjs …"
  Push-Location $web
  try {
    & $nodeExe $fetchBook
  } finally {
    Pop-Location
  }
  if ($LASTEXITCODE -ne 0) {
    Write-Log "ERROR: fetch-book-odds-into-projections.mjs exit $LASTEXITCODE"
    exit $LASTEXITCODE
  }
} elseif ($SkipBookOdds) {
  Write-Log "SkipBookOdds: skipping book odds refresh."
} else {
  Write-Log "WARNING: missing $fetchBook — skipping book odds refresh."
}

Write-Log "Running fetch-live-in-play.mjs …"
Push-Location $web
try {
  & $nodeExe $fetchLive
} finally {
  Pop-Location
}

if ($LASTEXITCODE -ne 0) {
  Write-Log "ERROR: fetch-live-in-play.mjs exit $LASTEXITCODE"
  exit $LASTEXITCODE
}

$liveRel = "alpha-caddie-web/live-in-play.json"
$liveAbs = Join-Path $repoRoot $liveRel
if (-not (Test-Path $liveAbs)) {
  Write-Log "ERROR: missing $liveAbs"
  exit 1
}

$projRel = "alpha-caddie-web/projections.json"
$websiteProjRel = "website/public/data/projections.json"
$websiteProjAbs = Join-Path $repoRoot $websiteProjRel

git -C $repoRoot add -- "$liveRel" "$projRel"
if (Test-Path $websiteProjAbs) {
  git -C $repoRoot add -- "$websiteProjRel"
}

git -C $repoRoot diff --cached --quiet
if ($LASTEXITCODE -eq 0) {
  Write-Log "No staged changes; nothing to commit."
  Write-Log "===== done (no commit) ====="
  exit 0
}

$msg = "chore(data): live + book odds $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss zzz')"
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
