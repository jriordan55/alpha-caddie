# LOCAL git commit + push after DK scrape (legacy). Prefer remote:
#   npm run push:dk-round-projections  → Render API or GitHub Actions (no local git)
#
# Usage:
#   npm run push:dk-round-projections:git
#   powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\dk-round-projections-push-once.ps1
#   powershell ... -File .\scripts\dk-round-projections-push-once.ps1 -SkipPush
#   powershell ... -File .\scripts\dk-round-projections-push-once.ps1 -PullFirst

param(
  [switch] $SkipPush,
  [switch] $PullFirst
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
$logDir = Join-Path $repoRoot "logs"
$logFile = Join-Path $logDir "dk-round-projections-push.log"
if (-not (Test-Path $logDir)) {
  New-Item -ItemType Directory -Path $logDir | Out-Null
}

function Write-Log([string] $msg) {
  $line = "$(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') $msg"
  Add-Content -LiteralPath $logFile -Value $line -Encoding UTF8
  Write-Host $line
}

Write-Log "===== start dk-round-projections-push (SkipPush=$SkipPush) ====="

$env:Path =
  [Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
  [Environment]::GetEnvironmentVariable("Path", "User")

Set-Location $repoRoot

$web = Join-Path $repoRoot "alpha-caddie-web"
$updateDk = Join-Path $web "scripts\update-dk-round-projections.mjs"
if (-not (Test-Path $updateDk)) {
  Write-Log "ERROR: missing $updateDk"
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

Write-Log "Running update-dk-round-projections.mjs …"
Push-Location $web
try {
  & $nodeExe $updateDk
} finally {
  Pop-Location
}
if ($LASTEXITCODE -ne 0) {
  Write-Log "ERROR: update-dk-round-projections.mjs exit $LASTEXITCODE"
  exit $LASTEXITCODE
}

$projRel = "alpha-caddie-web/projections.json"
$websiteProjRel = "website/public/data/projections.json"
$auditRel = "alpha-caddie-web/data/dk_round_projection_audit.csv"

$pathsToStage = @($projRel)
$websiteProjAbs = Join-Path $repoRoot $websiteProjRel
if (Test-Path $websiteProjAbs) {
  $pathsToStage += $websiteProjRel
}
$auditAbs = Join-Path $repoRoot $auditRel
if (Test-Path $auditAbs) {
  $pathsToStage += $auditRel
}

git -C $repoRoot add -- @pathsToStage

git -C $repoRoot diff --cached --quiet
if ($LASTEXITCODE -eq 0) {
  Write-Log "No staged changes; nothing to commit."
  Write-Log "===== done (no commit) ====="
  exit 0
}

$msg = "chore(data): DraftKings round projections $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss zzz')"
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
