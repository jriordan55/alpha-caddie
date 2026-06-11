param(
  [switch] $SkipPush,
  [switch] $SkipRefresh,
  [string] $CommitMessage = ""
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
$webRoot = Join-Path $repoRoot "alpha-caddie-web"

if (-not $SkipRefresh) {
  Push-Location $webRoot
  try {
    Write-Host "refresh:projections (fetch:dg + merge + weather + pin only) ..."
    npm run refresh:projections
  } finally {
    Pop-Location
  }
}

$artifacts = @(
  "alpha-caddie-web/projections.json",
  "alpha-caddie-web/data/pin_sheets/pin_sheet_active.json",
  "website/public/data/projections.json"
)

foreach ($rel in $artifacts) {
  $abs = Join-Path $repoRoot $rel
  if (Test-Path $abs) {
    git -C $repoRoot add -f -- "$rel" 2>&1 | Out-Null
  }
}

git -C $repoRoot add -f -- "alpha-caddie-web/scripts/refresh-projections-only.mjs" "alpha-caddie-web/package.json" "scripts/push-projections-only.ps1" "package.json" 2>&1 | Out-Null

git -C $repoRoot diff --cached --quiet
if ($LASTEXITCODE -eq 0) {
  Write-Host "No staged changes."
  if (-not $SkipPush) {
    $branch = git -C $repoRoot rev-parse --abbrev-ref HEAD
    git -C $repoRoot push origin $branch
  }
  exit 0
}

if ([string]::IsNullOrWhiteSpace($CommitMessage)) {
  $CommitMessage = "chore(data): projections + pin sheet $(Get-Date -Format 'yyyy-MM-dd HH:mm')"
}

git -C $repoRoot commit -m $CommitMessage
if ($LASTEXITCODE -ne 0) { throw "git commit failed" }

if ($SkipPush) {
  Write-Host "Committed locally (SkipPush)."
  exit 0
}

$branch = git -C $repoRoot rev-parse --abbrev-ref HEAD
Write-Host "Pushing origin $branch ..."
git -C $repoRoot push origin $branch
Write-Host "Done: projections pushed (no history rebuild)."
