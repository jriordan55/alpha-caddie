param(
  [Parameter(Mandatory = $true)][string] $RepoRoot,
  [Parameter(Mandatory = $true)][string] $WebRoot
)

$ErrorActionPreference = "Stop"

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
  Write-Host "Bumped alpha-caddie-web/index.html app.js cache to v=$n2"
  return $true
}

Push-Location $WebRoot
try {
  Write-Host "Running verify:web-deploy ..."
  # Soft mode: thin player/season canaries warn instead of fail (same as push:live refresh).
  # Never abort the commit/push on verify - mid-tournament data often lags live feeds.
  $prevSoft = $env:GOLF_LIVE_VALIDATE_SOFT
  $env:GOLF_LIVE_VALIDATE_SOFT = "1"
  try {
    npm run verify:web-deploy
  } finally {
    if ($null -eq $prevSoft) {
      Remove-Item Env:\GOLF_LIVE_VALIDATE_SOFT -ErrorAction SilentlyContinue
    } else {
      $env:GOLF_LIVE_VALIDATE_SOFT = $prevSoft
    }
  }
  if ($LASTEXITCODE -ne 0) {
    Write-Warning "verify:web-deploy exited $($LASTEXITCODE) - continuing publish anyway"
  }
} finally {
  Pop-Location
}

$staged = @(& git -C $RepoRoot diff --cached --name-only 2>$null)
$appJsStaged = $staged | Where-Object { $_ -replace '\\', '/' -eq 'alpha-caddie-web/app.js' }
$indexStaged = $staged | Where-Object { $_ -replace '\\', '/' -eq 'alpha-caddie-web/index.html' }

if ($appJsStaged -and -not $indexStaged) {
  if (Bump-AlphaCaddieAppJsCache $WebRoot) {
    git -C $RepoRoot add -f -- "alpha-caddie-web/index.html"
  }
}
