# Long-running loop: fetch live-in-play.json, commit+push when it changes, sleep, repeat.
# Use during tournament week in a dedicated terminal (or use register-live-in-play-push-task.ps1 instead).
#
# Usage:
#   powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\tournament-live-push-loop.ps1
#   powershell ... -File .\scripts\tournament-live-push-loop.ps1 -IntervalSec 90 -SkipPush

param(
  [int] $IntervalSec = 120,
  [switch] $SkipPush,
  [switch] $PullFirst
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
$once = Join-Path $repoRoot "scripts\live-in-play-push-once.ps1"
if (-not (Test-Path $once)) {
  throw "Missing script: $once"
}

if ($IntervalSec -lt 30) {
  throw "IntervalSec must be at least 30."
}

Write-Host "tournament-live-push-loop: every $IntervalSec s → $once (Ctrl+C to stop)"

while ($true) {
  try {
    & $once -SkipPush:$SkipPush -PullFirst:$PullFirst
    $code = $LASTEXITCODE
    if ($code -ne 0) {
      Write-Warning "live-in-play-push-once exited $code (sleeping anyway)"
    }
  } catch {
    Write-Warning $_
  }
  Start-Sleep -Seconds $IntervalSec
}
