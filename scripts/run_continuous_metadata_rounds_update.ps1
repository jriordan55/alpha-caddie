# Continuously refresh historical rounds + tournament metadata join.
# Runs in a loop until stopped (Ctrl+C).
#
# Example:
#   powershell -ExecutionPolicy Bypass -File scripts/run_continuous_metadata_rounds_update.ps1
#   powershell -ExecutionPolicy Bypass -File scripts/run_continuous_metadata_rounds_update.ps1 -IntervalMinutes 30

param(
  [string] $RepoRoot = "",
  [int] $IntervalMinutes = 30,
  [switch] $Once
)

$ErrorActionPreference = "Stop"

if (-not $RepoRoot) {
  $scriptRoot = if ($PSScriptRoot) { $PSScriptRoot } else { Split-Path -Parent $MyInvocation.MyCommand.Path }
  $RepoRoot = Split-Path $scriptRoot -Parent
}
if ($IntervalMinutes -lt 1) { $IntervalMinutes = 1 }

$runner = Join-Path $RepoRoot "scripts\refresh_all_data.ps1"
if (-not (Test-Path $runner)) {
  throw "Missing script: $runner"
}

$logDir = Join-Path $RepoRoot "logs"
if (-not (Test-Path $logDir)) {
  New-Item -ItemType Directory -Path $logDir | Out-Null
}
$logFile = Join-Path $logDir "continuous_metadata_rounds_update.log"

function Write-Log([string] $msg) {
  $line = "[$(Get-Date -Format s)] $msg"
  $line | Tee-Object -FilePath $logFile -Append | Out-Host
}

do {
  Write-Log "Starting refresh cycle."
  try {
    & powershell -NoProfile -ExecutionPolicy Bypass -File $runner -RepoRoot $RepoRoot
    if ($LASTEXITCODE -ne 0) {
      Write-Log "Refresh cycle failed (exit $LASTEXITCODE)."
    } else {
      Write-Log "Refresh cycle completed."
    }
  } catch {
    Write-Log ("Refresh cycle error: " + $_.Exception.Message)
  }

  if ($Once) { break }
  Write-Log "Sleeping $IntervalMinutes minute(s) ..."
  Start-Sleep -Seconds ($IntervalMinutes * 60)
} while ($true)

Write-Log "Stopped."
