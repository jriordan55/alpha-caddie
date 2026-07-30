# Optional LOCAL backup: register Windows Task Scheduler for npm run push:live at 9:00 PM Eastern.
# This only runs when the PC is on (or asleep with wake timers). For "PC off", use the
# GitHub Action `.github/workflows/nightly-push-live.yml` instead.
#
# Run once in an elevated PowerShell (optional; Action is the PC-off path):
#   powershell -ExecutionPolicy Bypass -File scripts/register-push-live-task.ps1

param(
  [string] $TaskName = "AlphaCaddie-PushLive-9pmET",
  [string] $TimeLocal = "21:00"
)

$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$webRoot = Join-Path $repoRoot "alpha-caddie-web"
$wrapper = Join-Path $repoRoot "scripts\run-push-live-scheduled.ps1"
$logDir = Join-Path $webRoot "logs"

if (-not (Test-Path $wrapper)) {
  throw "Missing $wrapper"
}

New-Item -ItemType Directory -Force -Path $logDir | Out-Null

$action = New-ScheduledTaskAction `
  -Execute "powershell.exe" `
  -Argument "-NoProfile -ExecutionPolicy Bypass -File `"$wrapper`"" `
  -WorkingDirectory $webRoot

# Daily at 9:00 PM on this machine's local clock (set Windows timezone to Eastern).
$trigger = New-ScheduledTaskTrigger -Daily -At $TimeLocal

$settings = New-ScheduledTaskSettingsSet `
  -AllowStartIfOnBatteries `
  -DontStopIfGoingOnBatteries `
  -StartWhenAvailable `
  -WakeToRun `
  -ExecutionTimeLimit (New-TimeSpan -Hours 3)

$principal = New-ScheduledTaskPrincipal `
  -UserId $env:USERNAME `
  -LogonType Interactive `
  -RunLevel Highest

Register-ScheduledTask `
  -TaskName $TaskName `
  -Action $action `
  -Trigger $trigger `
  -Settings $settings `
  -Principal $principal `
  -Force | Out-Null

Write-Host "Registered scheduled task '$TaskName' daily at $TimeLocal local time."
Write-Host "Wrapper: $wrapper"
Write-Host ""
Write-Host "IMPORTANT: WakeToRun only works from sleep/hibernate — not if the PC is fully powered off."
Write-Host "For PC-off nights, enable GitHub Actions workflow 'Nightly push:live' and set secret DATAGOLF_API_KEY."
