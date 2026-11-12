# Optional LOCAL backup: register Windows Task Scheduler for npm run push:live at 9:00 PM Eastern.
# This only runs when the PC is on (or asleep with wake timers). For "PC off", use the
# GitHub Action `.github/workflows/nightly-push-live.yml` instead.
#
# Run once:
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
  -MultipleInstances IgnoreNew `
  -ExecutionTimeLimit (New-TimeSpan -Hours 3)

# Interactive + Limited: no admin elevation required; runs when you are logged on.
$principal = New-ScheduledTaskPrincipal `
  -UserId $env:USERNAME `
  -LogonType Interactive `
  -RunLevel Limited

Register-ScheduledTask `
  -TaskName $TaskName `
  -Action $action `
  -Trigger $trigger `
  -Settings $settings `
  -Principal $principal `
  -Description "Alpha Caddie: npm run push:live at 9pm local (DK/PP/SL/UD/FD/KL/CZR/Onyx/Courtside round props), commit + push. Log: alpha-caddie-web/logs/push-live-scheduled.log" `
  -Force | Out-Null

Write-Host "Registered scheduled task '$TaskName' daily at $TimeLocal local time."
Write-Host "Wrapper: $wrapper"
Write-Host "Log: $(Join-Path $logDir 'push-live-scheduled.log')"
Write-Host ""
Write-Host "WakeToRun works from sleep/hibernate, not if the PC is fully powered off."
Write-Host "For PC-off nights, keep GitHub Actions workflow 'Nightly push:live' enabled with secret DATAGOLF_API_KEY."
Write-Host "Remove: Unregister-ScheduledTask -TaskName '$TaskName' -Confirm:`$false"
