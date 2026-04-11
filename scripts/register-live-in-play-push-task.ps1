# Registers a Windows Scheduled Task: fetch live-in-play.json, commit if changed, git push.
# Complements nightly-refresh-and-push.ps1 for high cadence during tournament weeks.
#
# Run once (usually no admin if "run only when user is logged on"):
#   powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\register-live-in-play-push-task.ps1
#   powershell ... -File .\scripts\register-live-in-play-push-task.ps1 -IntervalMinutes 5
#
# Remove:
#   Unregister-ScheduledTask -TaskName 'GolfModel-LiveInPlayPush' -Confirm:$false

param(
  [int] $IntervalMinutes = 3,
  [string] $TaskName = "GolfModel-LiveInPlayPush"
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
$runner = Join-Path $repoRoot "scripts\live-in-play-push-once.ps1"
if (-not (Test-Path $runner)) {
  throw "Missing script: $runner"
}

if ($IntervalMinutes -lt 1 -or $IntervalMinutes -gt 59) {
  throw "IntervalMinutes must be between 1 and 59 (Task Scheduler repetition limit)."
}

$arg = "-NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File `"$runner`""
$action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument $arg -WorkingDirectory $repoRoot

$start = (Get-Date).AddMinutes(1)
$trigger = New-ScheduledTaskTrigger -Once -At $start `
  -RepetitionInterval (New-TimeSpan -Minutes $IntervalMinutes) `
  -RepetitionDuration (New-TimeSpan -Days 9999)

$settings = New-ScheduledTaskSettingsSet `
  -AllowStartIfOnBatteries `
  -DontStopIfGoingOnBatteries `
  -StartWhenAvailable `
  -MultipleInstances IgnoreNew `
  -ExecutionTimeLimit (New-TimeSpan -Hours 2)

$principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive -RunLevel Limited

Register-ScheduledTask -TaskName $TaskName -Action $action -Trigger $trigger -Settings $settings -Principal $principal `
  -Description "golfModel: book odds + preds/in-play → projections/live-in-play, commit+push if changed. Logs: logs\live-in-play-push.log" `
  -Force

Write-Host "Registered task '$TaskName' every $IntervalMinutes min (starts ~1 min from now), cwd $repoRoot"
Write-Host "Log file: $(Join-Path $repoRoot 'logs\live-in-play-push.log')"
Write-Host "Test now: powershell -NoProfile -ExecutionPolicy Bypass -File `"$runner`" -SkipPush"
Write-Host "Remove: Unregister-ScheduledTask -TaskName '$TaskName' -Confirm:`$false"
