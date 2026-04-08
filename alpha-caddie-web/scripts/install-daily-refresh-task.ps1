# Installs a Windows Scheduled Task to run npm refresh:daily every morning.
# If "running scripts is disabled", use install-daily-refresh-task.cmd instead, or run:
#   powershell -NoProfile -ExecutionPolicy Bypass -File .\install-daily-refresh-task.ps1 -At "07:30"
# (Admin not usually required for "run only when user is logged on".)
param(
  [string]$At = "06:00",
  [string]$TaskName = "AlphaCaddieWeb-DailyRefresh"
)

$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$runner = Join-Path $here "daily-refresh-once.ps1"
if (-not (Test-Path $runner)) {
  throw "Missing script: $runner"
}

$arg = "-NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File `"$runner`""
$action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument $arg
$trigger = New-ScheduledTaskTrigger -Daily -At $At
$settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable -MultipleInstances IgnoreNew
$principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive -RunLevel Limited

Register-ScheduledTask -TaskName $TaskName -Action $action -Trigger $trigger -Settings $settings -Principal $principal `
  -Description "Alpha Caddie Web: daily DataGolf + PGA shots (projections, historical_rounds_all, player history, embed, all_shots_2021_2026.csv)." `
  -Force

Write-Host "Registered task '$TaskName' daily at $At (local time)."
Write-Host "Logs: $(Join-Path (Split-Path $here -Parent) 'logs\daily-refresh.log')"
Write-Host "Remove: Unregister-ScheduledTask -TaskName '$TaskName' -Confirm:`$false"
