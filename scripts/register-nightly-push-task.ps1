# Registers a Windows Scheduled Task: nightly R+Node refresh, commit, and git push.
#
# Run once (usually no admin needed if "run only when user is logged on"):
#   powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\register-nightly-push-task.ps1
#   powershell ... -File .\scripts\register-nightly-push-task.ps1 -At "02:15"
#
# Requires: git push already works from a normal terminal for this repo.
#
param(
  [string] $At = "02:15",
  [string] $TaskName = "GolfModel-NightlyRefreshPush"
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
$runner = Join-Path $repoRoot "scripts\nightly-refresh-and-push.ps1"
if (-not (Test-Path $runner)) {
  throw "Missing script: $runner"
}

# Scheduled tasks often start with a minimal PATH; nightly-refresh-and-push.ps1 invokes R/Node via full paths where needed.
$arg = "-NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File `"$runner`""

$action = New-ScheduledTaskAction -Execute "powershell.exe" -Argument $arg -WorkingDirectory $repoRoot
$trigger = New-ScheduledTaskTrigger -Daily -At $At
$settings = New-ScheduledTaskSettingsSet `
  -AllowStartIfOnBatteries `
  -DontStopIfGoingOnBatteries `
  -StartWhenAvailable `
  -MultipleInstances IgnoreNew `
  -ExecutionTimeLimit (New-TimeSpan -Hours 4)
$principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive -RunLevel Limited

Register-ScheduledTask -TaskName $TaskName -Action $action -Trigger $trigger -Settings $settings -Principal $principal `
  -Description "golfModel: update-website-full (R+Node), commit data JSON/CSVs, git push. Logs: logs\nightly-push.log" `
  -Force

Write-Host "Registered task '$TaskName' daily at $At (local time), cwd $repoRoot"
Write-Host "Log file: $(Join-Path $repoRoot 'logs\nightly-push.log')"
Write-Host "Remove: Unregister-ScheduledTask -TaskName '$TaskName' -Confirm:`$false"
Write-Host "Test now: powershell -NoProfile -ExecutionPolicy Bypass -File `"$runner`" -SkipPush"
