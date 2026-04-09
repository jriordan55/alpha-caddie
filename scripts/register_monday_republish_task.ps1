# Run in PowerShell (as your user - rsconnect tokens are per-user).
# Creates a weekly task: Monday 6:00 AM local time -> run_monday_republish.ps1
#
#   cd /d C:\Users\student\Documents\golfModel
#   powershell -ExecutionPolicy Bypass -File .\scripts\register_monday_republish_task.ps1
#
# Remove task: Unregister-ScheduledTask -TaskName "GolfModel Monday Republish" -Confirm:$false

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$runner = Join-Path $repoRoot "scripts\run_monday_republish.ps1"
if (-not (Test-Path $runner)) {
    throw "Missing: $runner"
}

$taskName = "GolfModel Monday Republish"
$ps = Join-Path $env:WINDIR "System32\WindowsPowerShell\v1.0\powershell.exe"
$arg = "-NoProfile -ExecutionPolicy Bypass -File `"$runner`""

$action = New-ScheduledTaskAction -Execute $ps -Argument $arg
$sixAm = [DateTime]::Today.AddHours(6)
$trigger = New-ScheduledTaskTrigger -Weekly -DaysOfWeek Monday -At $sixAm

# Run only when user is logged on (typical for rsconnect credentials in user profile)
# RunLevel: Limited (default user) or Highest (admin). "Least" is not valid on this cmdlet.
$userId = [System.Security.Principal.WindowsIdentity]::GetCurrent().Name
$principal = New-ScheduledTaskPrincipal -UserId $userId -LogonType Interactive -RunLevel Limited

$settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries -StartWhenAvailable

$taskPath = "\"
Register-ScheduledTask -TaskPath $taskPath -TaskName $taskName -Action $action -Trigger $trigger -Principal $principal -Settings $settings -Force | Out-Null

Write-Host ""
Write-Host "OK - Registered scheduled task: $taskName"
Write-Host "  Run as: $userId"
Write-Host "  Action: $ps $arg"
Write-Host '  Trigger: Every Monday at 6:00 AM (local time)'
Write-Host ""

$verify = Get-ScheduledTask -TaskPath $taskPath -TaskName $taskName -ErrorAction Stop
Write-Host "Verified in Task Scheduler - State: $($verify.State)"
Write-Host "  Open: taskschd.msc -> Task Scheduler Library -> $taskName"
