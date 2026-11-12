# Invoked by Windows Task Scheduler (optional local backup for push:live).
# IMPORTANT: do not use $ErrorActionPreference=Stop around npm — PowerShell treats
# native stderr (node progress / warnings) as terminating errors and aborts before EXIT.

$ErrorActionPreference = "Continue"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$webRoot = Join-Path $repoRoot "alpha-caddie-web"
$logDir = Join-Path $webRoot "logs"
New-Item -ItemType Directory -Force -Path $logDir | Out-Null
$log = Join-Path $logDir "push-live-scheduled.log"

function Write-Log([string] $msg) {
  $line = $msg
  Add-Content -Path $log -Value $line -Encoding utf8
}

$stamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss zzz"
Write-Log ""
Write-Log "===== $stamp ====="

Set-Location $webRoot
$env:Path =
  [Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
  [Environment]::GetEnvironmentVariable("Path", "User")

# Resolve npm.cmd explicitly (Task Scheduler PATH can be thin).
$npmCmd = $null
foreach ($dir in ($env:Path -split ';' | Where-Object { $_ })) {
  $candidate = Join-Path $dir "npm.cmd"
  if (Test-Path $candidate) { $npmCmd = $candidate; break }
}
if (-not $npmCmd) {
  Write-Log "FATAL: npm.cmd not found on PATH"
  exit 127
}

Write-Log "npm: $npmCmd"
Write-Log "cwd: $webRoot"

$code = 1
try {
  # cmd.exe captures stdout+stderr as text (UTF-8 log) and preserves npm's exit code
  # without PowerShell turning stderr lines into terminating errors.
  $arg = "/c `"`"$npmCmd`" run push:live >> `"$log`" 2>&1`""
  $p = Start-Process -FilePath "cmd.exe" -ArgumentList $arg -WorkingDirectory $webRoot -Wait -PassThru -NoNewWindow
  $code = [int]$p.ExitCode
} catch {
  Write-Log "FATAL: $($_.Exception.Message)"
  $code = 1
}

Write-Log "EXIT $code"
exit $code
