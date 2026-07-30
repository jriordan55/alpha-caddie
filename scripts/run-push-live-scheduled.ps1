# Invoked by Windows Task Scheduler (optional local backup for push:live).
$ErrorActionPreference = "Stop"

$repoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$webRoot = Join-Path $repoRoot "alpha-caddie-web"
$logDir = Join-Path $webRoot "logs"
New-Item -ItemType Directory -Force -Path $logDir | Out-Null
$log = Join-Path $logDir "push-live-scheduled.log"

$stamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss zzz"
Add-Content -Path $log -Value ""
Add-Content -Path $log -Value "===== $stamp ====="

Set-Location $webRoot
$env:Path =
  [Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
  [Environment]::GetEnvironmentVariable("Path", "User")

# Prefer machine-local key file if present; else DATAGOLF_API_KEY from User env.
npm run push:live *>> $log 2>&1
$code = $LASTEXITCODE
Add-Content -Path $log -Value "EXIT $code"
exit $code
