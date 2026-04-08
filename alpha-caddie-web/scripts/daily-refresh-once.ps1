# Runs the full Alpha Caddie Web DataGolf refresh once (projections + historical rounds + player history + embed).
# Intended for Windows Task Scheduler. Requires Node, R + Rscript on PATH, and datagolf.local.json or DATAGOLF_API_KEY.
$ErrorActionPreference = "Stop"
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$webRoot = (Resolve-Path (Join-Path $here "..")).Path
Set-Location $webRoot

$env:Path =
  [Environment]::GetEnvironmentVariable("Path", "Machine") + ";" +
  [Environment]::GetEnvironmentVariable("Path", "User")

$logDir = Join-Path $webRoot "logs"
if (-not (Test-Path $logDir)) {
  New-Item -ItemType Directory -Path $logDir | Out-Null
}
$log = Join-Path $logDir "daily-refresh.log"
$stamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss zzz"
Add-Content -Path $log -Value ""
Add-Content -Path $log -Value "===== $stamp ====="

# DataGolf (projections + historical_rounds_all + player history + embed) then PGA shot-level CSV
npm run refresh:daily *>> $log
$code = $LASTEXITCODE
if ($code -ne 0) {
  Add-Content -Path $log -Value "EXIT $code"
}
exit $code
