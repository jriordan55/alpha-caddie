<#
.SYNOPSIS
  Remove oversized CSV blobs from *all* Git history so GitHub accepts `git push` (Render deploys from GitHub).

.DESCRIPTION
  `git rm --cached` is not enough: GitHub scans every commit. This runs git-filter-repo with --invert-paths
  to strip these paths from the entire history (paths that never existed are ignored by filter-repo).

  Typical GH001 offenders: all_shots*.csv, mirrored historical_rounds_all under alpha-caddie-web/data/.

  Install tool (one-time):
    py -3 -m pip install --user git-filter-repo

  filter-repo removes `origin` — re-add it before force-push:
    git remote add origin https://github.com/<you>/<repo>.git
    git push origin main --force-with-lease
#>
$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

$pathsToStripFromHistory = @(
  "alpha-caddie-web/data/all_shots_2021_2026.csv",
  "alpha-caddie-web/data/all_shots_2022_2026.csv",
  "alpha-caddie-web/data/all_shots_2022_2026_round_fairways_gir_putts.csv",
  "alpha-caddie-web/data/historical_rounds_all.csv",
  "data/all_shots_2021_2026.csv",
  "data/all_shots_2022_2026.csv",
  "data/all_shots_2022_2026_round_fairways_gir_putts.csv",
  "data/historical_rounds_all.csv"
)

$frArgs = @("--force", "--invert-paths")
foreach ($p in $pathsToStripFromHistory) {
  $frArgs += @("--path", $p)
}

if (Get-Command git-filter-repo -ErrorAction SilentlyContinue) {
  Write-Host "Using: git-filter-repo"
  & git-filter-repo @frArgs
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  Write-Host "`nOK. Next:"
  Write-Host '  git remote add origin https://github.com/<you>/<repo>.git   # if git remote -v is empty'
  Write-Host "  git push origin main --force-with-lease"
  exit 0
}

foreach ($cmd in @(
    @("py", "-3", "-m", "git_filter_repo"),
    @("py", "-m", "git_filter_repo"),
    @("python3", "-m", "git_filter_repo"),
    @("python", "-m", "git_filter_repo")
  )) {
  Write-Host ("Trying: " + ($cmd -join " ") + " …")
  & $cmd[0] @($cmd[1..($cmd.Length - 1)] + $frArgs)
  if ($LASTEXITCODE -eq 0) {
    Write-Host "`nOK. Next:"
    Write-Host '  git remote add origin https://github.com/<you>/<repo>.git   # if remotes were cleared'
    Write-Host "  git push origin main --force-with-lease"
    exit 0
  }
}

Write-Host ""
Write-Host "git-filter-repo not found. Install:"
Write-Host "  py -3 -m pip install --user git-filter-repo"
Write-Host "Then re-run this script, or run manually from repo root:"
Write-Host ('  py -3 -m git_filter_repo ' + ($frArgs -join ' '))
exit 1
