<#
.SYNOPSIS
  Remove oversized CSV blobs from *all* Git history so GitHub accepts `git push`.

.DESCRIPTION
  `git rm --cached` is not enough: GitHub scans every commit. This runs git-filter-repo
  to delete these paths from history:

    alpha-caddie-web/data/all_shots_2021_2026.csv   (>100MB — always rejected)
    alpha-caddie-web/data/historical_rounds_all.csv (large mirror — keep local only)

  Install tool (one-time):
    py -3 -m pip install --user git-filter-repo

  Then either add Python Scripts to PATH, or use `py -3 -m git_filter_repo` (script tries both).

  After success, force-push:
    git push origin main --force-with-lease
#>
$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot
Set-Location $repoRoot

$frArgs = @(
  "--force",
  "--invert-paths",
  "--path", "alpha-caddie-web/data/all_shots_2021_2026.csv",
  "--path", "alpha-caddie-web/data/historical_rounds_all.csv"
)

if (Get-Command git-filter-repo -ErrorAction SilentlyContinue) {
  Write-Host "Using: git-filter-repo"
  & git-filter-repo @frArgs
  if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
  Write-Host "`nOK. Next: git push origin main --force-with-lease"
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
    Write-Host "`nOK. Next: git push origin main --force-with-lease"
    exit 0
  }
}

Write-Host ""
Write-Host "git-filter-repo not found. Install:"
Write-Host "  py -3 -m pip install --user git-filter-repo"
Write-Host "Then re-run this script, or run manually from repo root:"
Write-Host ('  py -3 -m git_filter_repo ' + ($frArgs -join ' '))
exit 1
