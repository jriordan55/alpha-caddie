# One-shot: refresh historical CSVs (R) + projections + player history (Node).
# Run from anywhere:  pwsh -File scripts/refresh_all_data.ps1
# Schedule this weekly in Task Scheduler for "always updating" data.
#
# Requires: R + live_updates.R deps, pgatouR (for PGA history), Node/npm, DATAGOLF_API_KEY for fetch:dg.

param(
  [string] $RepoRoot = (Split-Path $PSScriptRoot -Parent)
)

$ErrorActionPreference = "Stop"
$env:GOLF_MODEL_DIR = $RepoRoot

function Find-Rscript {
  if ($env:RSCRIPT_PATH -and (Test-Path $env:RSCRIPT_PATH)) { return $env:RSCRIPT_PATH }
  if ($env:R_HOME) {
    $exe = Join-Path $env:R_HOME "bin\Rscript.exe"
    if (Test-Path $exe) { return $exe }
  }
  $pf = $env:ProgramFiles
  if (-not $pf) { $pf = "C:\Program Files" }
  $rRoot = Join-Path $pf "R"
  if (Test-Path $rRoot) {
    $latest = Get-ChildItem $rRoot -Directory -ErrorAction SilentlyContinue |
      Where-Object { $_.Name -match '^R-[\d.]+$' } |
      Sort-Object { [version]($_.Name -replace '^R-','') } -Descending |
      Select-Object -First 1
    if ($latest) {
      $exe = Join-Path $latest.FullName "bin\Rscript.exe"
      if (Test-Path $exe) { return $exe }
    }
  }
  return $null
}

Set-Location $RepoRoot
Write-Host "Repo: $RepoRoot"

$rscript = Find-Rscript
function Run-RScriptFile {
  param(
    [string] $ScriptPath,
    [string] $Label
  )
  if (-not (Test-Path $ScriptPath)) {
    Write-Warning "$Label script not found: $ScriptPath"
    return
  }
  Write-Host "Running $Label ..."
  & $rscript $ScriptPath
  if ($LASTEXITCODE -ne 0) {
    throw "$Label failed with exit code $LASTEXITCODE"
  }
}

if ($rscript) {
  Write-Host "Running live_updates_2026() via $rscript"
  & $rscript --vanilla -e "source('live_updates.R'); live_updates_2026()"
  if ($LASTEXITCODE -ne 0) {
    throw "live_updates_2026 failed with exit code $LASTEXITCODE"
  }
  Run-RScriptFile -ScriptPath (Join-Path $RepoRoot "scripts\pull_pga_tournament_metadata_excel.R") -Label "tournament metadata pull"
  Run-RScriptFile -ScriptPath (Join-Path $RepoRoot "scripts\join_tournament_metadata_to_historical_rounds.R") -Label "historical rounds + metadata join"
} else {
  Write-Warning "Rscript not found (set R_HOME or RSCRIPT_PATH). Skipping live_updates - CSVs unchanged."
}

$web = Join-Path $RepoRoot "alpha-caddie-web"
if (-not (Test-Path $web)) { throw "Missing alpha-caddie-web folder" }
Set-Location $web
Write-Host "Running npm run fetch:dg (projections + history) ..."
npm run fetch:dg
Write-Host "Done. Serve with: cd alpha-caddie-web; npm start"
