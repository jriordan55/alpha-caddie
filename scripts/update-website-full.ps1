# One-shot refresh for everything the static Alpha Caddie site reads from the repo before you git push.
#
# 1) R: round_projections.R → simulated_round_static.rds
# 2) R: export_projections_for_website.R → website/public/data/projections.json + alpha-caddie-web/projections.json
# 3) R: fetch_datagolf_in_play.R → alpha-caddie-web/live-in-play.json (+EV / placement merge on load)
# 4) Node: npm run build:history (player_round_history.json, embedded history JS, player_shots_web.json)
#
# Requires: R + Rscript, Node + npm, DATAGOLF_API_KEY or alpha-caddie-web/datagolf.local.json
# Optional: npm install once inside alpha-caddie-web/
#
# Does NOT run npm run fetch:dg (that would replace R projections with the Node pipeline).
# To refresh historical_rounds_all.csv from DataGolf (slow), run separately:
#   cd alpha-caddie-web && npm run update:rounds
# then re-run this script.
#
# Usage (from anywhere):
#   powershell -NoProfile -ExecutionPolicy Bypass -File "C:\path\to\golfModel\scripts\update-website-full.ps1"

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$env:GOLF_MODEL_DIR = $repoRoot

$refresh = Join-Path $repoRoot "scripts\refresh_projections_between_rounds.ps1"
if (-not (Test-Path $refresh)) {
    Write-Error "Missing $refresh"
    exit 1
}

Write-Host "==> R pipeline (projections + live-in-play)..." -ForegroundColor Cyan
& powershell -NoProfile -ExecutionPolicy Bypass -File $refresh
if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
}

$web = Join-Path $repoRoot "alpha-caddie-web"
if (-not (Test-Path $web)) {
    Write-Error "alpha-caddie-web not found at $web"
    exit 1
}

$nm = Join-Path $web "node_modules"
if (-not (Test-Path $nm)) {
    Write-Warning "No node_modules in alpha-caddie-web. Run: cd alpha-caddie-web; npm install"
}

Write-Host "==> Node build:history (trends + embed + shots web)..." -ForegroundColor Cyan
Push-Location $web
try {
    $env:GOLF_MODEL_DIR = $repoRoot
    npm run build:history
    if ($LASTEXITCODE -ne 0) {
        Write-Warning "npm run build:history exited $LASTEXITCODE (site may still work if you skip history updates)"
    }
} finally {
    Pop-Location
}

Write-Host "Done. Commit and push paths under alpha-caddie-web/ and website/public/data/ as needed." -ForegroundColor Green
Write-Host "Automate nightly commit+push: scripts\register-nightly-push-task.ps1" -ForegroundColor DarkGray
exit 0
