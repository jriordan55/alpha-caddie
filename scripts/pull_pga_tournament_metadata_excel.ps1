# Run pgatouR tournament metadata export without adding R to PATH.
# From anywhere:
#   pwsh -File scripts/pull_pga_tournament_metadata_excel.ps1
# Optional: output .xlsx path, then years (default 5):
#   pwsh -File scripts/pull_pga_tournament_metadata_excel.ps1 "D:\out.xlsx" 5
#
# Override R location: set RSCRIPT_PATH or R_HOME.

param(
  [string] $RepoRoot = "",
  [Parameter(ValueFromRemainingArguments = $true)]
  [string[]] $PassThrough = @()
)

$ErrorActionPreference = "Stop"

if (-not $RepoRoot) {
  $scriptRoot = if ($PSScriptRoot) { $PSScriptRoot } else { Split-Path -Parent $MyInvocation.MyCommand.Path }
  $RepoRoot = Split-Path $scriptRoot -Parent
}

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
      Sort-Object { [version]($_.Name -replace '^R-', '') } -Descending |
      Select-Object -First 1
    if ($latest) {
      $exe = Join-Path $latest.FullName "bin\Rscript.exe"
      if (Test-Path $exe) { return $exe }
    }
  }
  return $null
}

$rscript = Find-Rscript
if (-not $rscript) {
  Write-Error "Rscript.exe not found. Install R or set RSCRIPT_PATH to ...\R-x.x.x\bin\Rscript.exe"
}

$rFile = Join-Path $RepoRoot "scripts\pull_pga_tournament_metadata_excel.R"
if (-not (Test-Path $rFile)) {
  Write-Error "Missing $rFile"
}

Set-Location $RepoRoot
Write-Host "Using: $rscript"
Write-Host "Repo:  $RepoRoot"

$argList = @($rFile) + $PassThrough
& $rscript @argList
