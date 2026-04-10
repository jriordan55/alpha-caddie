# Rebuild projections + export JSON for the web (run after each round or on a schedule).
# Finds Rscript.exe even when R is not on PATH (registry + Program Files\R\R-*).
#
# Manual:
#   powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\refresh_projections_between_rounds.ps1
#
# Automatic (between rounds): Task Scheduler → Create Task → Triggers → e.g. Weekly, Thu–Sun,
#   repeat every 1 hour for 24 hours (or set hourly during tournament weeks). Action:
#   Program: powershell.exe
#   Arguments: -NoProfile -ExecutionPolicy Bypass -File "C:\Users\student\Documents\golfModel\scripts\refresh_projections_between_rounds.ps1"
#
# Round selection: omit GOLF_NEXT_ROUND / golf_next_round.txt and round_projections.R uses
# Eastern Time + 9pm cutover (Thu→R1, Fri R1/R2, etc.). Override: $env:GOLF_NEXT_ROUND = "3"
#
# Optional: $env:RSCRIPT_PATH = "C:\Program Files\R\R-4.4.2\bin\Rscript.exe"
# DataGolf: set $env:DATAGOLF_API_KEY or copy alpha-caddie-web\datagolf.local.example.json
#   to alpha-caddie-web\datagolf.local.json with your apiKey (Scratch Plus required for API).

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$logDir = Join-Path $repoRoot "logs"
$logFile = Join-Path $logDir "refresh_projections.log"

if (-not (Test-Path $logDir)) {
    New-Item -ItemType Directory -Path $logDir | Out-Null
}

function Get-RscriptPath {
    $override = [Environment]::GetEnvironmentVariable("RSCRIPT_PATH", "Process")
    if ($override -and (Test-Path $override)) { return $override }

    $cmd = Get-Command Rscript.exe -ErrorAction SilentlyContinue
    if ($cmd) { return $cmd.Source }

    $regKeys = @(
        "HKLM:\SOFTWARE\R-core\R",
        "HKCU:\SOFTWARE\R-core\R",
        "HKLM:\SOFTWARE\WOW6432Node\R-core\R"
    )
    foreach ($key in $regKeys) {
        $item = Get-ItemProperty -Path $key -ErrorAction SilentlyContinue
        if ($item -and $item.InstallPath) {
            $candidate = Join-Path $item.InstallPath "bin\Rscript.exe"
            if (Test-Path $candidate) { return $candidate }
        }
    }

    $rRoot = Join-Path ${env:ProgramFiles} "R"
    if (Test-Path $rRoot) {
        $dirs = Get-ChildItem -Path $rRoot -Directory -ErrorAction SilentlyContinue |
            Where-Object { $_.Name -match '^R-' } |
            Sort-Object { $_.Name } -Descending
        foreach ($d in $dirs) {
            $candidate = Join-Path $d.FullName "bin\Rscript.exe"
            if (Test-Path $candidate) { return $candidate }
        }
    }

    $candidates = @(
        "C:\Program Files\R\R-4.5.0\bin\Rscript.exe",
        "C:\Program Files\R\R-4.4.2\bin\Rscript.exe",
        "C:\Program Files\R\R-4.4.1\bin\Rscript.exe",
        "C:\Program Files\R\R-4.4.0\bin\Rscript.exe",
        "C:\Program Files\R\R-4.3.3\bin\Rscript.exe"
    )
    foreach ($path in $candidates) {
        if (Test-Path $path) { return $path }
    }

    throw @"
Rscript.exe not found.
Install R from https://cran.r-project.org/ or set RSCRIPT_PATH to the full path of Rscript.exe
(e.g. C:\Program Files\R\R-4.4.2\bin\Rscript.exe).
"@
}

$rscript = Get-RscriptPath
$env:GOLF_MODEL_DIR = $repoRoot
if (-not $env:GOLF_RAW_PROJECTIONS) {
    $env:GOLF_RAW_PROJECTIONS = "0"
}

# Run R without piping stderr through PowerShell (R writes messages/warnings to stderr;
# 2>&1 becomes ErrorRecord and can abort the script as NativeCommandError).
function Invoke-RscriptFile {
    param(
        [Parameter(Mandatory = $true)][string] $RscriptExe,
        [Parameter(Mandatory = $true)][string] $ScriptPath,
        [Parameter(Mandatory = $true)][string] $WorkingDirectory,
        [Parameter(Mandatory = $true)][string] $LogPath
    )
    $outF = Join-Path $env:TEMP ("r_stdout_{0}.txt" -f [Guid]::NewGuid().ToString("n"))
    $errF = Join-Path $env:TEMP ("r_stderr_{0}.txt" -f [Guid]::NewGuid().ToString("n"))
    try {
        $p = Start-Process -FilePath $RscriptExe -ArgumentList @($ScriptPath) `
            -WorkingDirectory $WorkingDirectory -Wait -PassThru `
            -NoNewWindow -RedirectStandardOutput $outF -RedirectStandardError $errF
        foreach ($f in @($outF, $errF)) {
            if (Test-Path $f) {
                Get-Content -LiteralPath $f -Encoding UTF8 -ErrorAction SilentlyContinue | ForEach-Object {
                    Add-Content -LiteralPath $LogPath -Value $_ -Encoding UTF8
                    Write-Host $_
                }
            }
        }
        $code = $p.ExitCode
        if ($null -eq $code) { $code = -1 }
        return [int]$code
    }
    finally {
        Remove-Item -LiteralPath $outF, $errF -ErrorAction SilentlyContinue
    }
}

$stamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
Add-Content -Path $logFile -Value ""
Add-Content -Path $logFile -Value "===== $stamp (Rscript=$rscript) ====="

$rp = Join-Path $repoRoot "round_projections.R"
$ex = Join-Path $repoRoot "scripts\export_projections_for_website.R"

$code1 = Invoke-RscriptFile -RscriptExe $rscript -ScriptPath $rp -WorkingDirectory $repoRoot -LogPath $logFile
if ($code1 -ne 0) {
    Add-Content -Path $logFile -Value "ERROR: round_projections.R exit code $code1"
    exit $code1
}

$code2 = Invoke-RscriptFile -RscriptExe $rscript -ScriptPath $ex -WorkingDirectory $repoRoot -LogPath $logFile
if ($code2 -ne 0) {
    Add-Content -Path $logFile -Value "ERROR: export_projections_for_website.R exit code $code2"
    exit $code2
}

# Writes alpha-caddie-web/live-in-play.json so the web app can merge DataGolf placement probs (win/top10/…)
# into players on load; without this, +EV can show huge fake EV vs updated books.
$fetchLive = Join-Path $repoRoot "scripts\fetch_datagolf_in_play.R"
if (Test-Path $fetchLive) {
    $code3 = Invoke-RscriptFile -RscriptExe $rscript -ScriptPath $fetchLive -WorkingDirectory $repoRoot -LogPath $logFile
    if ($code3 -ne 0) {
        Add-Content -Path $logFile -Value "WARNING: fetch_datagolf_in_play.R exit code $code3 (optional; +EV may lag books until this succeeds)"
    }
}

Add-Content -Path $logFile -Value "===== done $(Get-Date -Format s) ====="
exit 0
