$ErrorActionPreference = "Stop"
$PSNativeCommandUseErrorActionPreference = $false

$repoRoot = Split-Path -Parent $PSScriptRoot
# So round_projections.R / app.R resolve paths the same as interactive use
$env:GOLF_MODEL_DIR = $repoRoot
$scriptPath = Join-Path $repoRoot "scripts\monday_republish.R"
$logDir = Join-Path $repoRoot "logs"
$logFile = Join-Path $logDir "monday_republish.log"

if (-not (Test-Path $logDir)) {
    New-Item -ItemType Directory -Path $logDir | Out-Null
}

function Get-RscriptPath {
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

    $candidates = @(
        "C:\Program Files\R\R-4.4.1\bin\Rscript.exe",
        "C:\Program Files\R\R-4.4.0\bin\Rscript.exe",
        "C:\Program Files\R\R-4.3.3\bin\Rscript.exe",
        "C:\Program Files\R\R-4.3.2\bin\Rscript.exe",
        "C:\Program Files\R\R-4.3.1\bin\Rscript.exe",
        "C:\Program Files\R\R-4.3.0\bin\Rscript.exe",
        "C:\Program Files\R\R-4.2.3\bin\Rscript.exe",
        "C:\Program Files\R\R-4.2.0\bin\Rscript.exe"
    )
    foreach ($path in $candidates) {
        if (Test-Path $path) { return $path }
    }

    throw "Rscript.exe not found. Add R to PATH or edit scripts/run_monday_republish.ps1."
}

$rscript = Get-RscriptPath

"[$(Get-Date -Format s)] Starting Monday republish task" | Add-Content $logFile
& $rscript $scriptPath 2>&1 | Tee-Object -FilePath $logFile -Append | Out-Host
$exitCode = $LASTEXITCODE
"[$(Get-Date -Format s)] Finished with exit code $exitCode" | Add-Content $logFile

exit $exitCode
