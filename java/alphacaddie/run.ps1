# Run AlphaCaddie without installing Maven globally.
# Downloads Apache Maven once into .mvn-tools\ under this project.
# Requires JDK 8+ on PATH or JAVA_HOME (Spring Boot 2.7).

$ErrorActionPreference = "Stop"
$Root = $PSScriptRoot
$Tools = Join-Path $Root ".mvn-tools"
$MavenVersion = "3.9.9"
$ZipName = "apache-maven-$MavenVersion-bin.zip"
$ZipPath = Join-Path $Tools $ZipName
$MavenHome = Join-Path $Tools "apache-maven-$MavenVersion"
$MvnCmd = Join-Path $MavenHome "bin\mvn.cmd"

function Test-Java8Plus {
    $java = $null
    if ($env:JAVA_HOME -and (Test-Path (Join-Path $env:JAVA_HOME "bin\java.exe"))) {
        $java = Join-Path $env:JAVA_HOME "bin\java.exe"
    } else {
        $j = Get-Command java -ErrorAction SilentlyContinue
        if ($j) { $java = $j.Source }
    }
    if (-not $java) { return $false }
    # java -version writes to stderr; avoid Stop treating it as a terminating error
    $prevEa = $ErrorActionPreference
    $ErrorActionPreference = "Continue"
    $ver = (& $java -version 2>&1 | Out-String).Trim()
    $ErrorActionPreference = $prevEa
    if ($ver -match 'version "1\.8') { return $true }
    if ($ver -match 'version "1\.[0-7]') { return $false }
    # Java 9+ reports "9", "11.0.1", "17.0.2", etc.
    if ($ver -match 'version "(9|[1-9][0-9]+)\.') { return $true }
    return $false
}

if (-not (Test-Java8Plus)) {
    Write-Host "Need JDK 8 or newer on PATH, or set JAVA_HOME to a JDK folder." -ForegroundColor Yellow
    Write-Host "Install: https://adoptium.net/ (Temurin 8 or 11 is fine for this project)"
    exit 1
}

function Get-JavaExe {
    if ($env:JAVA_HOME -and (Test-Path (Join-Path $env:JAVA_HOME "bin\java.exe"))) {
        return (Join-Path $env:JAVA_HOME "bin\java.exe")
    }
    $j = Get-Command java -ErrorAction SilentlyContinue
    if ($j) { return $j.Source }
    return $null
}

$javaExe = Get-JavaExe
$javaBin = if ($javaExe) { Split-Path $javaExe } else { $null }
$javacExe = if ($javaBin) { Join-Path $javaBin "javac.exe" } else { $null }
if (-not $javacExe -or -not (Test-Path $javacExe)) {
    Write-Host "Maven needs a JDK (folder must contain bin\javac.exe), not a JRE-only install." -ForegroundColor Yellow
    Write-Host "Install Temurin JDK 8+ from https://adoptium.net/ and set JAVA_HOME to that JDK, or put JDK\bin first on PATH."
    exit 1
}

if (-not (Test-Path $MvnCmd)) {
    Write-Host "Downloading Apache Maven $MavenVersion (one-time)..."
    New-Item -ItemType Directory -Force -Path $Tools | Out-Null
    $url = "https://repo.maven.apache.org/maven2/org/apache/maven/apache-maven/$MavenVersion/$ZipName"
    Invoke-WebRequest -Uri $url -OutFile $ZipPath -UseBasicParsing
    Expand-Archive -Path $ZipPath -DestinationPath $Tools -Force
}

Push-Location $Root
try {
    & $MvnCmd @args
} finally {
    Pop-Location
}
