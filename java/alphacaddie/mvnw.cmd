@REM Maven Wrapper (Windows) — run: .\mvnw.cmd spring-boot:run
@REM Requires JAVA_HOME pointing to JDK 17+ (Spring Boot 3)
@echo off
setlocal
title mvnw
if "%JAVA_HOME%"=="" (
  echo ERROR: JAVA_HOME is not set.
  echo Install JDK 17+ from https://adoptium.net/ then:
  echo   setx JAVA_HOME "C:\Program Files\Eclipse Adoptium\jdk-17.x.x-hotspot"
  echo Close and reopen PowerShell, then run this script again.
  exit /b 1
)
if not exist "%JAVA_HOME%\bin\java.exe" (
  echo ERROR: JAVA_HOME does not point to a JDK: %JAVA_HOME%
  exit /b 1
)

set "MAVEN_PROJECTBASEDIR=%~dp0"
if "%MAVEN_PROJECTBASEDIR:~-1%"=="\" set "MAVEN_PROJECTBASEDIR=%MAVEN_PROJECTBASEDIR:~0,-1%"

set "WRAPPER_JAR=%MAVEN_PROJECTBASEDIR%\.mvn\wrapper\maven-wrapper.jar"
if not exist "%WRAPPER_JAR%" (
  echo Downloading maven-wrapper.jar ...
  powershell -NoProfile -ExecutionPolicy Bypass -Command ^
    "[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12; $u='https://repo.maven.apache.org/maven2/org/apache/maven/wrapper/maven-wrapper/3.3.2/maven-wrapper-3.3.2.jar'; $d='%MAVEN_PROJECTBASEDIR%\.mvn\wrapper'; New-Item -ItemType Directory -Force -Path $d | Out-Null; Invoke-WebRequest -Uri $u -OutFile '%WRAPPER_JAR%' -UseBasicParsing"
  if not exist "%WRAPPER_JAR%" (
    echo Failed to download wrapper jar. Use .\run.ps1 instead.
    exit /b 1
  )
)

"%JAVA_HOME%\bin\java.exe" -classpath "%WRAPPER_JAR%" "-Dmaven.multiModuleProjectDirectory=%MAVEN_PROJECTBASEDIR%" org.apache.maven.wrapper.MavenWrapperMain %*
exit /b %ERRORLEVEL%
