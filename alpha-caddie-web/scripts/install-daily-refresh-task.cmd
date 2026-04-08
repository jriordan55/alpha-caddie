@echo off
REM Bypasses execution policy for this run only (no system-wide change).
cd /d "%~dp0"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0install-daily-refresh-task.ps1" %*
