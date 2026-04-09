@echo off
REM Double-click this file, OR run from cmd:  scripts\register_monday_task.bat
cd /d "%~dp0.."
echo Current folder: %CD%
echo.
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0register_monday_republish_task.ps1"
echo.
pause
