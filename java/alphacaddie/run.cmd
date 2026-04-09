@echo off
REM Double-click or: run.cmd spring-boot:run
REM Uses Bypass so you don't need to change PowerShell execution policy.
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0run.ps1" %*
