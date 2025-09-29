@echo off
REM scripts\run.cmd — Windows launcher (prefers Rterm for interactive input)
setlocal enabledelayedexpansion

set "SCRIPT_DIR=%~dp0"
set "ROOT_DIR=%SCRIPT_DIR%.."

set "ENGINE="
REM 1) Prefer Rterm (interactive)
for %%P in ("C:\Program Files\R") do (
  if exist "%%~fP" (
    for /f "delims=" %%D in ('dir /b /ad "%%~fP\R-*"' ) do (
      if exist "%%~fP\%%D\bin\Rterm.exe"  set "ENGINE=%%~fP\%%D\bin\Rterm.exe"
      if exist "%%~fP\%%D\bin\x64\Rterm.exe" set "ENGINE=%%~fP\%%D\bin\x64\Rterm.exe"
    )
  )
)

REM 2) If no Rterm found, try Rscript on PATH
if not defined ENGINE (
  where Rscript >nul 2>nul && set "ENGINE=Rscript"
)

if not defined ENGINE (
  echo Error: Could not find Rterm.exe or Rscript. Install R 4.5.1 or set RSCRIPT/RTERM paths. >&2
  exit /b 9009
)

if not exist "%ROOT_DIR%\app\main.R" (
  echo Error: "%ROOT_DIR%\app\main.R" not found. >&2
  exit /b 1
)

pushd "%ROOT_DIR%"
echo Starting app with "%ENGINE%"
if /i "%ENGINE%"=="Rscript" (
  "%ENGINE%" --vanilla app\main.R
) else (
  "%ENGINE%" --vanilla -f app\main.R
)
set "STATUS=%ERRORLEVEL%"
popd

if "%STATUS%"=="0" ( echo OK. ) else ( echo Failed with status %STATUS%. )
exit /b %STATUS%
