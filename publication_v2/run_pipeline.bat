@echo off
REM ===========================================================================
REM  run_pipeline.bat - double-click launcher for the publication_v2 pipeline.
REM  Optional argument: --fast   (skip the Bayesian MCMC steps)
REM ===========================================================================
setlocal
cd /d "%~dp0"

set "RSCRIPT="
where Rscript >nul 2>nul && set "RSCRIPT=Rscript"
if not defined RSCRIPT if exist "D:\tools\R\R-4.5.2\bin\Rscript.exe" set "RSCRIPT=D:\tools\R\R-4.5.2\bin\Rscript.exe"
if not defined RSCRIPT if exist "C:\Program Files\R\R-4.5.2\bin\Rscript.exe" set "RSCRIPT=C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
if not defined RSCRIPT if exist "C:\Program Files\R\R-4.3.0\bin\Rscript.exe" set "RSCRIPT=C:\Program Files\R\R-4.3.0\bin\Rscript.exe"

if not defined RSCRIPT (
  echo [ERROR] Rscript.exe not found.
  echo         Install R 4.5.x or add its bin folder to PATH.
  pause
  exit /b 1
)

echo Using: %RSCRIPT%
"%RSCRIPT%" run_pipeline.R %*
pause
