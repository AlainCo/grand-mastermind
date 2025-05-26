set PROLOG=swipl.exe
set BASE=%~dp0.
setlocal
cd /d "%BASE%\web
call npm run build
if errorlevel 1 pause
endlocal
"%PROLOG%" "%BASE%"\prolog\run.pl
if errorlevel 1 pause