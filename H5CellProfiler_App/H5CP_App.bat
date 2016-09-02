:: Windows Batch file to run the H5CP_App_Rscript.R file.
:: First looks if Rscript is in PATH, if not we add it by looking it up in the registry.
:: http://stackoverflow.com/a/34749932/1439843

@echo off
setlocal enableextensions

where /q Rscript.exe
IF NOT ERRORLEVEL 1 (
	ECHO Rscript found on PATH, continuing...
	goto :rscript_found
)

ECHO Rscript.exe not found on PATH, looking up R installation in registry...

SET RKEY=
SET RPATH=
FOR /F "tokens=* skip=2" %%L IN ('reg.exe QUERY HKLM\Software\R-core\R /f * /k ^| sort') DO (
    IF NOT "%%~L"=="" SET "RKEY=%%~L"
)
IF NOT DEFINED RKEY (
    ECHO Unable to query registry key HKLM\Software\R-core\R
    pause
    EXIT /B 1

)
FOR /F "tokens=2* skip=2" %%A IN ('REG QUERY %RKEY% /v "installPath"') DO (
    IF NOT "%%~B"=="" SET "RPATH=%%~B"
)
IF NOT DEFINED RPATH (
    ECHO Unable to query registry value %RKEY%\installPath
    pause
    EXIT /B 2
)
IF NOT EXIST "%RPATH%" (
    ECHO Found path for R (%RPATH%^) does not exist
    pause
    EXIT /B 3
)

IF "%PROCESSOR_ARCHITECTURE%"=="AMD64" (
    SET "PATH=%RPATH%\bin\x64;%PATH%"
    ECHO Found %RPATH%\bin\x64
) ELSE (
    SET "PATH=%RPATH%\bin\i386;%PATH%"
    ECHO Found %RPATH%\bin\i386
)

ECHO Added Rscript to PATH, continuing...

:rscript_found

Rscript H5CP_App_Rscript.R

pause
