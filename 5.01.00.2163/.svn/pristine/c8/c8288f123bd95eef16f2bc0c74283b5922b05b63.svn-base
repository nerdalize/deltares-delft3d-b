@echo off

echo Installing...

rem Change to directory tree where this batch file resides (necessary when oss-install.cmd is called from outside of oss/trunk/src)
cd %~dp0

call scripts_lgpl\win32\oss-install.cmd %1 %2

if NOT %ErrorLevel% EQU 0 (
    exit %ErrorLevel%
)
