@ echo off

rem ============ set runid
set runid=b01

rem ============ set exedir
rem === testbank executables: 
rem set exedir=p:\delft3d\test_data\d3d-flow\flow\versions\3.50.03.00\w32\flow\bin

rem === local    executables: 
set exedir=d:\delft3d\code\modules\flow\bin\win32

rem ============ set SHLVL
rem only when md-ver/trisim should not wait for user interaction in case of errors
set SHLVL=0
rem set SHLVL=1

echo =================================================================== 
echo === runid  = %runid%
echo === exedir = %exedir%
IF %SHLVL% == 1 (
   echo === Don't wait for user interaction in case of errors
)
echo ===================================================================


rem pause

rem ============ remove output files
del /f runid       > del.log 2>&1
del /f TMP*.*      > del.log 2>&1
del /f *.msg       > del.log 2>&1
del /f com*.*      > del.log 2>&1
del /f fourier*.*  > del.log 2>&1
del /f td-diag*.*  > del.log 2>&1
del /f tri-diag.*  > del.log 2>&1
del /f md-diag*.*  > del.log 2>&1
del /f tri-prt.*   > del.log 2>&1
del /f del.log

echo === start tdatom.exe ===
%exedir%\tdatom.exe -r %runid%
echo === end tdatom.exe ===

echo === start md-ver.exe ===
%exedir%\md-ver.exe -r %runid%
echo === end md-ver.exe ===

pause

echo === start tdatom.exe ===
%exedir%\tdatom.exe -r %runid%
echo === end tdatom.exe ===

echo === start trisim.exe ===
%exedir%\trisim.exe -r %runid%
echo === end trisim.exe ===

pause
