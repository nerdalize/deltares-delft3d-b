@ECHO OFF

REM =====================================
REM Arguments
REM =====================================

REM %1: Top directory of the source tree: used to define SVN_DIR and CMD_DIR
REM %2: Module directory                : svnrevision is executed in this directory
REM %3: Version number file             : containing MAJOR, MINOR and REVISION definitions
REM %4: Input file                      : containing SVN_REVISION to be replaced, normally outputfile.svn (with a double extension)
REM %5: Output file                     : contents of input file with SVN_REVISION replaced by the actual revision string
REM %6: ConfigurationName (optional)    : Name of Visual Studio configuration, to optionally skip version file generation.


REM =====================================
REM Get all directories needed
REM =====================================

SET CURDIR=%CD%
CD %1
SET TOPDIR=%CD%
CD %CURDIR%
CD %2
SET MODDIR=%CD%

SET SVN_DIR=%TOPDIR%\third_party_open\subversion\bin\win32
SET CMD_DIR=%TOPDIR%\third_party_open\commandline\bin\win32
SET VN_DIR=%TOPDIR%\third_party_open\version_number\bin\win32

CD %CURDIR%

REM Skip generation if 6th argument is 1. PRESENT and 2. equal to "Debug" and 3. target file already exists.
IF "%6"=="Debug" (
   IF EXIST "%5" (
      echo %0: Leaving existing file '%5' as is.
      EXIT
   ) ELSE (
      echo %0: Create missing file '%5'.
   )
) ELSE (
   echo %0: Regenerating existing file '%5'.
)

CD "%2"

IF DEFINED BUILD_NUMBER (
   echo %0: build exists
   REM =====================================
   REM BUILD_NUMBER already known
   REM =====================================

) ELSE (

   REM =====================================
   REM Execute svnrevision
   REM =====================================

   set BUILD_NUMBER=000000
   CD "%MODDIR%"
   IF EXIST "%SVN_DIR%\svnversion.exe" (
       echo %0: executing %SVN_DIR%\svnversion.exe -n
       FOR /F "tokens=*" %%i IN ('call "%SVN_DIR%\svnversion.exe" -n "%MODDIR%"') DO set BUILD_NUMBER=%%i 
   )					         
) 

REM ==========================================================================
REM If the source has been obtained using a svn export command, the "exported"
REM string has been generated, but this cannot be used within *.rc files
REM Replace it using 000000 (only necessary on Windows systems)
REM ==========================================================================

set BUILD_NUMBER=%BUILD_NUMBER:~0,12%
IF "%BUILD_NUMBER%" == "exported" (
   SET BUILD_NUMBER=000000
)
echo %0: %BUILD_NUMBER%

REM =====================================
REM Build substitution line
REM =====================================

SET ADDLINE=%BUILD_NUMBER%

                         
                         
REM =====================================
REM Inputfile > Substitute > Outputfile
REM =====================================

CD "%CURDIR%"
echo %0: executing version_number.exe %BUILD_NUMBER% "%3" "%4" "%5"
"%VN_DIR%\version_number.exe" %BUILD_NUMBER% "%3" "%4" "%5"

REM =====================================
REM Clean up
REM =====================================

del /f "%MODDIR%\BUILD_NUMBER" > del.log 2>&1
del /f del.log 



REM =====================================
REM Finished
REM =====================================

REM pause
