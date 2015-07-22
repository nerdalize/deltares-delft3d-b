@echo off
rem Program will replace %1 by the %1.svn and will replace VERSION_BUILD_NUMBER by a corresponding svn version using svnversion command
rem
rem %1 - path to the target source file
rem %2 - path to the folder to be used to check svnversion
rem %3 - Single file with version number information version_number.ini
rem %4 - --onlyifmissing: only regenerate when target source file does not exist (optional, default: off)

echo Generating version number in '%1' ...

set SCRIPT_DIRECTORY=%~dp0

set SED=%SCRIPT_DIRECTORY%\..\..\third_party_open\commandline\bin\win32\sed.exe
set SVNVERSION=%SCRIPT_DIRECTORY%\..\..\third_party_open\subversion\bin\win32\svnversion.exe
set VN=%SCRIPT_DIRECTORY%\..\..\third_party_open\version_number\bin\win32\version_number.exe

IF DEFINED BUILD_NUMBER (
	set version=%BUILD_NUMBER%
) ELSE (
        set version=000000
	rem Obtain the svn version number 
	"%SVNVERSION%" %2 | "%SED%" "s/\(.*\)/set version=\1/" > setversion.bat
 	call setversion.bat & del setversion.bat > NUL
)

rem ==========================================================================
rem If the source has been obtained using a svn export command, the "Unversioned directory"
rem string has been generated, but this cannot be used within *.rc files
rem Replace it using 000000 (only necessary on Windows systems)
rem ==========================================================================

IF "%version:~0,11%" == "Unversioned" (
   set version=000000
)

IF "%4" == "--onlyifmissing" (
   IF EXIST "%1" (
      echo %0: Leaving existing file '%1' as is.
      goto end
   ) ELSE (
      echo %0: Create missing file '%1'.
   )
) ELSE (
   echo %0: Regenerating existing file '%1'.
)


if exist %1 (
	del %1
)

rem Generate version number source module using version_number.exe
"%VN%" "%version%" "%3" "%1.svn" "%1"

:end
