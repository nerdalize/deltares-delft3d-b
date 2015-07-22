@ echo off

   rem set tcl="c:\Program Files\tcl\bin\tclsh85.exe"
set tcl=tclsh

set scriptname=../../../bin/win32/flow2d3d/scripts/mormerge.tcl

%tcl% %scriptname% -i basin_windows.mm -s %scriptname%

pause
