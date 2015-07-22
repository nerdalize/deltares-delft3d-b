@echo off

if not exist bin\win32     mkdir bin\win32
del bin\win32\deltares_hydro.exe

copy tclkit.exe tclkit2.exe
tclkit sdx.kit qwrap ../../scripts_lgpl/deltares_hydro/deltares_hydro.tcl
tclkit sdx.kit unwrap deltares_hydro.kit
tclkit sdx.kit wrap bin\win32\deltares_hydro.exe -runtime tclkit2.exe

del tclkit2.exe
del deltares_hydro.kit
rmdir /S /Q deltares_hydro.vfs
