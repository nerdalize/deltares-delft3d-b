@echo off

   rem set TCL_EXE=Y:\app\ActiveTcl\bin\tclsh84.exe
set TCL_EXE=..\..\..\third_party_open\tclkit\tclkit.exe

%TCL_EXE% changeprecision.tcl double
