#! /bin/bash

mkdir -p bin/intel

cp tclkit tclkit2

tclkit sdx.kit qwrap deltares_hydro.tcl
tclkit sdx.kit unwrap deltares_hydro.kit
tclkit sdx.kit wrap bin/intel/deltares_hydro.exe -runtime tclkit2

rm -f tclkit2
rm -f deltares_hydro.kit
rm -rf deltares_hydro.vfs
