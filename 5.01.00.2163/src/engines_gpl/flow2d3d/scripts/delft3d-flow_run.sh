#!/bin/sh

export DHSDELFT_LICENSE_FILE=/app/delft3d/D3dnew.lic

# ============ set runid
runid=b01

# ============ set exedir
# === testbank executables:
#D3D_HOME=/p/delft3d/test_data/d3d-flow/flow/versions/3.48.03.00
#exedir=$D3D_HOME/$ARCH/flow/bin

# === local    executables:
D3D_HOME=/u/mourits/delft3d
exedir=$D3D_HOME/code/modules/flow/bin/wlinux

export D3D_HOME

echo ===================================================================
echo === runid  = $runid
echo === exedir = $exedir
echo ===================================================================

# ============ remove output files
rm -f runid
rm -f TMP*.*
rm -f *.msg
rm -f com*.*
rm -f fourier*.*
rm -f td-diag*.*
rm -f tri-diag.*
rm -f md-diag*.*
rm -f tri-prt.*

echo === start tdatom.exe ===
$exedir/tdatom.exe -r $runid
echo === end tdatom.exe ===

echo === start md-ver.exe ===
$exedir/md-ver.exe -r $runid
echo === end md-ver.exe ===

echo press enter to continue
read dummy
 
echo === start tdatom.exe ===
$exedir/tdatom.exe -r $runid
echo === end tdatom.exe ===

echo === start trisim.exe ===
$exedir/trisim.exe -r $runid
echo === end trisim.exe ===

