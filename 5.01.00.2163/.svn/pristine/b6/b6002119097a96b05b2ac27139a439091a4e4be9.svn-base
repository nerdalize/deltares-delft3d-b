#! /bin/sh

#-------------------------------------------------------------------------------
#   Script to run a multi-node DD simulation on the Deltares Linux cluster.
#   Usage: qsub -pe distrib <numnodes> runddq.sh <ddbounds-file>
#
#   Irv.Elshoff@Deltares.NL
#   18 nov 11
#-------------------------------------------------------------------------------


. /opt/sge/InitSGE

ddbounds="$1"
d3dHome='/path/to/d_hydro_exe_and_sos'

if [ ! -f "$ddbounds" ]; then
    echo "ABORT: DD bounds file \"$ddbounds\" not found"
    exit 1
fi

if [ ! -d "$d3dHome" ]; then
    echo "ABORT: D3D home directory \"$d3dHome\" not found"
    exit 1
fi

LD_LIBRARY_PATH="$d3dHome"

echo $DELTAQ_NodeList | tr ' ' '\n' | sed 's/.deltares.nl//' > machines

cat > d_hydro.xml <<!!EOF
<?xml version="1.0" encoding="iso-8859-1"?>
    <DeltaresHydro start="Flow2D3D">
        <CrashOnAbort/>
        <Flow2D3D library='$d3dHome/libflow2d3d.so'>
            <DomainDecomposition>
                <DDBounds file='$ddbounds' />
                <MultiNode file='machines' />
            </DomainDecomposition>
        </Flow2D3D>
    </DeltaresHydro>
!!EOF

ulimit -c unlimited

command="$d3dHome/d_hydro.exe d_hydro.xml"
echo $command
eval $command
