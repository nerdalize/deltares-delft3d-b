#!/bin/bash
#$ -V
#$ -j yes

set NHOSTS manually here:
export NHOSTS=3
NPART=$NHOSTS
    #
    # This script starts a single-domain Delft3D-FLOW computation on Linux in parallel mode
    # asuming nodes are allocated manually
    #
    # Usage example:
    # Create a file named machinefile as described below
    # Execute this script
    #
    # In case the error "unable to find mpd.conf" occurs:
    # Your home directory MUST contain file .mpd.conf with contents:
    # secretword=bla
    # and with file access permissions:
    # -r--------
    # 


    #
    # Specify the config file to be used here
    # 
argfile=config_flow2d3d.ini





    #
    # Set the directory containing delftflow.exe here
    #
export ARCH=intel
export D3D_HOME=../../bin/lnx
exedir=$D3D_HOME/flow2d3d/bin
libdir=$D3D_HOME/flow2d3d/lib
 
    #
    # No adaptions needed below
    #

    # Set some (environment) parameters
export LD_LIBRARY_PATH=$exedir:$libdir:$LD_LIBRARY_PATH 



    ### Specific setting for H3/H4 linuxclusters, needed for MPICH2
    ### commands (mpdboot, mpirun, mpiexed, mpdallexit etc.).
export PATH="/opt/mpich2/bin:${PATH}"
 
    ### The file "machinefile" is assumed to be created manually and should
    ### look like:
    ### machinename1:2
    ### machinename2:2
    ### machinename3:2

echo Contents of machinefile:
cat $(pwd)/machinefile
echo ----------------------------------------------------------------------





    # Run

    ### command="d3d.run -nproc "$NHOSTS" -input "$inputfile" -back no
    ### eval $command

    ### General for MPICH2, startup your MPICH2 communication network (you
    ### can check if it is already there with mpdtrace).
    ###
    ### Optionally:
    ### Add option --rsh=/usr/bin/rsh to mpdboot
    ### This is needed when the following error appears:
    ### mpdboot_xh5000 (handle_mpd_output 420): from mpd on xh5001, invalid port info: no_port
mpd &
mpdboot -n $NHOSTS -f $(pwd)/machinefile --ncpus=2

# link mpich debug rubbish to /dev/null
node_number=$NPART
while test $node_number -ge 1
do
   node_number=`expr $node_number - 1`
   ln -s /dev/null log$node_number.irlog
done

    ### General, start delftflow in parallel by means of mpirun.
    ### The machines in the h4 cluster are dual core; start 2*NHOSTS parallel processes
mpirun -np $NHOSTS $exedir/deltares_hydro.exe $argfile
    ### alternatives:
    ### mpiexec -n $DELTAQ_NumNodes delftflow_91.exe -r $inputfile.mdf
    ### mpiexec -n `expr $DELTAQ_NumNodes \* 2` $exedir/deltares_hydro.exe $argfile


rm -f log*.irlog

    ### General for MPICH2, finish your MPICH2 communication network.
mpdallexit 

