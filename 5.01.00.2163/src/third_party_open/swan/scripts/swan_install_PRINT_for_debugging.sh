#!/bin/sh
#
################################################################################
## Example shell script for submitting Delft3D-FLOW/WAVE jobs using           ##
## parallel SWAN on the H4 linuxcluster by means of MPICH2.                   ##
## Note that for NHOSTS=1 the OpenMP version of SWAN will be started.         ##
## Menno.Genseberger@deltares.nl                                              ##
## Adri.Mourits@deltares.nl                                                   ##
## October 2009                                                               ##
################################################################################
#
#
################################################################################
## USAGE examples                                                             ##
################################################################################
#
# be sure that wave uses this swan.bat file
#
#
#
################################################################################
## SETTINGS                                                                   ##
################################################################################
#
## This script only:
debug=0

testpar=$NHOSTS
# testpar=$NSLOTS
if [ ! -z "$testpar" ]; then
  if [ $testpar -gt 1 ]; then
    mpirun=1
  else
    mpirun=0
  fi
else
  mpirun=0
fi

if [ -z "$ARCH" ]; then
  export ARCH=intel
fi
#
#
#
################################################################################
## INITIALIZATION                                                             ##
################################################################################
if [ $mpirun -eq 1 ]; then
  SWANEXEC=${D3D_HOME}/$ARCH/wave/bin/swan_4072ABC_del_l32_i11_mpi.exe
else
  SWANEXEC=${D3D_HOME}/$ARCH/wave/bin/swan_4072ABC_del_l32_i11_omp.exe
  #
  # swan40.72AB and newer runs parallel using OpenMP, using the total number of cores on the machine
  # To force the number of parallel processes, remove the "#" in front of the following line and adjust the number
  # export OMP_NUM_THREADS=1
fi
#
#
#
################################################################################
## DEBUG                                                                      ##
################################################################################
#
if [ $debug -eq 1 ]; then
  echo "=== debug information (start) ==="
  echo SGE_O_WORKDIR: $SGE_O_WORKDIR
  echo HOSTNAME     : $HOSTNAME
  echo NHOSTS       : $NHOSTS
  echo NQUEUES      : $NQUEUES
  echo NSLOTS       : $NSLOTS
  echo PE_HOSTFILE  : $PE_HOSTFILE
  echo D3D_HOME     : $D3D_HOME
  echo PATH         : $PATH
  echo "=== debug information (end) ==="
fi
#
#
#
################################################################################
## RUN                                                                        ##
################################################################################
#
type swan.sh
echo "Using swan executable $SWANEXEC"
echo " "
echo "SWAN batchfile executed for Delft3D"
#
# Check D3D_HOME environment variable
#
ready=0
if [ "${D3D_HOME:-0}" = "0" ]; then
  echo " "
  echo "***ERROR: Delft3D profile not yet executed; can\'t run SWAN"
  read dummy
  ready=1
fi
#
# Check swan.bat argument(s)
#
if [ "${1:-0}" = "0" ]; then
  echo " "
  echo "***ERROR: No argument added to call"
  echo "          Should be \"swan.bat Run_Id\" "
  read dummy
  ready=1
fi
if [ ${ready} -eq 0 ]; then
  echo "Performing computation for: ${1}.swn"
  #
  # Check whether SWAN executable exist
  #
  if [ -x ${SWANEXEC} ]; then
    #
    # Check whether inputfile $1.swn exists
    #
    if [ -f "${1}.swn" ]; then
      #
      # Delete scratch files first
      #
      rm -rf PRINT INPUT swaninit Errfile errpts ${1}.erf ${1}.erp >/dev/null
      #
      # Copy input to INPUT file and run SWAN executable
      #
      cp $1.swn INPUT >/dev/null
      #
      #echo press enter to continue
      #read dummy
      #
      if [ $mpirun -eq 1 ]; then
         echo "Start of parallel computation with MPICH2 using $NSLOTS slots"
         #
         ## General.
         #
         mpirun -np $NSLOTS ${SWANEXEC}

         #
         ## Specific for MPICH2, mpiexec offers more possibilities than mpirun (for
         ## instance combining different executables on different cores and/or nodes).
         ## See also MPICH2 user guide:
         ## http://www.mcs.anl.gov/research/projects/mpich2/documentation/files/mpich2-1.0.8-userguide.pdf
         #
         # mpiexec -machinefile machinefile -n $NSLOTS ${SWANEXEC}
         #
         # Move PRINT file to output file
         #
         slot_number=$NSLOTS
         while [ $slot_number -ge 1 ]
         do
            if [ $slot_number -lt 10 ]; then
               print_filename=PRINT-00$slot_number
            elif [ $slot_number -lt 100 ]; then
               print_filename=PRINT-0$slot_number
            elif [ $slot_number -lt 1000 ]; then
               print_filename=PRINT-$slot_number
            else
               echo Warning: for all slot numbers larger than 999, print files will be moved to PRINT-1000.
               print_filename=PRINT-1000
            fi
            if [ -e $print_filename ]
            then
               mv $print_filename ${1}.prt-$slot_number               
               export printext=$(sed 's/ /_/g' <<< $(date))
	       export printex2=$(sed 's/:/_/g' <<< $printext)
	       cp ${1}.prt-$slot_number ${1}.prt-$slot_number.$printex2
            fi
            slot_number=`expr $slot_number - 1`
         done 
         echo "End of parallel computation using $NSLOTS slots."
         #
      else
         #
         # SWAN run on 1 node.
         #
         ${SWANEXEC}
         #
         export printext=$(sed 's/ /_/g' <<< $(date))
	 export printex2=$(sed 's/:/_/g' <<< $printext)
	 cp PRINT PRINT.$printex2
         #
         # Move PRINT file to output file
         #
         mv PRINT ${1}.prt
      fi
      if [ -f "${1}.src" ]; then
        cp source ${1}.src >/dev/null
      fi
    else
      echo " "
      echo "*** Error: SWAN input file ${1}.swn does not exist"
      echo " "
      read dummy
    fi
  else
    echo " "
    echo "*** ERROR: SWAN executable does not exist"
    echo "           ${SWANEXEC}"
    read dummy
  fi
fi
exit
