#! /bin/sh

#-------------------------------------------------------------------------------
#   Delft3D-FLOW RunDD Script
#
#   This script prepares and runs trisim.exe for simulations
#   with domain decomposition.
#   See http://wwl.deltares.nl/~elshoff/d3d/rundd.html .
#
#   ToDo: MPICH2 + OpenPBS
#       mpdboot -n `wc -l < $PBS_NODEFILE` -f $PBS_NODEFILE
#       make sure ssh login is possible (host key in known_hosts)
#           /part0/diskless/slave/root/etc/ssh/ssh_host_rsa_key.pub
#
#   Irv.Elshoff@deltares.nl
#   09 jan 07
#-------------------------------------------------------------------------------


Usage () {
    cat <<EOF
Usage: rundd [options] <ddboundsfile> [-- hydraoptions]
    Command-line options:
        -a          Do TDATOM phase only
        -A          Skip TDATOM phase
        -C          Just clean up output files; do not run simulation
        -d <level>  Set debug level
        -L <file>   Write log output to combined file
        -l <file>   Write log output to iterator-specific files
        -M <hosts>  Run multi-process on the listed hosts
        -m <num>    Run multi-process num times on the local host
        -O          Run in online-mode
        -q          Run multi-node in OpenPBS/Torque batch queue
        -n          Do not execute anything; just print commands
        -P          Run in MPI mode
        -v          Run valgrind
        -x <dir>    Use executables in specified directory
        -?          This help
    All other command-line arguments are passed to trisim.exe (Hydra)
EOF
    }

MPICommand="
    /opt/mpich2/bin/mpirun
    "

ValgrindCommand="
    valgrind
        --tool=memcheck
        --trace-children=yes
        --verbose
        --time-stamp=yes
        --log-fd=1
        --show-below-main=yes
        --num-callers=50
        --error-limit=no
        "

RunMorflow () {

    #-----  Initialize variables
    
    BatchJob=0
    CleanupOnly=0
    Execute=1
    ExeDir="$D3D_HOME/bin/$D3D_PLATFORM"
    HydraArguments=""
    HydraConfig=""
    HydraDebug=""
    HydraLog=""
    InputFile=""
    MPICmd=""
    MPIMode=0
    NodeCount=0
    SkipTDATOM=0
    SkipTRISIM=0
    ValgrindCmd=""

    if [ "$D3D_HOME" == "" -o ! -d "$D3D_HOME" ]; then
        echo "Warning: D3D_HOME was not defined; is set to /tmp"
        export D3D_HOME="/tmp"
        ExeDir=""
    fi

    #-----  Process command-line arguments

    while [ $# -gt 0 ]; do
        case "$1" in
            -a)
                SkipTRISIM=1
                ;;
            -A)
                SkipTDATOM=1
                ;;
            -C)
                CleanupOnly=1
                ;;
            -d)
                HydraDebug="-d $2"
                shift
                ;;
            -l|-L)
                HydraLog="$1 $2"
                shift
                ;;
            -m)
                HydraMachines="-M localhost"
                count="$2"
                shift
                NodeCount="$count"
                while [ $count -gt 1 ]; do
                    HydraMachines="$HydraMachines,localhost"
                    (( count -= 1 ))
                done
                ;;
            -M)
                HydraMachines="-M $2"
                shift
                ;;
            -n)
                Execute=0
                ;;
            -q)
                BatchJob=1
                ;;
            -O)
                HydraArguments="$HydraArguments -O $2"
                shift
                ;;
            -P)
                MPIMode=1
                ;;
            -v)
                ValgrindCmd="$ValgrindCommand"
                ;;
            -x)
                ExeDir="$2"
                shift
                ;;
            -\?)
                Usage
                exit 0
                ;;
            --)
                shift
                break
                ;;
            *)
                if [ "$inputfile" = "" ]; then
                    inputfile=`basename $1`
                    if [ ! -r $inputfile ]; then
                        echo "Cannot read input file \"$inputfile\""
                        exit 1
                    fi
                else
                    echo "Too many input files specified; only one allowed"
                    Usage
                    exit 1
                fi
                ;;
        esac
        shift
    done

    if [ ! -d $ExeDir ]; then
        echo "Executable directory ($ExeDir) does not exist"
        exit 1
    fi

    HydraArguments="$HydraArguments $*"
    
    #-----  Get list of nodes for batch job
    
    if [ $BatchJob -eq 1 ]; then
        if [ "$HydraMachines" != "" ]; then
            echo "Options -q and -m/-M may not be used together"
            exit 1
        fi
        nodes=`tr '\n' ',' < $PBS_NODEFILE | sed 's/,$//'`
        HydraMachines="-M $nodes"
    fi

    #-----  Prepare run command for MPI mode

    if [ $MPIMode -eq 1 ]; then
        if [ ! -x "$MPICommand" ]; then
            MPICommand="mpirun"
        fi

        if [ $NodeCount -gt 0 ]; then
            MPICmd="$MPICommand -np $NodeCount"
            HydraMachines=""
            HydraArguments="-P $HydraArguments"

        elif [ "$HydraMachines" == "" ]; then
            MPICmd="$MPICommand -np $NodeCount"
            HydraArguments="-P $HydraArguments"

        else
            echo "Improper combinations of arguments for MPI run"
            exit 1
        fi
        echo "MPICmd=\"$MPICmd\""
    fi

    #-----  Determine if the run is single domain or DD, set list of
    #       subdomains and make sure they exist.

    case "$inputfile" in
        *.mdf)
            if [ "$HydraMachines" != "" ]; then
                echo "Cannot use multi-processor options in a single-domain run"
                exit 1
            fi
            Subdomains=`echo $inputfile | sed 's/.mdf$//' | sed 's/.MDF$//'`
            HydraArguments=" \
                $HydraLog \
                $HydraDebug \
                $HydraArguments \
                "
            ;;

        '')
            if [ $CleanupOnly -eq 0 ]; then
                echo "No input file specified (type \"rundd -?\" for help)"
                exit 1
            fi
            ;;

        *)
            Subdomains=`ParseDDBoundsFile $inputfile`
            if [ "$Subdomains" = "" ]; then
                echo "No subdomains could be found in DD-bounds file \"$inputfile\""
                exit 1
            fi
#           for runid in $Subdomains; do
#               if [ ! -r "$runid.mdf" ]; then
#                   echo "MD-file for subdomain \"$runid\" not found"
#                   exit 1
#               fi
#           done
            HydraArguments=" \
                -c $inputfile \
                $HydraLog \
                $HydraDebug \
                $HydraMachines \
                $HydraArguments \
                "
            ;;
    esac

    #-----  Prepare single or multi-domain simulation

    if [ $Execute -eq 1 -a $SkipTDATOM -eq 0 ]; then
        CleanupFiles
    fi
    if [ $CleanupOnly -eq 1 ]; then
        exit
    fi

    ulimit -c unlimited

    if [ "$LD_LIBRARY_PATH" == "" ]; then
        export LD_LIBRARY_PATH="/tmp"
    fi
    if [ "$DHSDELFT_LICENSE_FILE" == "" ]; then
        export DHSDELFT_LICENSE_FILE="/f/wlhost.WL/library/app/flexlm/ds_flex:/app/delft3d" 
    fi

    if [ $SkipTDATOM -eq 0 ]; then
        for runid in $Subdomains; do
            RunCommand "echo $runid > runid"
            RunCommand "$ExeDir/tdatom.exe"
        done
    fi
 
    #-----  Run Hydra Morflow simulation

     if [ $SkipTRISIM -eq 0 ]; then
         RunCommand " \
            time \
            $ValgrindCmd \
            $MPICmd \
            $ExeDir/trisim.exe $HydraArguments \
            "
    fi
    }


RunCommand () {
    cmd=`echo $*`
    echo "$cmd"
    if [ $Execute -eq 1 ]; then
        eval "$cmd"
        result=$?
        echo "  ==> `echo $cmd | awk '{print $1}'` returns $result"
    fi
    return $result
    }


ParseDDBoundsFile () {
    # Extract the names of the subdomains from a DD-bounds file
    cat "$1" |
        egrep -v '^#' |
            egrep -v '^[ \t]*$' |
                awk '{print $1 "\n" $6}' |
                    sed 's/\w+//g' |
                        sed 's/.grd$//' |
                            sed 's/.GRD$//' |
                                sort -u
    }


CleanupFiles () {
    rm -f *.msg
    rm -f core.*
    rm -f d3d_visu.*
    rm -f fort.35
    rm -f fourier*.*
    rm -f md-diag*.*
    rm -f runid
    rm -f td-diag*.*
    rm -f tstprt.*
    rm -f TMP*.*
    rm -f tri-diag.*
    rm -f vgcore.pid*
    }


RunMorflow $*
