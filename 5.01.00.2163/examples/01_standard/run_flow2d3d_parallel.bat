@ echo off
    rem
    rem This script is an example for running Delft3D-FLOW
    rem Adapt and use it for your own purpose
    rem
    rem edwin.spee@deltares.nl
    rem adri.mourits@deltares.nl
    rem 11 Apr 2011
    rem 
    rem
    rem This script starts a single-domain Delft3D-FLOW computation on Windows
    rem parallel
    rem
    rem When using mpich2 for the first time on a machine:
    rem Execute "smpd -install"
    rem 

    rem
    rem Set the config file here
    rem 
set argfile=config_flow2d3d.ini





    rem
    rem Set the directory containing ALL exes/dlls here (mpiexec.exe, delftflow.exe, flow2d3d.dll, mpich-dlls, DelftOnline dlls etc.)
    rem
set D3D_HOME=..\..\bin\win32
set exedir=%D3D_HOME%\flow2d3d\bin
set libdir=%D3D_HOME%\flow2d3d\lib

    rem
    rem No adaptions needed below
    rem

    rem Set some (environment) parameters
set PATH=%exedir%;%libdir%;%PATH%
    rem mpiexec is in %exedir%
    rem For some users, it is necessary to use the locally installed mpiexec:
set MPIPATH=%exedir%
    rem set MPIPATH="C:\Program Files (x86)\Common Files\Intel\Shared Libraries\redist\intel64\mpirt"


    rem Run
    rem start computation on all your local cores (2 for dual core; 4 for quad core etc.)
    rem note the flag "-localonly" which may be needed to avoid "Aborting: unable to connect to machinename.local"
%MPIPATH%\mpiexec -n %NUMBER_OF_PROCESSORS% -localonly %exedir%\deltares_hydro.exe %argfile%


    rem To prevent the DOS box from disappearing immediately: remove the rem on the following line
pause
