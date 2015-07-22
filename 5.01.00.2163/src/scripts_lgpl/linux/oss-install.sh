#!/bin/bash

globalErrorLevel=0

# This script uses the command ldd to collect all dynamic libraries used:
gatherScript=scripts_lgpl/linux/gatherlibraries.rb
# The following libraries must be removed from the list created by gatherScript:
# - system dependent libraries in the directories /lib and /lib64
# - libraries generated in the oss tree itself
gatherFilter="-e '^/lib/' -e '^/lib64/' -e 'flow2d3d' -e 'DelftOnline'"

# ===============================
# === copyFile: handles error ===
# ===============================
function copyFile () {
    # This function can handle wild characters in the arguments,
    # as long as they are quoted
    # example: copyFile "bin/*" targetdir

    # handle the error
    for file in $1
    do
        eval cp -fp $file $2
        if [ $? != 0 ]; then
            echo "can't copy \"$file\" to \"$2\"" 1>&2
            globalErrorLevel=1
        fi
    done

    return
}

# ===================
# === INSTALL_ALL ===
# ===================
function install_all () {
    echo "installing all open source projects . . ."

    d_hydro
    flow2d3d
    flow2d3d_openda
    wave
    plugin_culvert
    plugin_delftflow_traform
    datsel
    kubint
    lint
    mormerge
    vs

    return
}



# ============================
# === INSTALL_DELFT3D_FLOW ===
# ============================
function delft3d_flow () {
    echo "installing delft3d-flow . . ."

    d_hydro
    flow2d3d
    flow2d3d_openda
    plugin_culvert
    plugin_delftflow_traform
    mormerge
    vs

    return
}



# =======================
# === INSTALL_D_HYDRO ===
# =======================
function d_hydro () {
    echo "installing d_hydro . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"

    mkdir -p $dest_bin
    mkdir -p $dest_lib

    copyFile "bin/d_hydro.exe" 					    $dest_bin
    copyFile "third_party_open/tclkit/bin/intel/deltares_hydro.exe" $dest_bin

    echo "Gathering libraries for d_hydro..."
    cp -u `$gatherScript bin/d_hydro.exe | eval grep -v $gatherFilter` $dest_lib
    return
}



# ========================
# === INSTALL FLOW2D3D ===
# ========================
function flow2d3d () {
    echo "installing flow2d3d . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"
    dest_default="$dest_main/lnx/flow2d3d/default"
    dest_scripts="$dest_main/lnx/flow2d3d/scripts"

    mkdir -p $dest_bin
    mkdir -p $dest_lib
    mkdir -p $dest_default
    mkdir -p $dest_scripts

    copyFile "lib/libflow2d3d.so"                           $dest_bin
    copyFile "lib/libflow2d3d_sp.so"                        $dest_bin
    # For some reason, libflow2d3d.so depends on libDelftOnline.so.0 instead of libDelftOnline.so. Both are links to libDelftOnline.so.0.0.0
    copyFile "lib/libDelftOnline.so.0"                      $dest_bin
    copyFile "engines_gpl/flow2d3d/scripts/meteo_old2new.m" $dest_scripts
    copyFile "bin/esm_create"                               $dest_bin
    copyFile "bin/esm_delete"                               $dest_bin
    copyFile "bin/esm_info"                                 $dest_bin
    copyFile "engines_gpl/flow2d3d/default/*"               $dest_default

    echo "Gathering libraries for flow2d3d..."
    cp -u `$gatherScript lib/libflow2d3d*.so lib/libDelftOnline.so bin/esm_* | eval grep -v $gatherFilter` $dest_lib
    return
}



# ===============================
# === INSTALL FLOW2D3D_OPENDA ===
# ===============================
function flow2d3d_openda () {
    echo "installing flow2d3d_openda . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"
    dest_default="$dest_main/lnx/flow2d3d/default"
    dest_scripts="$dest_main/lnx/flow2d3d/scripts"

    mkdir -p $dest_bin
    mkdir -p $dest_lib
    mkdir -p $dest_default
    mkdir -p $dest_scripts

    copyFile "lib/libflow2d3d_openda.so"                    $dest_bin
    copyFile "lib/libflow2d3d_openda_sp.so"                 $dest_bin
    copyFile "lib/libDelftOnline.so"                        $dest_bin
    copyFile "engines_gpl/flow2d3d/scripts/meteo_old2new.m" $dest_scripts
    copyFile "bin/esm_create"                               $dest_bin
    copyFile "bin/esm_delete"                               $dest_bin
    copyFile "bin/esm_info"                                 $dest_bin
    copyFile "engines_gpl/flow2d3d/default/*.*"             $dest_default

    echo "Gathering libraries for flow2d3d_openda..."
    cp -u `$gatherScript lib/libflow2d3d_openda*.so lib/libDelftOnline.so bin/esm_* | eval grep -v $gatherFilter` $dest_lib
    return
}



# ====================
# === INSTALL WAVE ===
# ====================
function wave () {
    echo "installing wave . . ."

    dest_bin="$dest_main/lnx/wave/bin"
    dest_lib="$dest_main/lnx/wave/lib"
    dest_default="$dest_main/lnx/wave/default"
    dest_swan_bin="$dest_main/lnx/swan/bin"
    dest_swan_lib="$dest_main/lnx/swan/lib"
    dest_swan_scripts="$dest_main/lnx/swan/scripts"

    mkdir -p $dest_bin
    mkdir -p $dest_lib
    mkdir -p $dest_default
    mkdir -p $dest_swan_bin
    mkdir -p $dest_swan_lib
    mkdir -p $dest_swan_scripts

    copyFile "bin/wave.exe"                                  $dest_bin
    copyFile "engines_gpl/flow2d3d/default/dioconfig.ini"    $dest_default
    copyFile "third_party_open/swan/bin/linux/*.*"           $dest_swan_bin
    copyFile "third_party_open/swan/lib/linux/*.*"           $dest_swan_lib
    copyFile "third_party_open/swan/scripts/swan_install.sh" $dest_swan_scripts/swan.sh

    echo "Gathering libraries for wave..."
    cp -u `$gatherScript bin/wave.exe | eval grep -v $gatherFilter` $dest_lib
    echo "Gathering libraries for swan..."
    cp -u `$gatherScript third_party_open/swan/bin/linux/*.exe | eval grep -v $gatherFilter` $dest_swan_lib
    return
}



# ==============================
# === INSTALL PLUGIN_CULVERT ===
# ==============================
function plugin_culvert () {
    echo "installing plugin_culvert . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"

    mkdir -p $dest_bin
    mkdir -p $dest_lib

    copyFile "lib/libplugin_culvert.so" $dest_bin/plugin_culvert.so

    echo "Gathering libraries for plugin_culvert..."
    cp -u `$gatherScript lib/libplugin_culvert.so | eval grep -v $gatherFilter` $dest_lib
    return
}



# ========================================
# === INSTALL PLUGIN_DELFTFLOW_TRAFORM ===
# ========================================
function plugin_delftflow_traform () {
    echo "installing plugin_delftflow_traform . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"

    mkdir -p $dest_bin
    mkdir -p $dest_lib

    copyFile "lib/libplugin_delftflow_traform.so" $dest_bin/plugin_delftflow_traform.so

    echo "Gathering libraries for plugin_delftflow_traform..."
    cp -u `$gatherScript lib/libplugin_delftflow_traform.so | eval grep -v $gatherFilter` $dest_lib
    return
}



# ======================
# === INSTALL DATSEL ===
# ======================
function datsel () {
    echo "installing datsel . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"

    mkdir -p $dest_bin
    mkdir -p $dest_lib

    copyFile "bin/datsel" $dest_bin

    echo "Gathering libraries for datsel..."
    cp -u `$gatherScript bin/datsel | eval grep -v $gatherFilter` $dest_lib
    return
}



# ======================
# === INSTALL KUBINT ===
# ======================
function kubint () {
    echo "installing kubint . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"

    mkdir -p $dest_bin
    mkdir -p $dest_lib

    copyFile "bin/kubint" $dest_bin

    echo "Gathering libraries for kubint..."
    cp -u `$gatherScript bin/kubint | eval grep -v $gatherFilter` $dest_lib
    return
}



# ====================
# === INSTALL LINT ===
# ====================
function lint () {
    echo "installing lint . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"

    mkdir -p $dest_bin
    mkdir -p $dest_lib

    copyFile "bin/lint" $dest_bin

    echo "Gathering libraries for lint..."
    cp -u `$gatherScript bin/lint | eval grep -v $gatherFilter` $dest_lib
    return
}



# ========================
# === INSTALL MORMERGE ===
# ========================
function mormerge () {
    echo "installing mormerge . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"
    dest_scripts="$dest_main/lnx/flow2d3d/scripts"

    mkdir -p $dest_bin
    mkdir -p $dest_lib
    mkdir -p $dest_scripts

    copyFile "engines_gpl/flow2d3d/scripts/mormerge.tcl" $dest_scripts
    copyFile "bin/mormerge.exe"                          $dest_bin

    echo "Gathering libraries for mormerge..."
    cp -u `$gatherScript bin/mormerge.exe | eval grep -v $gatherFilter` $dest_lib
    return
}



# ==================
# === INSTALL VS ===
# ==================
function vs () {
    echo "installing vs . . ."

    dest="$dest_main/lnx/util/bin"
    dest_lib="$dest_main/lnx/util/lib"

    mkdir -p $dest
    mkdir -p $dest_lib

    copyFile "bin/vs" $dest

    echo "Gathering libraries for vs..."
    cp -u `$gatherScript bin/vs | eval grep -v $gatherFilter` $dest_lib
    return
}



# =======================
# === INSTALL NESTHD1 ===
# =======================
function nesthd1 () {
    echo "installing nesthd1 . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"

    mkdir -p $dest_bin
    mkdir -p $dest_lib

    copyFile "bin/nesthd1" $dest_bin

    echo "Gathering libraries for nesthd1..."
    cp -u `$gatherScript bin/nesthd1 | eval grep -v $gatherFilter` $dest_lib
    return
}



# =======================
# === INSTALL NESTHD2 ===
# =======================
function nesthd2 () {
    echo "installing nesthd2 . . ."

    dest_bin="$dest_main/lnx/flow2d3d/bin"
    dest_lib="$dest_main/lnx/flow2d3d/lib"

    mkdir -p $dest_bin
    mkdir -p $dest_lib

    copyFile "bin/nesthd2" $dest_bin

    echo "Gathering libraries for nesthd2..."
    cp -u `$gatherScript bin/nesthd2 | eval grep -v $gatherFilter` $dest_lib
    return
}



# ============
# === MAIN ===
# ============

echo oss-install...

# Example calls:
# > install.cmd <dest directory>              # Install entire solution
# > install.cmd flow2d3d <dest directory>     # Install only project flow2d3d (and its dependencies)

# 0. defaults:
project=
dest_main=

if [ "$2" == '' ]; then
    # Install all engines, assume the first argument is a target directory

    dest_main=$1
    project=install_all
    echo Target directory: $dest_main
    echo Source          : all engines
else
    # Install the package/engine specified by the first argument. The second argument is assumed to be the target directory.

    dest_main=$2
    project=$1
    echo Target directory: $dest_main
    echo Source          : package/engine $project
fi

if [ "$dest_main" == '' ]; then
    echo "ERROR: No target directory specified as argument of oss-install.sh"
    exit 1
fi

# Change to directory tree where this batch file resides (necessary when oss-install.sh is called from outside of oss/trunk/src)
curdir=`pwd`
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
cd $scriptdir/../..


mkdir -p $dest_main
cp scripts_lgpl/linux/convert_lnx_to_old_directory_structure_intel.sh $dest_main

$project

cd $curdir


exit $globalErrorLevel
