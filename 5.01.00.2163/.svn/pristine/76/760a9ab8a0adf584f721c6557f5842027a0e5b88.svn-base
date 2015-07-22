#!/bin/bash

echo "Converting 'new' directory structure (lnx) to 'old' directory structure (intel) ..."

if [ ! -d lnx ]; then
   echo "Directory 'lnx' does not exist. Can not convert."
   exit
fi

if [ -d intel ]; then
   echo "Deleting target directory intel..."
   rm -rf intel
fi

mkdir -p intel/flow/bin
mkdir -p intel/flow/default
mkdir -p intel/lib
mkdir -p intel/util
mkdir -p intel/wave/bin
mkdir -p intel/wave/default

cp lnx/flow2d3d/bin/*     intel/flow/bin
cp lnx/flow2d3d/lib/*     intel/flow/bin
cp lnx/flow2d3d/scripts/* intel/flow/bin
cp lnx/flow2d3d/default/* intel/flow/default

cp lnx/swan/bin/*     intel/wave/bin
cp lnx/swan/lib/*     intel/wave/bin
cp lnx/swan/scripts/* intel/wave/bin
cp lnx/wave/bin/*     intel/wave/bin
cp lnx/wave/lib/*     intel/wave/bin
cp lnx/wave/default/* intel/wave/default

cp lnx/util/bin/*     intel/util


echo "Finished converting."

