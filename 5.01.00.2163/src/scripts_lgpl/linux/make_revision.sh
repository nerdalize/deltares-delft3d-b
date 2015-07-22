#!/bin/bash

#   =====================================
#   Arguments
#   =====================================

#   $1: Top directory of the source tree: used to define SVN_DIR and CMD_DIR
#   $2: Module directory                : svnrevision is executed in this directory
#   $3: Version number file             : containing MAJOR, MINOR and REVISION definitions
#   $4: Input file                      : containing SVN_REVISION to be replaced, normally outputfile.svn (with a double extension)
#   $5: Output file                     : contents of input file with SVN_REVISION replaced by the actual revision string



#   =====================================
#   Get all directories needed
#   =====================================

CURDIR=`pwd` 
cd $1
TOPDIR=`pwd`
cd $CURDIR
cd $2
MODDIR=`pwd`

# svn_dir is not needed to set explicitly
# cmd_dir is not used on LINUX

# we just made this one. ( I think this should work for everyone, TODO: pls make this the default after tested )
VN_DIR=$TOPDIR/third_party_open/version_number/packages/version_number/src



if [ "$BUILD_NUMBER" != "" ]; then

   #   =====================================
   #   BUILD_NUMBER already known
   #   =====================================
   BUILD_NUMBER=$BUILD_NUMBER

else

   #   =====================================
   #   Execute svnrevision
   #   =====================================

   cd $MODDIR
   if svnversion . >/dev/null 2>/dev/null ; then 
      BUILD_NUMBER=`svnversion -n $MODDIR`; \
   else 
      BUILD_NUMBER="000000"; 
   fi;
   #   also write it to file
   # echo $BUILD_NUMBER > $MODDIR/BUILD_NUMBER

fi 



#   =====================================
#   Build substitution line
#   =====================================

ADDLINE="$BUILD_NUMBER"



#   =====================================
#   Inputfile > Substitute > Outputfile
#   =====================================

cd $CURDIR

$VN_DIR/version_number.exe $BUILD_NUMBER $3 $4 $5

#   =====================================
#   Clean up
#   =====================================

# rm -f $MODDIR/BUILD_NUMBER




#   =====================================
#   Finished
#   =====================================


