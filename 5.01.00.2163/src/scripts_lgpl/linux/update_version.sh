#!/bin/bash

# Program will replace $1 by the $1.svn and will replace VERSION_BUILD_NUMBER by a corresponding svn version using svnversion command

# $1 - path to the target source file
# $2 - path to the folder to be used to check svnversion
# $3 - Single file with version number information version_number.ini
# $4 - Top directory of the source tree: used to define VN_DIR

echo Generating version number in the $1

cd $2
MODDIR=`pwd` 
cd $4
TOPDIR=`pwd`
# we just made this one. ( I think this should work for everyone, TODO: pls make this the default after tested )
VN_DIR=$TOPDIR/third_party_open/version_number/packages/version_number/src

cd $MODDIR

if [ "$BUILD_NUMBER" != "" ]; then

   #   =====================================
   #   BUILD_NUMBER already known
   #   =====================================
   SVN_VERSION=$BUILD_NUMBER

else

   #   =====================================
   #   Execute svnrevision
   #   =====================================

   cd $MODDIR
   if svnversion . >/dev/null 2>/dev/null ; then 
      SVN_VERSION=`svnversion -n $MODDIR`; \
   else 
      # source is exported/exculded from SVN, no revision number known
      SVN_VERSION="000000";  
   fi;
   #   also write it to file
   # echo $SVN_VERSION > $MODDIR/$SVN_VERSION

fi  

# Generate version number source module using version_number.exe
$VN_DIR/version_number.exe $SVN_VERSION $3 $1.svn $1.temp

if [ -f $1 ]; then
	diff $1 $1.temp > nul
    ERRORLEVEL=$?
else
	ERRORLEVEL=1
fi

if [ $ERRORLEVEL==1 ]; then
	mv $1.temp $1
    echo Done, new version number is: $SVN_VERSION
else
	rm $1.temp
	echo Done, file is up to date, no need to update version
fi
