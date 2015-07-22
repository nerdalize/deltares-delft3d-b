#!/bin/bash

# Change to directory where this batch file resides (necessary when oss-install.sh is called from outside of oss/trunk/src)
curdir=`pwd`
scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
cd $scriptdir

scripts_lgpl/linux/oss-install.sh $1 $2

cd $curdir

exit 0
