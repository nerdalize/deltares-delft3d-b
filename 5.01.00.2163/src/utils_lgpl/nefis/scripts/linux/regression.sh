#!/bin/bash
#
echo "==== $1 ===="
pwd
testnr=`echo "$1" | egrep -o "[[:digit:]]+$"`
testdir=`echo "$1" | egrep -o "^.+/"`
testexe=test_${testnr}
cd ${testdir}
rm -rf data*.*
./${testexe} > data_c${testnr}.res 2>&1
diff -b data_c${testnr}.res pre_c${testnr}.res
cd -
