#!/bin/bash
#
echo "==== $1 ===="
testnr=`echo "$1" | grep -Po "\d+"`
testdir=`echo "$1" | grep -Po "^.+/"`
rm -rf data*.*
rm -rf ${testdir}data*.*
cp ../test_06/nefis_ex.dat .
cp ../test_06/nefis_ex.def .
$1 > ${testdir}data_c$testnr.res 2>&1
diff -b ${testdir}data_c$testnr.res ${testdir}pre_c$testnr.res
