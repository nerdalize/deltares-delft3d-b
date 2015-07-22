#! /bin/sh

remote=x303

echo "local = `hostname`"
echo "remote = $remote"
echo

time ./stream02test.exe -s -T -r $remote -v

echo
echo "Finished at `date`"

