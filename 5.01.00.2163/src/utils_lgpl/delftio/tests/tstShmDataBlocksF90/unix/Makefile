#   PutGet Parallel Test defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jun '01
#

CHECK_DIFF	=	../../../include/checkDiff

test:
		make reset
		diorun tst-shmDatablock-put.exe tst-shmDatablock-get.exe 3
		sleep 1

reset:
		@rm -f *.stream *.data TEST* *.txt
