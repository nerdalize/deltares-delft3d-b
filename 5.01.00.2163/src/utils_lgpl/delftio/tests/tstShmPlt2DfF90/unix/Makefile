#   PutGet Parallel Test defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jun '01
#

CHECK_DIFF	=	../../../include/checkDiff

test:
		make reset
		diorun tst-shm-plt-2df-put.exe tst-shm-plt-2df-get.exe
		tst-shm-plt-2df-inMem.exe
		sleep 1
		$(CHECK_DIFF)

reset:
		@rm -f *.stream *.data TEST* *.txt *.tmp???
