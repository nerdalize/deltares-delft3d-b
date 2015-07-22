#   PutGet Parallel Test defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jun '01
#

CHECK_DIFF	=	../../../include/checkDiff

test:
		make reset
		diorun tst-f77-put.exe tst-f77-get.exe
		sleep 1
		$(CHECK_DIFF)

reset:
		@rm -f *.stream *.data sync.TEST* TEST* *.txt
