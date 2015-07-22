#   PutGet Parallel Test defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jun '01
#

CHECK_DIFF	=	../../../include/checkDiff

test:
		make reset
		diorun tst-plt-put.exe tst-plt-get.exe
		sleep 1
		$(CHECK_DIFF)

reset:
		@rm -f *.stream *.data TEST* *.txt *.tmp???
