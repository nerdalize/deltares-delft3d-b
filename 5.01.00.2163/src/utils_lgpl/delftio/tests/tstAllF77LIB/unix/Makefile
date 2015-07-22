#   PutGet Parallel Test defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jun '01
#

CHECK_DIFF	=	../../../include/checkDiff

test:
		make reset
		tst-f77-put.exe
		tst-f77-get.exe
		tst-f77-map.exe
		tst-f77-endTime.exe
		sleep 1
		$(CHECK_DIFF)

reset:
		@rm -f *.stream *.data sync.TEST* TEST* Test* *res.txt dio-*-errors.txt
