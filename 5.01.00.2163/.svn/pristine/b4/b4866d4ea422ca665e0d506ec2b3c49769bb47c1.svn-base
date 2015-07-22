#   PutGet Parallel Test defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jun '01
#

CHECK_DIFF	=	../../../include/checkDiff

test:
		make reset
		tst-many-put.exe
		tst-many-get.exe
		sleep 1
		$(CHECK_DIFF)

reset:
		@rm -f TEST* dio-*-errors.txt
