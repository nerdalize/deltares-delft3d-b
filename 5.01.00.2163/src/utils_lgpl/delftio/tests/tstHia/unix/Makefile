#   PutGet defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jan '03
#

CHECK_DIFF	=	../../../include/checkDiff

test:
		make reset
		tst_hia.exe
		tst_his_long.exe
		sleep 1
		$(CHECK_DIFF)

reset:
		@rm -f TEST* dio-*-errors.txt
