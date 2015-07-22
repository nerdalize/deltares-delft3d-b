#   PutGet defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jan '03
#

CHECK_DIFF	=	../../../include/checkDiff

test:
		make reset
		diorun tst_his_put.exe tst_his_get.exe
		sleep 1
		tst_his_copy.exe
		tst_his_put_time.exe
		tst_his_put_hisStep.exe
		tst_his_put_1D.exe
		tst_his_put_double.exe
		tst_his_get_time.exe
		tst_his_get_hisstep.exe
		tst_his_get_selection.exe
		sleep 1
		$(CHECK_DIFF)

reset:
		@rm -f TEST* dio-*-errors.txt
