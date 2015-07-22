#   PutGet defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jan '03
#

CHECK_DIFF	=	../../../include/checkDiff

test:
		make reset
		tst_rewind.exe
		mv TESTRewind1.his TESTRewindTot.his 
		cat TESTRewind2.his >> TESTRewindTot.his 
		rm -f TESTRewind2.his
		tst_rewind_append_get.exe
		sleep 1
		$(CHECK_DIFF)

reset:
		@rm -f TEST* dio-*-errors.txt
