#   PutGet Parallel Test defines for DIO parallel tests
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Jun '01
#

CHECK_DIFF	=	../../../include/checkDiff

nefisTest:
		make nefisReset
		tstRestartput.exe
		tstRestartget.exe
		tstTriton.exe
		sleep 1
		$(CHECK_DIFF)

nefisReset:
		@rm -f *.def *.dat TEST* *.txt *.tmp???
