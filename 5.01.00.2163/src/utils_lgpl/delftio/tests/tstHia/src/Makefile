#   Makefile for DIO test
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Feb '01
#


TOPDIR=../..
include $(TOPDIR)/make/preamble.mkf


SRC_tst_hia		= tst_hia.F90
SRC_tst_his_long	= tst_his_long.F90
SRC_tst_descr		= tst_descr.F90

OBJ_tst_hia		= $(MAKE_TARGETDIR)/$(SRC_tst_hia:.F90=.o)
OBJ_tst_his_long	= $(MAKE_TARGETDIR)/$(SRC_tst_his_long:.F90=.o)
OBJ_tst_descr		= $(MAKE_TARGETDIR)/$(SRC_tst_descr:.F90=.o)

PROG_tst_hia		= unix/$(SRC_tst_hia:.F90=.exe)
PROG_tst_his_long	= unix/$(SRC_tst_his_long:.F90=.exe)
PROG_tst_descr		= unix/$(SRC_tst_descr:.F90=.exe)


all:		$(PROG_tst_hia) \
		$(PROG_tst_his_long) \
		$(PROG_tst_descr)

nefis:

$(PROG_tst_hia):	$(OBJ_tst_hia) $(LIB_DELFTIO)
			$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst_hia) $(OBJ_tst_hia) $(LIBS)

$(PROG_tst_his_long):	$(OBJ_tst_his_long) $(LIB_DELFTIO)
			$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst_his_long) $(OBJ_tst_his_long) $(LIBS)

$(PROG_tst_descr):	$(OBJ_tst_descr) $(LIB_DELFTIO)
			$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst_descr) $(OBJ_tst_descr) $(LIBS)


include $(TOPDIR)/make/postamble-dio-tests.mkf
