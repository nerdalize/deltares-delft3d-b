#   Makefile for DIO test
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Feb '01
#


TOPDIR=../..
include $(TOPDIR)/make/preamble.mkf


SRC_PUT		= tst_dio_conf.F90

OBJ_PUT		= $(MAKE_TARGETDIR)/$(SRC_PUT:.F90=.o)

PROG_PUT	= unix/$(SRC_PUT:.F90=.exe)


all:		$(PROG_PUT)

nefis:

$(PROG_PUT):	$(OBJ_PUT) $(LIB_DELFTIO)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_PUT) $(OBJ_PUT) $(LIBS)


include $(TOPDIR)/make/postamble-dio-tests.mkf
