#   Makefile for DIO test
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Feb '01
#


TOPDIR=../..
include $(TOPDIR)/make/preamble.mkf


SRC_PUT		= tst_rewind.F90
SRC_GET		= tst_rewind_append_get.F90

OBJ_PUT		= $(MAKE_TARGETDIR)/$(SRC_PUT:.F90=.o)
OBJ_GET		= $(MAKE_TARGETDIR)/$(SRC_GET:.F90=.o)

PROG_PUT	= unix/$(SRC_PUT:.F90=.exe)
PROG_GET	= unix/$(SRC_GET:.F90=.exe)


all:		$(PROG_PUT) $(PROG_GET)

nefis:

$(PROG_GET):	$(OBJ_GET) $(LIB_DELFTIO)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_GET) $(OBJ_GET) $(LIBS)

$(PROG_PUT):	$(OBJ_PUT) $(LIB_DELFTIO)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_PUT) $(OBJ_PUT) $(LIBS)


include $(TOPDIR)/make/postamble-dio-tests.mkf
