#   Makefile for DIO test
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Feb '01
#


TOPDIR=../..
include $(TOPDIR)/make/preamble.mkf


SRC_PUT		= tst-plt-put.F90
SRC_GET		= tst-plt-get.F90
SRC_TEST	= dio-plt-tst.F90

OBJ_PUT		= $(MAKE_TARGETDIR)/$(SRC_PUT:.F90=.o)
OBJ_GET		= $(MAKE_TARGETDIR)/$(SRC_GET:.F90=.o)
OBJ_TEST	= $(MAKE_TARGETDIR)/$(SRC_TEST:.F90=.o)

PROG_PUT	= unix/$(SRC_PUT:.F90=.exe)
PROG_GET	= unix/$(SRC_GET:.F90=.exe)


all:		$(OBJ_TEST) $(PROG_PUT) $(PROG_GET)

nefis:

$(PROG_GET):	$(OBJ_TEST) $(OBJ_GET) $(LIB_DELFTIO)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_GET) $(OBJ_GET) $(OBJ_TEST) $(LIBS)

$(PROG_PUT):	$(OBJ_TEST) $(OBJ_PUT) $(LIBS)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_PUT) $(OBJ_PUT) $(OBJ_TEST) $(LIBS)


include $(TOPDIR)/make/postamble-dio-tests.mkf
