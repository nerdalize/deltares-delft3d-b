#   Makefile for DIO test
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Feb '01
#

TOPDIR=../..
include $(TOPDIR)/make/preamble.mkf

SRC_PUT		= tst-shmDatablock-put.F90
SRC_GET		= tst-shmDatablock-get.F90

OBJ_PUT		= $(MAKE_TARGETDIR)/$(SRC_PUT:.F90=.o)
OBJ_GET		= $(MAKE_TARGETDIR)/$(SRC_GET:.F90=.o)

PROG_PUT	= unix/$(SRC_PUT:.F90=.exe)
PROG_GET	= unix/$(SRC_GET:.F90=.exe)


LIBS	= $(LIB_DELFTIO) $(LIB_ESMFSM)


all:		$(PROG_PUT) $(PROG_GET)

nefis:

$(PROG_GET):	$(OBJ_GET) $(LIBS)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_GET) $(OBJ_GET) $(LIBS)

$(PROG_PUT):	$(OBJ_PUT) $(LIBS)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_PUT) $(OBJ_PUT) $(LIBS)

include $(TOPDIR)/make/postamble-dio-tests.mkf
