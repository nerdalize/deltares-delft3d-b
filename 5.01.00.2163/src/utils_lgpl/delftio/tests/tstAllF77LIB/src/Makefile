#   Makefile for DIO test
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Feb '01
#


TOPDIR=../..
include $(TOPDIR)/make/preamble.mkf


SRC_PUT		= tst-f77-put.F
SRC_GET		= tst-f77-get.F
SRC_TEST	= dio-f77-tst.F
SRC_MAP		= tst-f77-map.F
SRC_ENDTIME	= tst-f77-endTime.F

OBJ_PUT		= $(MAKE_TARGETDIR)/$(SRC_PUT:.F=.o)
OBJ_GET		= $(MAKE_TARGETDIR)/$(SRC_GET:.F=.o)
OBJ_TEST	= $(MAKE_TARGETDIR)/$(SRC_TEST:.F=.o)
OBJ_MAP		= $(MAKE_TARGETDIR)/$(SRC_MAP:.F=.o)
OBJ_ENDTIME	= $(MAKE_TARGETDIR)/$(SRC_ENDTIME:.F=.o)

PROG_PUT	= unix/$(SRC_PUT:.F=.exe)
PROG_GET	= unix/$(SRC_GET:.F=.exe)
PROG_MAP	= unix/$(SRC_MAP:.F=.exe)
PROG_ENDTIME	= unix/$(SRC_ENDTIME:.F=.exe)


all:		$(OBJ_TEST) $(PROG_PUT) $(PROG_GET) $(PROG_MAP) $(PROG_ENDTIME)

nefis:

$(PROG_GET):	$(OBJ_TEST) $(OBJ_GET) $(LIB_DELFTIO)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_GET) $(OBJ_GET) $(OBJ_TEST) $(LIBS)

$(PROG_PUT):	$(OBJ_TEST) $(OBJ_PUT) $(LIB_DELFTIO)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_PUT) $(OBJ_PUT) $(OBJ_TEST) $(LIBS)

$(PROG_MAP):	$(OBJ_MAP) $(LIB_DELFTIO)
		$(FF) $(MAKE_LDFLAGS) -o $(PROG_MAP) $(OBJ_MAP) $(LIBS)

$(PROG_ENDTIME):	$(OBJ_ENDTIME) $(LIB_DELFTIO)
			$(FF) $(MAKE_LDFLAGS) -o $(PROG_ENDTIME) $(OBJ_ENDTIME) $(LIBS)


include $(TOPDIR)/make/postamble-dio-tests.mkf
