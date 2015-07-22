#   Makefile for DIO test
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Feb '01
#


TOPDIR=../..
include $(TOPDIR)/make/preamble.mkf


SRC_tst_map		= tst_map.F90
SRC_tst_map_long	= tst_map_long.F90

OBJ_tst_map		= $(MAKE_TARGETDIR)/$(SRC_tst_map:.F90=.o)
OBJ_tst_map_long	= $(MAKE_TARGETDIR)/$(SRC_tst_map_long:.F90=.o)

PROG_tst_map		= unix/$(SRC_tst_map:.F90=.exe)
PROG_tst_map_long	= unix/$(SRC_tst_map_long:.F90=.exe)


all:		$(PROG_tst_map) \
		$(PROG_tst_map_long)

nefis:

$(PROG_tst_map):	$(OBJ_tst_map) $(LIB_DELFTIO)
			$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst_map) $(OBJ_tst_map) $(LIBS)

$(PROG_tst_map_long):	$(OBJ_tst_map_long) $(LIB_DELFTIO)
			$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst_map_long) $(OBJ_tst_map_long) $(LIBS)


include $(TOPDIR)/make/postamble-dio-tests.mkf
