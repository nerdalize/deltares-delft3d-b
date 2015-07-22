#   Makefile for DIO test
#
#   Stef.Hummel@deltares.nl
#
#   (c) Deltares, Feb '01
#


TOPDIR=../..
include $(TOPDIR)/make/preamble.mkf


SRC_tst-his-get-selection	= tst-his-get-selection.F90
SRC_tst-his-get-time		= tst-his-get-time.F90
SRC_tst-his-put-time		= tst-his-put-time.F90
SRC_tst-his-put-1D		= tst-his-put-1D.F90
SRC_tst-his-copy		= tst-his-copy.F90
SRC_tst-his-get			= tst-his-get.F90
SRC_tst-his-get-hisstep		= tst-his-get-hisstep.F90
SRC_tst-his-put			= tst-his-put.F90
SRC_tst-his-put-double		= tst-his-put-double.F90
SRC_tst-his-put-hisStep		= tst-his-put-hisStep.F90

OBJ_tst-his-get-selection	= $(MAKE_TARGETDIR)/$(SRC_tst-his-get-selection:.F90=.o)
OBJ_tst-his-get-time		= $(MAKE_TARGETDIR)/$(SRC_tst-his-get-time:.F90=.o)
OBJ_tst-his-put-time		= $(MAKE_TARGETDIR)/$(SRC_tst-his-put-time:.F90=.o)
OBJ_tst-his-put-1D		= $(MAKE_TARGETDIR)/$(SRC_tst-his-put-1D:.F90=.o)
OBJ_tst-his-copy		= $(MAKE_TARGETDIR)/$(SRC_tst-his-copy:.F90=.o)
OBJ_tst-his-get			= $(MAKE_TARGETDIR)/$(SRC_tst-his-get:.F90=.o)
OBJ_tst-his-get-hisstep		= $(MAKE_TARGETDIR)/$(SRC_tst-his-get-hisstep:.F90=.o)
OBJ_tst-his-put			= $(MAKE_TARGETDIR)/$(SRC_tst-his-put:.F90=.o)
OBJ_tst-his-put-double		= $(MAKE_TARGETDIR)/$(SRC_tst-his-put-double:.F90=.o)
OBJ_tst-his-put-hisStep		= $(MAKE_TARGETDIR)/$(SRC_tst-his-put-hisStep:.F90=.o)

PROG_tst-his-get-selection	= unix/$(SRC_tst-his-get-selection:.F90=.exe)
PROG_tst-his-get-time		= unix/$(SRC_tst-his-get-time:.F90=.exe)
PROG_tst-his-put-time		= unix/$(SRC_tst-his-put-time:.F90=.exe)
PROG_tst-his-put-1D		= unix/$(SRC_tst-his-put-1D:.F90=.exe)
PROG_tst-his-copy		= unix/$(SRC_tst-his-copy:.F90=.exe)
PROG_tst-his-get		= unix/$(SRC_tst-his-get:.F90=.exe)
PROG_tst-his-get-hisstep	= unix/$(SRC_tst-his-get-hisstep:.F90=.exe)
PROG_tst-his-put		= unix/$(SRC_tst-his-put:.F90=.exe)
PROG_tst-his-put-double		= unix/$(SRC_tst-his-put-double:.F90=.exe)
PROG_tst-his-put-hisStep	= unix/$(SRC_tst-his-put-hisStep:.F90=.exe)


all:		$(PROG_tst-his-get-selection) \
		$(PROG_tst-his-get-time) \
		$(PROG_tst-his-put-time) \
		$(PROG_tst-his-put-1D) \
		$(PROG_tst-his-copy) \
		$(PROG_tst-his-get) \
		$(PROG_tst-his-get-hisstep) \
		$(PROG_tst-his-put) \
		$(PROG_tst-his-put-double) \
		$(PROG_tst-his-put-hisStep)


nefis:


$(PROG_tst-his-get-selection):	$(OBJ_tst-his-get-selection) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-get-selection) $(OBJ_tst-his-get-selection) $(LIBS)

$(PROG_tst-his-get-time):	$(OBJ_tst-his-get-time) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-get-time) $(OBJ_tst-his-get-time) $(LIBS)

$(PROG_tst-his-put-time):	$(OBJ_tst-his-put-time) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-put-time) $(OBJ_tst-his-put-time) $(LIBS)

$(PROG_tst-his-put-1D):		$(OBJ_tst-his-put-1D) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-put-1D) $(OBJ_tst-his-put-1D) $(LIBS)

$(PROG_tst-his-copy):		$(OBJ_tst-his-copy) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-copy) $(OBJ_tst-his-copy) $(LIBS)

$(PROG_tst-his-get):		$(OBJ_tst-his-get) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-get) $(OBJ_tst-his-get) $(LIBS)

$(PROG_tst-his-get-hisstep):	$(OBJ_tst-his-get-hisstep) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-get-hisstep) $(OBJ_tst-his-get-hisstep) $(LIBS)

$(PROG_tst-his-put):		$(OBJ_tst-his-put) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-put) $(OBJ_tst-his-put) $(LIBS)

$(PROG_tst-his-put-double):	$(OBJ_tst-his-put-double) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-put-double) $(OBJ_tst-his-put-double) $(LIBS)

$(PROG_tst-his-put-hisStep):	$(OBJ_tst-his-put-hisStep) $(LIBS)
				$(FF) $(MAKE_LDFLAGS) -o $(PROG_tst-his-put-hisStep) $(OBJ_tst-his-put-hisStep) $(LIBS)


include $(TOPDIR)/make/postamble-dio-tests.mkf
