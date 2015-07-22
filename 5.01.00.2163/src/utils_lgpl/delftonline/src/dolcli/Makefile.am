#-------------------------------------------------------------------------------
#   Makefile for DelftOnline
#
#   Irv.Elshoff@Deltares.NL
#   4 jul 12
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

if ENABLE_DOLCLI

bin_PROGRAMS = dolcli

dolcli_SOURCES = \
    dolcli.cpp \
    parse.yy \
    lexan.ll

# TODO, check for these libs in configure.
# The flags should then be automaticly added.
# AC_CHECK_LIB(library, function)
dolcli_LDADD = \
    ../delftonline/libDelftOnline.la \
    -ll \
    -lreadline \
    -ltermcap

INCLUDES = \
    -I$(top_builddir)/utils_lgpl/delftonline/include

AM_LFLAGS = -L
AM_YFLAGS = -d

CLEANFILES = \
    lexan.cc \
    parse.cc \
    parse.h

endif
