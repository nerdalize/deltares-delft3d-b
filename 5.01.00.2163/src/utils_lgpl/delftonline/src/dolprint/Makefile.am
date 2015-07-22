#-------------------------------------------------------------------------------
#   Makefile for DelftOnline
#
#   Irv.Elshoff@Deltares.NL
#   4 jul 12
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

bin_PROGRAMS = dolprint

dolprint_SOURCES = \
    dolprint.cpp

INCLUDES = \
    -I$(top_builddir)/utils_lgpl/delftonline/include

dolprint_LDADD = \
    ../delftonline/libDelftOnline.la

