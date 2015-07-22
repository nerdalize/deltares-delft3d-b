#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   02 dec 2008
#
#   Copyright (C) 2008, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = stream01test.exe

#-------------------------------------------------------------------------------
#Sources
stream01test_exe_SOURCES = \
	smalltest.cpp

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/stream/include

#-------------------------------------------------------------------------------
#Libraries 

stream01test_exe_LDADD = \
	$(top_builddir)/utils_lgpl/stream/packages/stream/src/libstream.a \
	$(top_builddir)/utils_lgpl/stream/packages/stream_version_number/src/libstream_version_number.a


#	-ldl \
#	$(FCLIBS) -limf
