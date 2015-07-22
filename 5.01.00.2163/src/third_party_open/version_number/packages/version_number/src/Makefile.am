#-------------------------------------------------------------------------------
#   Makefile for version_number
#
#   Adri.Mourits@deltares.nl
#
#   Copyright (C) 2008, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_PROGRAMS = version_number.exe

#-------------------------------------------------------------------------------
#Includes

#-------------------------------------------------------------------------------
#Sources
version_number_exe_SOURCES = \
	version_number.f90
all-local: version_number.exe
	${INSTALL} -d $(top_builddir)/third_party_open/version_number/bin/linux
	${INSTALL}    $(top_builddir)/third_party_open/version_number/packages/version_number/src/version_number.exe $(top_builddir)/third_party_open/version_number/bin/linux

