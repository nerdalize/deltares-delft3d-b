#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = mormerge.exe

#-------------------------------------------------------------------------------
#Includes

FMODULES += \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src

#-------------------------------------------------------------------------------
#Sources
mormerge_exe_SOURCES = \
         mormerge.f90 \
         merge.f90

#-------------------------------------------------------------------------------
#Libraries

mormerge_exe_LDADD   = \
$(top_builddir)/tools_gpl/mormerge/packages/mormerge_version_number/src/libmormerge_version_number.a \
$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src/libinifiles.a \
$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libdeltares_common.a \
$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libdeltares_common_c.a \
$(top_builddir)/utils_lgpl/semaphore/packages/semaphore/src/libsemaphore.a \
$(top_builddir)/utils_lgpl/stream/packages/stream/src/libstream.a \
$(top_builddir)/utils_lgpl/stream/packages/stream_version_number/src/libstream_version_number.a \
$(FCLIBS)
