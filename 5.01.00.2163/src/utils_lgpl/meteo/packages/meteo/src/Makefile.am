#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libmeteo.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src

#-------------------------------------------------------------------------------
#Sources
libmeteo_a_SOURCES = \
         meteo_data.f90 \
         distance2.f90 \
         meteo_read.f90 \
         meteo.f90 \
         checkmeteoheader.f90 \
         numbersonline_m.f90 \
         openexistingfile_meteo.f90 \
         readmeteoheader.f90

#-------------------------------------------------------------------------------
#Libraries

