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
lib_LIBRARIES = libmanager.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/wave/packages/data/src \
	-I$(top_builddir)/engines_gpl/wave/packages/kernel/src \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src

#-------------------------------------------------------------------------------
#Sources
libmanager_a_SOURCES = \
         swan_tot.f90

#-------------------------------------------------------------------------------
#Libraries

