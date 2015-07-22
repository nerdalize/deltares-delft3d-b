#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@wldelft.nl
#   Adri.Mourits@wldelft.nl
#   Fedor.Baart@wldelft.nl
#   11 jan 2007
#
#   Copyright (C) 2007, WL | Delft Hydraulics
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libdata_general.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libdata_general_la_SOURCES = \
         griddims.f90

#-------------------------------------------------------------------------------
#Libraries

