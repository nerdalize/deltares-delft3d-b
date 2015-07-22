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
lib_LTLIBRARIES = libio_dol_f.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/utils_lgpl/precision/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libio_dol_f_la_SOURCES = \
         d3d_olv_class.f90 \
         d3donline.f90 \
         d3dpublish.f90


#-------------------------------------------------------------------------------
#Libraries

