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
lib_LIBRARIES = libdelftio_sync.a

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/delftio/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src

#-------------------------------------------------------------------------------
#Sources
libdelftio_sync_a_SOURCES = \
         d3d_sobek_conf.f90 \
         d3d_sobek.f90 \
         d3s_get_discharges.f90 \
         d3s_put_levels.f90 \
         d3s_setup.f90 \
         putget_dio.f90 \
         sync_couple.f90 \
         sync_flm.f90 \
         sync_flowwave.f90 \
         sync_rtcflow.f90

#-------------------------------------------------------------------------------
#Libraries

