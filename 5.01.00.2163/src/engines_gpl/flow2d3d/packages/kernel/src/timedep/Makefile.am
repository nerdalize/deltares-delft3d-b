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
lib_LTLIBRARIES = libkernel_timedep.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_builddir)/utils_lgpl/delftio_sync/packages/delftio_sync/src \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/constants/packages/mathconsts/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/inichk

#-------------------------------------------------------------------------------
#Sources
libkernel_timedep_la_SOURCES = \
         cofrbc.f90 \
         eoferr.f90 \
         incbc.f90 \
         incbcc.f90 \
         incdis.f90 \
         inceva.f90 \
         incmeteo.f90 \
         incrbc.f90 \
         inctem.f90 \
         incwav.f90 \
         rtc_comm_get.f90 \
         rtc_comm_init.f90 \
         rtc_comm_put.f90 \
         update_nodal_factors.f90 \
         updbcc.f90 \
         updbcq.f90 \
         updbct.f90 \
         upddis.f90 \
         upddpmveg.f90 \
         updeva.f90 \
         updtem.f90 \
         wetdis.f90 \
         windtogridc.f90 \
         windtostress.f90

#-------------------------------------------------------------------------------
#Libraries

