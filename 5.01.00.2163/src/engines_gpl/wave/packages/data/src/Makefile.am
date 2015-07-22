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
lib_LIBRARIES = libdata.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src \
	-I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libdata_a_SOURCES = \
         checkmeteoresult_wave.f90 \
         datetime_to_string.f90 \
         utilities.f90 \
         handles.f90 \
         tables.f90 \
         table_handles.f90 \
         wave_data.f90 \
         rdsec.f90 \
         read_grids.f90 \
         swan_input.f90 \
         filldm.f90 \
         hunt.f90 \
         indexx.f90 \
         ipon.f90 \
         mkmap.f90 \
         skcoma.f90 \
         skcomc.f90 \
         sort.f90 \
         swan_flow_grid_maps.f90 \
         sync_flowwave.f90 \
         timdat.f90 \
         flow_data.f90

#-------------------------------------------------------------------------------
#Libraries

