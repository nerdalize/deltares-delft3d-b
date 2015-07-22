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
lib_LIBRARIES = libkernel.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/wave/packages/data/src \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src
	
#-------------------------------------------------------------------------------
#Sources
libkernel_a_SOURCES = \
         corrht.f90 \
         check_input.f90 \
         del_temp_files.f90 \
         dircor.f90 \
         fxfydr.f90 \
         grids_and_gridmaps.f90 \
         grmap.f90 \
         map_swan_output.f90 \
         new_lun.f90 \
         perpar.f90 \
         rm_del.f90 \
         run_swan.f90 \
         small.f90 \
         update_wavecond.f90 \
         wapar.f90 \
         wave_init.f90 \
         wave2com.f90 \
         wave2flow.f90

#-------------------------------------------------------------------------------
#Libraries

