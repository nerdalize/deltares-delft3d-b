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
lib_LIBRARIES = libio.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/wave/packages/data/src \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src

#-------------------------------------------------------------------------------
#Sources
libio_a_SOURCES = \
         compvel.f90 \
         cp_file.f90 \
         dam_cod.f90 \
         deletehotfile.f90 \
         flow2wav.f90 \
         get_cur.f90 \
         get_dep.f90 \
         get_flow_fields.f90 \
         get_lev.f90 \
         get_params.f90 \
         get_swan_depth.f90 \
         get_vegi_map.f90 \
         get_visc.f90 \
         get_wind.f90 \
         noextspaces.f90 \
         out2d.f90 \
         put_wave_fields.f90 \
         putgtc.f90 \
         putgti.f90 \
         putgtl.f90 \
         putgtr.f90 \
         read_swan_output.f90 \
         readn.f90 \
         scan_fl.f90 \
         wavcur2d.f90 \
         wavenr.f90 \
         write_bnd_loc.f90 \
         write_swan_datafile.f90 \
         write_wave_map.f90 \
         write_wave_map_wind.f90 \
         writek.f90 \
         wrivek.f90 \
         wrpara.f90

#-------------------------------------------------------------------------------
#Libraries

