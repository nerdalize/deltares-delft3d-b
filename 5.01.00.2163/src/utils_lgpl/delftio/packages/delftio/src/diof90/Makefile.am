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
lib_LIBRARIES = libdelftio.a

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/delftio/include

#-------------------------------------------------------------------------------
#Sources
libdelftio_a_SOURCES = \
         dio_shm_f2c_f.F90 \
         dio-3d-block.F90 \
         dio-prop.F90 \
         dio-ds-config.F90 \
         dio-streams.F90 \
         dio-ds.F90 \
         dio-2dfield-rw.F90 \
         dio-2dfield-shm.F90 \
         dio-ini.F90 \
         dio-plt-rw.F90 \
         dio-plt-f77.F90 \
         dio-plt-shm.F90 \
         dio-sync-support.F90 \
         dio-sync.F90 \
         dio-time-support.F90 \
         open_mi_dio.f90
#-------------------------------------------------------------------------------
#Libraries

