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
lib_LIBRARIES = libdelftio_shm.a

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I$(top_builddir)/utils_lgpl/delftio/packages/delftio_shm/include \
	-I$(top_builddir)/utils_lgpl/esmfsm/include

#-------------------------------------------------------------------------------
#Sources
libdelftio_shm_a_SOURCES = \
         dio-sync-ux.cpp \
         dio_shm.cpp \
         dio_shm_datablock.cpp \
         dio_shm_f2c_c.cpp \
         dio_shm_handle.cpp \
         dio_shm_sync.cpp

#-------------------------------------------------------------------------------
#Libraries

