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
lib_LIBRARIES = libdiof90Nefis.a

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/delftio/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90

#-------------------------------------------------------------------------------
#Sources
libdiof90Nefis_a_SOURCES = \
         dio-2dfield-nefis.F90 \
         dio-3d-block-nefis.F90 \
         dio-check-nefis-error \
         dio-const-rw.F90 \
         dio-plt-nefis.F90

#-------------------------------------------------------------------------------
#Libraries

