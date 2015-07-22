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
lib_LIBRARIES = libmathconsts.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src

#-------------------------------------------------------------------------------
#Sources
libmathconsts_a_SOURCES = \
         mathconsts_version.F90 \
         mathconsts.f90


BUILT_SOURCES = ./always \
	./mathconsts_version.F90 

./always:
	rm -rf ./mathconsts_version.F90 

./mathconsts_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/mathconsts_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)

