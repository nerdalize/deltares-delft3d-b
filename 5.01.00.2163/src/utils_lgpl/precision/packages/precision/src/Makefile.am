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
lib_LIBRARIES = libprecision.a

#-------------------------------------------------------------------------------
#Includes

#-------------------------------------------------------------------------------
#Sources
libprecision_a_SOURCES = \
         precision_version.F90 \
         precision_basics.F90 \
         precision.F90 \
         precision_sp.F90

EXTRA_DIST = \
	precision_version.F90.svn

BUILT_SOURCES = ./always \
	./precision_version.F90 

./always:
	rm -rf ./precision_version.F90 

./precision_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/precision_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)

