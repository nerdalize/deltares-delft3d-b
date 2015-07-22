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
lib_LIBRARIES = libinifiles.a
#-------------------------------------------------------------------------------
#Sources
libinifiles_a_SOURCES = \
         inifiles_version.F90 \
         tree_struct.f90 \
         properties.f90

EXTRA_DIST = \
	inifiles_version.F90.svn

BUILT_SOURCES = ./always \
    ./inifiles_version.F90 

./always:
	rm -rf ./inifiles_version.F90 

./inifiles_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/inifiles_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)

