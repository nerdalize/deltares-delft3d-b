#-------------------------------------------------------------------------------
#   Makefile for d_hydro_lib
#
#   Irv.Elshoff@deltares.nl
#   2 jun 11
#
#   Copyright (C) 2011, Deltares
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

lib_LIBRARIES = libD_hydro.a

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
        -I../include \
        -I../../../include

#-------------------------------------------------------------------------------
#Sources
libD_hydro_a_SOURCES = \
    coredump.cpp \
    clock.cpp \
    component.cpp \
    d_hydro_lib_version.cpp \
    exception.cpp \
    log.cpp \
    stringutils.cpp \
    xmltree.cpp

BUILT_SOURCES = ./always \
	../include/d_hydro_lib_version.h

./always:
	rm -rf ../include/d_hydro_lib_version.h

../include/d_hydro_lib_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh ../include/d_hydro_lib_version.h $(srcdir) ../include/version_number.ini $(top_builddir)

#-------------------------------------------------------------------------------
#Libraries
