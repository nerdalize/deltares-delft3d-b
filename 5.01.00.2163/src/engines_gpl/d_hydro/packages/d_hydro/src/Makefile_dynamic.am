#-------------------------------------------------------------------------------
#   Makefile for d_hydro
#
#   Irv.Elshoff@Deltares.NL
#   Adri.Mourits@Deltares.NL
#   29 oct 11
#
#   Copyright (C) 2011, Deltares
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am


bin_PROGRAMS = \
    d_hydro.exe

d_hydro_exe_SOURCES = \
    d_hydro_version.cpp \
    d_hydro.cpp


INCLUDES = \
    -I$(STANDARD_C_INCLUDES) \
    -I../include


#-------------------------------------------------------------------------------
#   Libraries


d_hydro_exe_LDADD = \
    $(top_builddir)/utils_lgpl/d_hydro_lib/packages/d_hydro_lib/src/libD_hydro.a \
    -ldl

#    -lmcheck


#-------------------------------------------------------------------------------

BUILT_SOURCES = \
    ./always \
    ../include/d_hydro_version.h

./always:
	rm -f ../include/d_hydro_version.h

../include/d_hydro_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh ../include/d_hydro_version.h $(srcdir) ../include/version_number.ini $(top_builddir)

