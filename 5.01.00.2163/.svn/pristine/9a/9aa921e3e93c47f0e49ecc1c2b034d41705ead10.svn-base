#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS = wave.exe

FCLIBS += \
	-lpthread

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/engines_gpl/wave/packages/wave/include

FMODULES += \
	-I$(top_builddir)/engines_gpl/wave/packages/data/src \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src

#-------------------------------------------------------------------------------
#Sources
wave_exe_SOURCES = \
         checklicense.f90 \
         usage.f90 \
         wave_version.F90 \
         wave.f90

#-------------------------------------------------------------------------------
#Libraries

wave_exe_LDADD   = \
$(top_builddir)/engines_gpl/wave/packages/manager/src/libmanager.a \
$(top_builddir)/engines_gpl/wave/packages/kernel/src/libkernel.a \
$(top_builddir)/engines_gpl/wave/packages/io/src/libio.a \
$(top_builddir)/engines_gpl/wave/packages/data/src/libdata.a \
$(top_builddir)/engines_gpl/wave/packages/io/src/libio.a \
$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90/libdelftio.a \
$(top_builddir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libdelftio_shm.a \
$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libfsm.a \
$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libfsm_c.a \
$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libesm_c.a \
$(top_builddir)/utils_lgpl/meteo/packages/meteo/src/libmeteo.a \
$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src/libinifiles.a \
$(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libnefis.a \
$(top_builddir)/utils_lgpl/precision/packages/precision/src/libprecision.a \
$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libdeltares_common.a \
$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libdeltares_common_c.a \
$(top_builddir)/utils_lgpl/meteo/packages/meteo_version_number/src/libmeteo_version_number.a \
$(top_builddir)/utils_lgpl/delftio/packages/delftio_version_number/src/libdelftio_version_number.a \
$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libesmfsm_version_number.a \
$(top_builddir)/utils_lgpl/nefis/packages/nefis_version_number/src/libnefis_version_number.a \
$(EXTERNALSOURCES) \
$(FCLIBS)


BUILT_SOURCES = ./always \
	./wave_version.F90 

./always:
	rm -rf ./wave_version.F90 

./wave_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/wave_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)
  
