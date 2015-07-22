#-------------------------------------------------------------------------------
#   Makefile for d_hydro - Flow2D3D Component
#
#   Adri.Mourits@Deltares.NL
#   Fedor.Baart@Deltares.NL
#   Irv.Elshoff@Deltares.NL
#   17 nov 11
#
#   Copyright (C) 2011, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

lib_LTLIBRARIES = \
    libtrisim.la

#-------------------------------------------------------------------------------
#   Includes

FINCLUDES += \
    -I$(top_builddir)/utils_lgpl/esmfsm/include \
    -I$(top_builddir)/utils_lgpl/precision/include \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/include

FMODULES += \
    -I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
    -I$(top_builddir)/utils_lgpl/delftio_sync/packages/delftio_sync/src \
    -I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
    -I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src \
    -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
    -I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/src \
    -I$(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/inichk

#-------------------------------------------------------------------------------
#   Sources

libtrisim_la_SOURCES = \
     tdatmain.F90 \
     tricom_init.F90 \
     tricom_step.F90 \
     tricom_finish.F90 \
     triend.F90 \
     tripoi.F90 \
     trisim_mod.F90 \
     trisim.F90

#-------------------------------------------------------------------------------
#   Libraries

libtrisim_la_LIBADD = \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/main/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/non_hydro/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/taylor/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/timedep/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute_roller/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel_dd_f/src/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io/src/preprocessor/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io/src/output/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/inichk/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/dd/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io/src/input/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/kernel/src/general/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/src/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/plugin_user/src/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/plugin_3dmortrafrm_c/src/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/allocation/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/general/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/gdp/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/*.lo \
    $(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics/*.lo \
    $(top_builddir)/utils_lgpl/meteo/packages/meteo/src/libmeteo.a \
    $(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src/libec_module.a \
    $(top_builddir)/utils_lgpl/delftio_sync/packages/delftio_sync/src/libdelftio_sync.a \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90/libdelftio.a \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libdelftio_shm.a \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libfsm.a \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libfsm_c.a \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libesm_c.a \
    $(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src/libinifiles.a \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libnefis.a \
    $(top_builddir)/utils_lgpl/constants/packages/mathconsts/src/libmathconsts.a \
    $(top_builddir)/utils_lgpl/precision/packages/precision/src/libprecision.a \
    $(top_builddir)/utils_lgpl/semaphore/packages/semaphore/src/libsemaphore.a \
    $(top_builddir)/utils_lgpl/stream/packages/stream/src/libstream.a \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libdeltares_common.a \
    $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libdeltares_common_c.a \
    $(top_builddir)/utils_lgpl/delftio_sync/packages/delftio_sync_version_number/src/libdelftio_sync_version_number.a \
    $(top_builddir)/utils_lgpl/delftio/packages/delftio_version_number/src/libdelftio_version_number.a \
    $(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libesmfsm_version_number.a \
    $(top_builddir)/utils_lgpl/meteo/packages/meteo_version_number/src/libmeteo_version_number.a \
    $(top_builddir)/utils_lgpl/nefis/packages/nefis_version_number/src/libnefis_version_number.a \
    $(top_builddir)/utils_lgpl/stream/packages/stream_version_number/src/libstream_version_number.a \
    $(EXTERNALSOURCES) \
    $(MPILIBS) \
    $(FCLIBS)
