#-------------------------------------------------------------------------------
#   Makefile for unit_tests_waq
#
#   arjen.markus@deltares.nl
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = unit_tests_waq

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_srcdir)/utils_lgpl/ftnunit/packages/ftnunit/src

#-------------------------------------------------------------------------------
#Sources
unit_tests_waq_SOURCES = \
       unit_tests_module.f90 \
       unit_tests_waq.f90

#-------------------------------------------------------------------------------
#Libraries
unit_tests_waq_LDADD = \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/waq_kernel/libwaq_kernel.a \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/waq_process/libwaq_process.a \
    $(top_srcdir)/engines/waq/packages/waq_plugin_wasteload/src/libwaq_plugin_wasteload.a \
    $(top_srcdir)/engines/waq/packages/waq_io/src/waq_io/libwaq_io.a \
    $(top_srcdir)/engines/waq/packages/waq_io/src/proc_preprocess/libproc_preprocess.a \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/bloom/libbloom.a \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/charon/libcharon.a \
    $(top_srcdir)/engines/waq/packages/waq_io/src/waq_io/libwaq_io.a \
    $(top_srcdir)/engines/waq/packages/waq_data/src/proc_data/libproc_data.a \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.a \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/mod_couplib/libmod_couplib.a \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/mod_timings/libmod_timings.a \
    $(top_srcdir)/utils_lgpl/delftio/packages/delftio/src/diof90/libdelftio.a \
    $(top_srcdir)/utils_lgpl/delftio/packages/delftio_shm/src/diof90/libdelftio_shm.a \
    $(top_srcdir)/utils_lgpl/delftio/packages/delftio_version_number/src/libdelftio_version_number.a \
    $(top_srcdir)/engines/waq/packages/waq_io/src/ods/libods.a \
    $(top_srcdir)/utils_lgpl/nefis/packages/nefis/src/libnefis.a \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/interact/libinteract.a \
    $(top_srcdir)/engines/waq/packages/waq_utils_c/src/libwaq_utils_c.a \
    $(top_srcdir)/engines/waq/packages/delwaq2_version_number/src/libdelwaq2_version_number.a \
    $(top_srcdir)/utils_lgpl/nefis/packages/nefis_version_number/src/libnefis_version_number.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libfsm.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libfsm_c.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libesm_c.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libesmfsm_version_number.a \
    $(top_srcdir)/utils_lgpl/ftnunit/packages/ftnunit/src/libftnunit.a \
    /opt/mpich2/lib/libmpich.a \
    -lpthread
                                   
