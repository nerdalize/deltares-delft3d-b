#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   26 aug 10
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = delwaq2

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_srcdir)/engines/waq/include \
        -I$(top_srcdir)/engines/waq/packages/waq_kernel/src/waq_kernel \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/mod_timings \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/mod_couplib \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils \
        -I$(top_srcdir)/engines/waq/packages/delftio/src/diof90


#-------------------------------------------------------------------------------
#Sources
delwaq2_SOURCES = block.f \
       d_tr_d3d.f \
       dlwqmain.f \
       dlwqmainprog.f \
       getidentification.f

#-------------------------------------------------------------------------------
# Libraries

#delwaq2_LDFLAGS = \
#    -Wl,-rpath /opt/intel/fc/10.1.015/lib

delwaq2_LDADD = \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/waq_kernel/libwaq_kernel.a \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/waq_process/libwaq_process.a \
    $(top_srcdir)/engines/waq/packages/waq_plugin_wasteload/src/libwaq_plugin_wasteload.a \
    $(top_srcdir)/engines/waq/packages/waq_io/src/waq_io/libwaq_io.a \
    $(top_srcdir)/engines/waq/packages/waq_io/src/proc_preprocess/libproc_preprocess.a \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/bloom/libbloom.a \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/charon/libcharon.a \
    $(top_srcdir)/engines/waq/packages/waq_io/src/waq_io/libwaq_io.a \
    $(top_srcdir)/engines/waq/packages/waq_data/src/proc_data/libproc_data.a \
    $(top_srcdir)/engines/part/packages/kernel_f/src/libkernel_f.a \
    $(top_srcdir)/engines/part/packages/kernel_c/src/libkernel_c.a \
    $(top_srcdir)/engines/part/packages/io_f/src/libio_f.a \
    $(top_srcdir)/engines/part/packages/utils_f/src/libutils_f.a \
    $(top_srcdir)/engines/part/packages/data_f/src/libdata_f.a \
    $(top_srcdir)/engines/part/packages/version_number/src/libpart_version_number.a \
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
    /opt/mpich2/lib/libmpich.a \
    -lpthread

# To be added when using Intel 11:
#    $(FCLIBS)
