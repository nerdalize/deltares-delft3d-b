#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = delwaq1

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_srcdir)/engines/waq/include \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
delwaq1_SOURCES = block.f \
       dlwq00.f \
       getidentification.f

#-------------------------------------------------------------------------------
# Libraries

delwaq1_LDADD = \
    $(top_srcdir)/engines/waq/packages/waq_io/src/proc_preprocess/libproc_preprocess.a \
    $(top_srcdir)/engines/waq/packages/waq_kernel/src/charon/libcharon.a \
    $(top_srcdir)/engines/waq/packages/waq_io/src/waq_io/libwaq_io.a \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils/libwaq_utils.a \
    $(top_srcdir)/engines/waq/packages/waq_io/src/ods/libods.a \
    $(top_srcdir)/engines/part/packages/kernel_f/src/libkernel_f.a \
    $(top_srcdir)/engines/part/packages/kernel_c/src/libkernel_c.a \
    $(top_srcdir)/engines/part/packages/io_f/src/libio_f.a \
    $(top_srcdir)/engines/part/packages/utils_f/src/libutils_f.a \
    $(top_srcdir)/engines/part/packages/data_f/src/libdata_f.a \
    $(top_srcdir)/engines/part/packages/version_number/src/libpart_version_number.a \
    $(top_srcdir)/utils_lgpl/nefis/packages/nefis/src/libnefis.a \
    $(top_srcdir)/engines/waq/packages/waq_utils_f/src/interact/libinteract.a \
    $(top_srcdir)/engines/waq/packages/delwaq1_version_number/src/libdelwaq1_version_number.a \
    $(top_srcdir)/utils_lgpl/nefis/packages/nefis_version_number/src/libnefis_version_number.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libfsm.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libfsm_c.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libesm_c.a \
    $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libesmfsm_version_number.a \
    -lpthread
