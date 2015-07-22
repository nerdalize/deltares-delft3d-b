#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   26 aug 10
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libmod_couplib.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I/opt/mpich2/include \
        -I$(top_srcdir)/engines/waq/include \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/mod_timings

#-------------------------------------------------------------------------------
#Sources
libmod_couplib_a_SOURCES = \
       m_coupcns.f90 \
       m_ixset.f90 \
       m_prcgrp.f90 \
       m_intfc.f90 \
       m_sndrcv.f90  \
       m_globcomm.f90 \
       reshape_idata.f90 \
       reshape_rdata.f90 \
       m_couplib.f90 \
       reshape_ddata.f90

#-------------------------------------------------------------------------------
# Libraries
