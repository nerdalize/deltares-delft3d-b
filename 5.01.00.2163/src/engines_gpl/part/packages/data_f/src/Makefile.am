#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libdata_f.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
libdata_f_a_SOURCES = \
         precision.f90 \
         typos_mod.f90 \
         file_info.f90 \
         modeldim.f90 \
         partmem.f90 \
         global_data.f90 \
         alloc.f90 \
         dealloc.f90 \
         dealloc_part.f90

#-------------------------------------------------------------------------------
#Libraries

