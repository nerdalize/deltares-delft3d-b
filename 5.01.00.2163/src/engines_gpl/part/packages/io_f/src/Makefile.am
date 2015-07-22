#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libio_f.a

#-------------------------------------------------------------------------------
#Includes
FMODULES = \
	-I../../data_f/src \
	-I../../utils_f/src \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
libio_f_a_SOURCES = \
         dlwqbl.f90 \
         openfl.f90 \
         getdps.f90 \
         part12.f90 \
         part13.f90 \
         part16.f90 \
         parths.f90 \
         rdccol.f90 \
         rdfnam.f90 \
         rdlgri.f90 \
         rdparm.f90 \
         rdpart.f90 \
         report_date_time.f90 \
         wrttrk.f90

#-------------------------------------------------------------------------------
#Libraries

