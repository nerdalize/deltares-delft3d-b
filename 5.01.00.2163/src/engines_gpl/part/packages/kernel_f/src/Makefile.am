#-------------------------------------------------------------------------------
#   Makefile for delpar
#
#   jan.mooiman@deltares.nl
#   09 Feb 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libkernel_f.a

#-------------------------------------------------------------------------------
#Includes
FMODULES = \
	-I../../data_f/src \
	-I../../utils_f/src \
	-I../../io_f/src \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
libkernel_f_a_SOURCES = \
         avundf_linux.f90 \
         oildsp.f90 \
         inipart.f90 \
         part01.f90 \
         part10.f90 \
         plotgr.f90 \ 
         part03.f90 \
         part21.f90 \
         part14.f90 \
         part06.f90 \
         part15.f90 \
         partur.f90 \
         part08.f90 \
         part17.f90 \
         partwq.f90 \
         part09.f90 \
         part18.f90 \
         partvs.f90 \
         partwr.f90

#-------------------------------------------------------------------------------
#Libraries

