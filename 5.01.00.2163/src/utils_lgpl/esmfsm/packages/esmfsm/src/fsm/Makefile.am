#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libfsm.a

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src

#-------------------------------------------------------------------------------
#Sources
libfsm_a_SOURCES = \
         fortapi.f90

#-------------------------------------------------------------------------------
#Libraries

