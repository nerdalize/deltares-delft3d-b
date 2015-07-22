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
lib_LTLIBRARIES = libkernel_taylor.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libkernel_taylor_la_SOURCES = \
         ampiwe.f90 \
         bediwe.f90 \
         brkbed.f90 \
         brksur.f90 \
         disper.f90 \
         distri.f90 \
         intpol.f90 \
         iwe_00.f90 \
         seliwe.f90 \
         suriwe.f90 \
         taylor.f90 \
         tg.f90 \
         totiwe.f90 \
         turiwe.f90 \
         zbrent.f90

#-------------------------------------------------------------------------------
#Libraries

