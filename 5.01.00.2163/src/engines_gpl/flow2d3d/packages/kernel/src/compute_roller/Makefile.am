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
lib_LTLIBRARIES = libkernel_compute_roller.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/constants/packages/mathconsts/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libkernel_compute_roller_la_SOURCES = \
         constwave.f90 \
         difuwe.f90 \
         hds.f90 \
         hds_wf.f90 \
         le.f90 \
         massfl.f90 \
         orbvel.f90 \
         qkwbn.f90 \
         qkwcg.f90 \
         radstr.f90 \
         rbsig.f90 \
         rolcor.f90 \
         rollu.f90 \
         snel.f90 \
         tapf.f90 \
         turwav.f90 \
         varcon.f90 \
         waveu.f90 \
         wnr.f90

#-------------------------------------------------------------------------------
#Libraries

