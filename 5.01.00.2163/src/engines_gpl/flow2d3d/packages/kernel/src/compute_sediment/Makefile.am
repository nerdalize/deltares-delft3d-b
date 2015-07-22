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
lib_LTLIBRARIES = libkernel_compute_sediment.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/utils_lgpl/precision/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/constants/packages/mathconsts/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------
#Sources
libkernel_compute_sediment_la_SOURCES = \
         adjust_bedload.f90 \
         avalan.f90 \
         dredge.f90 \
         bailtr.f90 \
         bedbc1993.f90 \
         bedbc2004.f90 \
         bedtr1993.f90 \
         bedtr2004.f90 \
         bndmorlyr.f90 \
         bott3d.f90 \
         calseddf1993.f90 \
         calseddf2004.f90 \
         compbsskin.f90 \
         compdiam.f90 \
         comphidexp.f90 \
         compsandfrac.f90 \
         compthick.f90 \
         dredgedump.f90 \
         dwnvel.f90 \
         eqtran.f90 \
         erosed.f90 \
         erosilt.f90 \
         fgyint.f90 \
         getfixfac.f90 \
         layers.f90 \
         osmom.f90 \
         red_soursin.f90 \
         shearx.f90 \
         shld.f90 \
         soursin_2d.f90 \
         soursin_3d.f90 \
         termfy.f90 \
         termgy.f90 \
         trab10.f90 \
         trab11.f90 \
         trab12.f90 \
         trab14.f90 \
         trabg.f90 \
         trabwc.f90 \
         trabwc2.f90 \
         tran9t.f90 \
         tranb1.f90 \
         tranb2.f90 \
         tranb3.f90 \
         tranb4.f90 \
         tranb5.f90 \
         tranb7.f90 \
         upwbed.f90 \
         wave.f90 \
         upbdps.f90 \
         updwaqflxsed.f90

#-------------------------------------------------------------------------------
#Libraries

