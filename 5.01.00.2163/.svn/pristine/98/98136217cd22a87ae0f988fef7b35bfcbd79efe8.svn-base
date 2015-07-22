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
lib_LTLIBRARIES = libio_preprocessor.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics

#-------------------------------------------------------------------------------
#Sources
libio_preprocessor_la_SOURCES = \
         asc.f90 \
         bchfil.f90 \
         bewvuf.f90 \
         datumi.f90 \
         hulpgr.f90 \
         kompbs.f90 \
         rdbcb.f90 \
         rdbcc.f90 \
         rdbch.f90 \
         rdbcq.f90 \
         rdbct.f90 \
         rddis.f90 \
         rdeva.f90 \
         rdheat.f90 \
         rdnamc.f90 \
         rdqh.f90 \
         rdtdc.f90 \
         rdtdcn.f90 \
         rdtdd.f90 \
         rdtddn.f90 \
         rdtdf.f90 \
         rdtdt.f90 \
         rdtdtn.f90 \
         rdtold.f90 \
         rwbcb.f90 \
         tdatom.f90 \
         wrtbch.f90

#-------------------------------------------------------------------------------
#Libraries

