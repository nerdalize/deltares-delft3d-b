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
lib_LTLIBRARIES = libkernel_inichk.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/utils_lgpl/precision/include \
	-I$(top_builddir)/utils_lgpl/parallel_mpi/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/general \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/io_dol_f/src \
	-I$(top_builddir)/utils_lgpl/constants/packages/mathconsts/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src

#-------------------------------------------------------------------------------
#Sources
libkernel_inichk_la_SOURCES = \
         allocadv2d.f90 \
         caldps.f90 \
         caldpu.f90 \
         calpsh.f90 \
         chkbnd.f90 \
         chkcom.f90 \
         chkdis.f90 \
         chkdro.f90 \
         chkdry.f90 \
         chkgeo.f90 \
         chkic.f90 \
         chkiwe.f90 \
         chkkc.f90 \
         chklod.f90 \
         chknum.f90 \
         chkphy.f90 \
         chkset.f90 \
         chksit.f90 \
         chkstr.f90 \
         chktim.f90 \
         chktra.f90 \
         chktrt.f90 \
         chkvic.f90 \
         creategridborder.f90 \
         d3d_in_openda.f90 \
         getdx.f90 \
         getdy.f90 \
         inchki.f90 \
         inchkr.f90 \
         increm.f90 \
         inibcc.f90 \
         inibcparl.f90 \
         inibcq.f90 \
         inibct.f90 \
         inibnd.f90 \
         inicdw.f90 \
         inidis.f90 \
         inieva.f90 \
         inigeo.f90 \
         inigrd.f90 \
         inimorlyr.f90 \
         iniphy.f90 \
         inirgl.f90 \
         inised.f90 \
         initau.f90 \
         initem.f90 \
         initgf.f90 \
         initmerge.f90 \
         initur.f90 \
         inivol.f90 \
         iwecof.f90 \
         mirror_bnd.f90 \
         tkecof.f90 \
         xyder.f90 \
         z_chkdry.f90 \
         z_initur.f90 \
         z_inizm.f90 \
         z_kfmnmx.f90

#-------------------------------------------------------------------------------
#Libraries

