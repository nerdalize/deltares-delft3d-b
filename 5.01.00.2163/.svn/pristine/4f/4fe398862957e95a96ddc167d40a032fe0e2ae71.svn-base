#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   11 jan 2007
#
#   Copyright (C) 2011, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libio_input.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/utils_lgpl/precision/include \
	-I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/constants/packages/mathconsts/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src
#-------------------------------------------------------------------------------
#Sources
libio_input_la_SOURCES = \
         barfil.f90 \
         bndfil.f90 \
         bubfil.f90 \
         ciofil.f90 \
         ck_dpopt.f90 \
         cmpnum.f90 \
         polygon_module.f90 \
         depfil.f90 \
         depfil_double.f90 \
         dimbch.f90 \
         dimbnd.f90 \
         dimbub.f90 \
         dimdis.f90 \
         dimfou.f90 \
         dimiwe.f90 \
         dimmud.f90 \
         dimpro.f90 \
         dimrd.f90 \
         dimsedconst.f90 \
         dimsit.f90 \
         dimstr.f90 \
         dimtrt.f90 \
         drofil.f90 \
         dryfil.f90 \
         edyfil.f90 \
         expblk.f90 \
         fbcorr.f90 \
         flow_nefis_restart.f90 \
         flsfil.f90 \
         grdfil.f90 \
         hybfil.f90 \
         icfil.f90 \
         inicut.f90 \
         inimet.f90 \
         morbndfill.f90 \
         numcmp.f90 \
         rdbedformpar.f90 \
         rdbcg.f90 \
         rdbndd.f90 \
         rdcul.f90 \
         rddept.f90 \
         rddpmveg.f90 \
         rddredge.f90 \
         rdfcio.f90 \
         rdfour.f90 \
         rdgrid.f90 \
         rdhyb.f90 \
         rdhyg.f90 \
         rdhyvd.f90 \
         rdibch.f90 \
         rdic.f90 \
         rdinimorlyr.f90 \
         rdirt.f90 \
         rdiwe.f90 \
         rdlaydis.f90 \
         rdmassbal.f90 \
         rdmeteo.f90 \
         rdmor.f90 \
         rdmor0.f90 \
         rdmor1.f90 \
         rdmorlyr.f90 \
         rdmud.f90 \
         rdnum.f90 \
         rdprfl.f90 \
         rdproc.f90 \
         rdq2eb.f90 \
         rdrgf.f90 \
         rdroll.f90 \
         rdrund.f90 \
         rdscour.f90 \
         rdsed.f90 \
         rdsed0.f90 \
         rdsed1.f90 \
         rdsite.f90 \
         rdspec.f90 \
         rdstru.f90 \
         rdtimo.f90 \
         rdtrafrm.f90 \
         rdtrafrm0.f90 \
         rdtrt.f90 \
         rdttar.f90 \
         rdusrc.f90 \
         rdusrf.f90 \
         rdusrp.f90 \
         rdwaqpar.f90 \
         rdxyzo.f90 \
         roller_nefis_restart.f90 \
         reacmp.f90 \
         readmd.f90 \
         read_triana.f90 \
         reafou.f90 \
         restart_bdf_from_trim.f90 \
         restart_bodsed.f90 \
         restart_lyrs.f90 \
         rstfil.f90 \
         srcfil.f90 \
         stafil.f90 \
         strfil.f90 \
         tdfil.f90 \
         trafil.f90 \
         search2.f90

#-------------------------------------------------------------------------------
#Libraries

