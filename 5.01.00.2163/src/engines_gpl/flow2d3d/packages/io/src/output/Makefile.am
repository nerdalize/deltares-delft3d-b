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
lib_LTLIBRARIES = libio_output.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I$(top_builddir)/utils_lgpl/esmfsm/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/precision/include \
	-I$(top_builddir)/utils_lgpl/constants/packages/mathconsts/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/utils_lgpl/delftio/packages/delftio/src/diof90 \
	-I$(top_builddir)/utils_lgpl/delftio_sync/packages/delftio_sync/src \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/general


#-------------------------------------------------------------------------------
#Sources
libio_output_la_SOURCES = \
         addelm.f90 \
         calbed.f90 \
         defnewgrp.f90 \
         dirint.f90 \
         dp999.f90 \
         filldm.f90 \
         fouana.f90 \
         fouvecmax.f90 \
         frdint.f90 \
         getfpt.f90 \
         inippr.f90 \
         findnm_kcs.f90 \
         findnm_kcs_flowwrapper.f90 \
         ksieta2xy.f90 \
         layer_interfaces.f90 \
         pinpol.f90 \
         postpr.f90 \
         postpr_hdt.f90 \
         pppsol.f90 \
         prhead.f90 \
         prihis.f90 \
         prthis.f90 \
         prtmap.f90 \
         putgtc.f90 \
         putgti.f90 \
         putgtl.f90 \
         putgtr.f90 \
         rdtimc.f90 \
         rdtimw.f90 \
         realfileversion.f90 \
         rstcom.f90 \
         rwbotc.f90 \
         rwbotc_double.f90 \
         setwav.f90 \
         tcross.f90 \
         tstat.f90 \
         tstat_bed.f90 \
         update_stat_locations.f90 \
         updbnddps.f90 \
         uv2zeta.f90 \
         waq2flow.f90 \
         wavenr.f90 \
         wrboun.f90 \
         wrcomi.f90 \
         wrcomt.f90 \
         wrcomwind.f90 \
         wrcurt.f90 \
         wrdwqt.f90 \
         wrfou.f90 \
         wrfous.f90 \
         wrfouv.f90 \
         wrgrid.f90 \
         wribot.f90 \
         wridoc.f90 \
         wridro.f90 \
         wrihis.f90 \
         wrihisbal.f90 \
         wrihisdad.f90 \
         wrihisdis.f90 \
         wrimap.f90 \
         wrirst.f90 \
         wrkenc.f90 \
         wrkent.f90 \
         wrmorm.f90 \
         wrmorm1.f90 \
         wrmorm2.f90 \
         wrparm.f90 \
         wrplot.f90 \
         wrrolm.f90 \
         wrrouf.f90 \
         wrsedd.f90 \
         wrsedh.f90 \
         wrsedm.f90 \
         wrsedmavg.f90 \
         wrsedmgrp.f90 \
         wrsedwaqm.f90 \
         wrspcp.f90 \
         wrtdro.f90 \
         wrthis.f90 \
         wrthisbal.f90 \
         wrthisdad.f90 \
         wrthisdis.f90 \
         wrtmap.f90 \
         wrwaqapnt.f90 \
         wrwaqbal.f90 \
         wrwaqcco.F90 \
         wrwaqfil.F90 \
         wrwaqflo.f90 \
         wrwaqhyd.f90 \
         wrwaqld0.f90 \
         wrwaqlenaggr.f90 \
         wrwaqlennoag.f90 \
         wrwaqload.f90 \
         wrwaqmakpnt.f90 \
         wrwaqnpnt.f90 \
         wrwaqpnt.F90 \
         wrwaqsrf.F90 \
         wrwaqvol.f90 \
         wrwavh.f90 \
         wrxbm.f90 \
         zrtc.f90

#-------------------------------------------------------------------------------
#Libraries

