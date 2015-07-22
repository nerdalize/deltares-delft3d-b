#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   26 aug 10
#
#   Copyright (C) 2007-2010, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libkernel_compute.la

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
	-I/opt/mpich2/include \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/utils_lgpl/meteo/packages/meteo/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/constants/packages/mathconsts/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics

#-------------------------------------------------------------------------------
#Sources
libkernel_compute_la_SOURCES = \
         adi.f90 \
         adv2d.f90 \
         bccorr.f90 \
         bedform.f90 \
         c_vort.f90 \
         c_vvv.f90 \
         calbf.f90 \
         calksc.f90 \
         calrou.f90 \
         caltmx.f90 \
         cdwkad.f90 \
         checku.f90 \
         chkadv.f90 \
         cline.f90 \
         cnvbub.f90 \
         comvol.f90 \
         cptdis.f90 \
         cucbp.f90 \
         cucdp.f90 \
         cucnp.f90 \
         culver.f90 \
         curvat.f90 \
         dengra.f90 \
         dens.f90 \
         dens_eck.f90 \
         dens_unes.f90 \
         dersig.f90 \
         detvic.f90 \
         dif_ws.f90 \
         difacr.f90 \
         difhor.f90 \
         difu.f90 \
         difuflux.f90 \
         difuvl.f90 \
         disbub.f90 \
         discha.f90 \
         drotim.f90 \
         drychk.f90 \
         enloss.f90 \
         euler.f90 \
         f0isf1.f90 \
         fallve.f90 \
         filterstructures.f90 \
         forfil.f90 \
         heatu.f90 \
         hydkad.f90 \
         interp.f90 \
         itadi2.f90 \
         layerdep.f90 \
         lpfluc.f90 \
         mom_cw.f90 \
         mom_cyclic.f90 \
         mom_fls.f90 \
         mom_waqua.f90 \
         protke.f90 \
         reddic.f90 \
         redvic.f90 \
         secbou.f90 \
         secrhs.f90 \
         shrwav.f90 \
         slim.f90 \
         sline.f90 \
         sourmu.f90 \
         sousin.f90 \
         stoktb.f90 \
         stress.f90 \
         sud.f90 \
         tabel.f90 \
         taubot.f90 \
         tfzeta.f90 \
         thahbc.f90 \
         trakad.f90 \
         tratur.f90 \
         trid2.f90 \
         tritra.f90 \
         trtrou.f90 \
         tur2d.f90 \
         turclo.f90 \
         ulim.f90 \
         updbar.f90 \
         updmassbal.f90 \
         updwaqflx.f90 \
         upwhu.f90 \
         usrbcc.f90 \
         usrbrl.f90 \
         usrbrl2d.f90 \
         ustar.f90 \
         uzd.f90 \
         vihrov.f90 \
         vihsec.f90 \
         wphys.f90 \
         z_adi.f90 \
         z_checku.f90 \
         z_chkadv.f90 \
         z_cucbp_nhfull.f90 \
         z_cucnp.f90 \
         z_dengra.f90 \
         z_difhor_nhfull.f90 \
         z_difu.f90 \
         z_difuflux.f90 \
         z_disbub.f90 \
         z_discha.f90 \
         z_drychk.f90 \
         z_drychku.f90 \
         z_forfil.f90 \
         z_hormom_finvol.f90 \
         z_hormom_fls.f90 \
         z_hormom_iupw.f90 \
         z_hormom_mdue.f90 \
         z_hormom_mdui.f90 \
         z_predict_nhfull.f90 \
         z_shrwav.f90 \
         z_sud.f90 \
         z_tratur.f90 \
         z_turclo.f90 \
         z_updtvol.f90 \
         z_uzd.f90 \
         z_vermom_finvol.f90 \
         z_vihrov.f90

#-------------------------------------------------------------------------------
#Libraries

