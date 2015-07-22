#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libwaq_process.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	 -I$(top_srcdir)/engines/waq/include \
	 -I$(top_srcdir)/engines/waq/packages/waq_data/src/proc_data \
	 -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/mod_timings \
	 -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
libwaq_process_a_SOURCES = \
       bottom_set.f \
       adspo4.f \
       advtra.f \
       apatit.f \
       atmdep.f \
       bacmrt.f \
       bodcod.f \
       botmin.f \
       burial.f \
       calchz.f \
       calsed.f \
       caltau.f \
       calwav.f \
       clcrad.f \
       consbl.f \
       cselac.f \
       d40blo.f \
       dayl.f \
       dayrad.f \
       ddepth.f \
       decbod.f \
       decdet.f \
       decpc5.f \
       degmp.f \
       densed.f \
       denwat.f \
       depave.f \
       diggin.f \
       dissi.f \
       dlalg.f \
       dmvol.f \
       dsptra.f \
       dsurf.f \
       ebuch4.f \
       emersi.f \
       extina.f \
       extinc.f \
       flxfrc.f90 \
       harves.f \
       hdisp.f \
       hdispv.f \
       heatfl.f \
       intpol.f \
       makpoc.f \
       meteo.f \
       methox.f \
       nh3fre.f \
       nitrif.f \
       nlalg.f \
       nralgs.f \
       nutrel.f \
       nutupt.f \
       oxymin.f \
       partmp.f \
       phcomp.f \
       phcomb.f \
       posoxy.f \
       pprlim.f \
       pripro.f \
       procal.f \
       pronrs.f \
       ptewor.f \
       radalg.f \
       rdbalg.f \
       rear.f \
       resant.f \
       resdm.f \
       restim.f \
       rfpart.f \
       s12tra.f \
       salchl.f \
       satch4.f \
       satco2.f \
       satoxy.f \
       sdppro.f \
       secchi.f \
       sedaap.f \
       sedcar.f \
       sedcom.f \
       sedhm.f \
       sednut.f \
       sedomv.f \
       sedox.f \
       sedsod.f \
       set_effi.f \
       simph.f \
       somsed.f \
       ssedph.f \
       staday.f \
       stadpt.f \
       stadsc.f \
       stageo.f \
       staprc.f \
       staqtl.f \
       stox3d.f \
       strear.f \
       sulfox.f \
       sulfpr.f \
       sumfrc.f90 \
       swoxy.f \
       temper.f \
       tfalg.f \
       totdep.f \
       trase2.f \
       trcoef.f \
       trsoxy.f \
       uit_zi.f \
       ulfix.f \
       varoxy.f \
       varsal.f \
       veg2dn.f \
       veg3du.f \
       veg3dx.f \
       vervlu.f \
       vivian.f \
       vtrans.f \
       watage.f \
       wkcomp.f \
       zerome.f \
       ironox.f90 \
       ironre.f90 \
       priron.f90 \
       spcarb.f90 \
       specfe.f90 \
       sulfid.f90 \
       sulpho.f90 \
       vbgro.f90 \
       vbmrt.f90 \
       vbstat.f90 \
       vbupt.f90 \
       veloc.f90
	   
#-------------------------------------------------------------------------------
# Libraries
