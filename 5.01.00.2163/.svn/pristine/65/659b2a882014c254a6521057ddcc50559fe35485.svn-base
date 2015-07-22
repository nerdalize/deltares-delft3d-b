#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libproc_preprocess.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_srcdir)/engines/waq/include \
	-I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
libproc_preprocess_a_SOURCES = partab.f90 \
       actrep.f \
       add_atrfrc.f \
       add_dspfrc.f \
       add_flxfrc.f \
       add_prcfrc.f \
       add_sumfrc.f \
       algrep.f \
       blmeff.f \
       cnfrep.f \
       cnvpro.f \
       dhpfil.f \
       dhrep2.f \
       dhrepc.f \
       dlwqp1.f \
       dlwqs1.f \
       expand_frc.f \
       fill_old_items.f \
       get_sfrac.f \
       getinv.f \
       intoou.f \
       makbar.f \
       primpro.f \
       proc_totals.f \
       prprop.f \
       prsort.f \
       rbpdf2.f \
       rd_filid.f \
       rd_tabm1.f \
       rd_tabp1.f \
       rd_tabp2.f \
       rd_tabp3.f \
       rd_tabp4.f \
       rd_tabp5.f \
       rd_tabr1.f \
       rd_tabr2.f \
       rd_tabr3.f \
       rd_tabr4.f \
       rd_tabr5.f \
       rd_tabr6.f \
       rd_tabr7.f \
       rd_tabr8.f \
       rd_tabs.f \
       rdstat.f \
       rdwrko.f \
       reaalg.f \
       repuse.f \
       set_active.f \
       set_fractions.f \
       set_old_items.f \
       set_stat_output.f \
       setday.f \
       setdpt.f \
       setdsc.f \
       setdvp.f \
       setgeo.f \
       setgr2.f \
       setgrd.f \
       setopo.f \
       setopp.f \
       setprc.f \
       setprg.f \
       setqtl.f \
       setvat.f \
       tabdim.f \
       timers.f \
       valpoi.f \
       vxlpoi.f \
       wr_proceswrk.f \
       wripro.f \
       wrstoc.f \
       wrtoys.f \
       wrwrko.f

#-------------------------------------------------------------------------------
# Libraries
