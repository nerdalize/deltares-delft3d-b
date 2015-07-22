#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libwaq_utils.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_srcdir)/engines/waq/include

#-------------------------------------------------------------------------------
#Sources
libwaq_utils_a_SOURCES = \
       waqmem.f90 \
       progress.f90 \
       timers.f \
       fileutils.f90 \
       dlwq1_grid_mod.f \
       dlwq_data.f \
       dwq_proces_set.f \
       dhcommand.f90 \
       DHGARG.F \
       DHHASP.F \
       DHSTOP.F \
       DHTIME.F \
       SRSTOP.F \
       UNLOCK.F \
       avundf_linux.f \
       cnvper.f \
       cnvtim.f \
       d00sub.f \
       dattim.f \
       delwaq_loads.f \
       dhagg2.f \
       dhaggr.f \
       dhagkm.f \
       dhc20m.f \
       dhcarg.f \
       dhcmov.f \
       dhctim.f \
       dhcwrd.f \
       dhdag2.f \
       dhdagg.f \
       dhdelf.f \
       dherrs.f \
       dhfext.f \
       dhggd.f \
       dhgnam.f \
       dhgpoi.f \
       dhgvar.f \
       dhgwrd.f \
       dhimov.f \
       dhisys.f \
       dhkmrk.f \
       dhkmst.f \
       dhlic.f \
       partition_arrays.f90 \
       dhmmar.f \
       dhmmca.f \
       dhmmja.f \
       dhmmra.f \
       dhltim.f \
       dhnlun.f \
       dhnolay.f \
       dhnoseg.f \
       dhopnf.F \
       dhpath.f \
       dhralloc.f \
       dhrmis.f \
       dhscal.f \
       dhslen.f \
       dhsuma.f \
       dhswtr.f \
       dhucas.f \
       dhzerc.f \
       dhzeri.f \
       dlwqj1.f \
       errsys.f \
       fffind.f \
       filldm.f \
       getcom.f \
       getper.f \
       gettok.f \
       gkwini.f \
       makpnt.f \
       mestok.f \
       monsys.f \
       movchr.f \
       move.f \
       movint.f \
       movrl.f \
       output_mod.f \
       putget.f \
       putgtc.f \
       putper.f \
       rapdef.f \
       rbpdef.f \
       rd_token.f90 \
       rdvers.f \
       rdwrk3.f \
       rdwrk4.f \
       reptim.f \
       space.f \
       stasub.f \
       strip.f \
       usedcp.f \
       wapdef.f \
       wbpdef.f \
       wrwrk3.f \
       zero.f \
       zoek.f

#-------------------------------------------------------------------------------
# Libraries
