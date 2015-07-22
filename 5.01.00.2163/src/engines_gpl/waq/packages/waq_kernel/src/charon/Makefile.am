#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libcharon.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_srcdir)/engines/waq/include \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
libcharon_a_SOURCES = arith.f \
       barx.f \
       berror.f \
       ceccal.f \
       chalsp.f \
       chinp2.f \
       chinpu.f \
       chmapd.f \
       chphas.f \
       chsto2.f \
       chstoc.f \
       cjcor.f \
       cjtemp.f \
       del.f \
       dexp2.f \
       divide.f \
       ehcalc.f \
       exit.f \
       find.f \
       leave.f \
       lookup.f \
       lp.f \
       matinv.f \
       matrix.f \
       partb.f \
       pdfch1.f \
       pdfch2.f \
       pdfcha.f \
       phcalc.f \
       pop.f \
       push.f \
       rcalc.f \
       rows.f \
       simple.f \
       solve2.f \
       solvex.f \
       start.f \
       wricha.f \
       xlog.f

#-------------------------------------------------------------------------------
# Libraries
