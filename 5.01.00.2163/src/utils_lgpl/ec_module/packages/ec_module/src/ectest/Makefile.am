#-------------------------------------------------------------------------------
#   Makefile for ectest
#
#   edwin.spee@deltares.nl
#   6 nov 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am

bin_PROGRAMS= ectest 

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src \
	-I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src

#-------------------------------------------------------------------------------

ectest_SOURCES= \
         ectest_gdp.f90 \
         ectest_readmeteo.f90 \
         ectest_rr_cf.f90 \
         ectest.f90


ectest_LDADD =\
	../libec_module.a \
        $(top_builddir)/utils_lgpl/precision/packages/precision/src/libprecision.a \
        $(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libdeltares_common.a

