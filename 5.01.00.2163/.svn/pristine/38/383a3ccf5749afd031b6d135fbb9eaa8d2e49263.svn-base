#-------------------------------------------------------------------------------
#   Makefile for ec-module
#
#   Irv.Elshoff@deltares.nl
#   Adri.Mourits@deltares.nl
#   Fedor.Baart@deltares.nl
#   Edwin.Spee@deltares.nl
#   9 nov 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libec_module.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
	-I$(top_builddir)/utils_lgpl/precision/packages/precision/src \
	-I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
	-I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libec_module_a_SOURCES = \
         ec_typedefs.f90 \
         ec_message.f90 \
         openexistingfile.f90 \
         ec_parameters.f90 \
         ec_field.f90 \
         ec_quantity.f90 \
         ec_elementset.f90 \
         ec_ecitem.f90 \
         ec_connection.f90 \
         ec_data.f90 \
         ec_provider_read.F90 \
         ec_provider.F90 \
         ec_converter.f90 \
         ec_time.f90 \
         ec_module.f90

#-------------------------------------------------------------------------------
#Libraries

