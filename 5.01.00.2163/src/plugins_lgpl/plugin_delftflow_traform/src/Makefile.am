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

#
# Libtools demands that the name of library to be generated starts with lib
# Remove lib in front of the library name when created
#

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libplugin_delftflow_traform.la

#-------------------------------------------------------------------------------
#Sources
libplugin_delftflow_traform_la_SOURCES = \
         engelund_hansen.f90 \
         partheniades_krone.f90 \
         settling_velocity.f90 \
         shld.f90 \
         vrijn84_riv_77.f90

#-------------------------------------------------------------------------------
#Libraries

