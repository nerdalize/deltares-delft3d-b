#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libwaq_plugin_wasteload.a

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_srcdir)/engines/waq/packages/waq_utils_f/src/waq_utils

#-------------------------------------------------------------------------------
#Sources
libwaq_plugin_wasteload_a_SOURCES = delwaq_user_wasteloads.f \
       srstop.f \
       zoekns.f

#-------------------------------------------------------------------------------
# Libraries
