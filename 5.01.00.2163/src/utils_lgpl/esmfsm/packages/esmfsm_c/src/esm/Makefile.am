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
lib_LIBRARIES = libesm_c.a

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I$(top_builddir)/utils_lgpl/esmfsm/include \
           -I$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/include

#-------------------------------------------------------------------------------
#Sources
libesm_c_a_SOURCES = \
         esm.cpp \
         esm_f.cpp \
         local.cpp \
         shared.cpp

#-------------------------------------------------------------------------------
#Libraries

