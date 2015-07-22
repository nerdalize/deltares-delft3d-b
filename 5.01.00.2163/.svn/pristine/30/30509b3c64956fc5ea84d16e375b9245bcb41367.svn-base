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
lib_LIBRARIES = libdeltares_common_c.a

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/include

#-------------------------------------------------------------------------------
#Sources
libdeltares_common_c_a_SOURCES = \
         cutil.c \
         getopt.cpp \
         getputarray.cpp \
         throwexception.cpp

#-------------------------------------------------------------------------------
#Libraries

