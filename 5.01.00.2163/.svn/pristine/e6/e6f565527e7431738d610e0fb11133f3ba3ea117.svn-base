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
lib_LIBRARIES = libnefis.a

#-------------------------------------------------------------------------------
#Includes
AM_CFLAGS += \
    -I../include \
    -fPIC \
    -D_FILE_OFFSET_BITS=64 \
    -DPTR8
AM_CXXFLAGS += \
    -I../include \
    -fPIC \
    -D_FILE_OFFSET_BITS=64 \
    -DPTR8

#-------------------------------------------------------------------------------
#Sources
libnefis_a_SOURCES = \
         c2c.c \
         df.c \
         er.c \
         f2c.c \
         gp.c \
         gt.c \
         hs.c \
         oc.c \
         pt.c \
         rt.c \
         wl-xdr.c

#-------------------------------------------------------------------------------
#Libraries

#libnefis_a_LIBADD = \
#	$(top_builddir)/utils_lgpl/nefis/packages/nefis_version_number/src/libnefis_version_number.a
