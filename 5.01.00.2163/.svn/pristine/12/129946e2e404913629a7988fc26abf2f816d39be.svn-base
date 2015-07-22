#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Jan. Mooiman@deltares.nl
#   19 june 2009
#
#   Copyright (C) 2009, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libnefis.la

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
libnefis_la_SOURCES = \
	../src/c2c.c \
	../src/df.c \
	../src/er.c \
	../src/f2c.c \
	../src/gp.c \
	../src/gt.c \
	../src/hs.c \
	../src/oc.c \
	../src/pt.c \
	../src/rt.c \
	../src/wl-xdr.c

#-------------------------------------------------------------------------------
#Libraries

libnefis_la_LIBADD = \
	$(top_builddir)/utils_lgpl/nefis/packages/nefis_version_number/src_so/version.lo \
	$(top_builddir)/utils_lgpl/nefis/packages/nefis_version_number/src_so/f2c.lo \
	$(top_builddir)/utils_lgpl/nefis/packages/nefis_version_number/src_so/c2c.lo \
	-lltdl
