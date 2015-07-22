#-------------------------------------------------------------------------------
#   Makefile for d_hydro - Flow2D3D Component
#
#   Irv.Elshoff@Deltares.NL
#   31 may 11
#
#   Copyright (C) 2011, Deltares
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

lib_LTLIBRARIES = \
    libDDMapper.la

libDDMapper_la_SOURCES = \
    dredgemerge.cpp \
    gawsbarrier.cpp \
    initbarrier.cpp \
    minimumbarrier.cpp \
    rtccomm.cpp

INCLUDES = \
    $(STANDARD_C_INCLUDES) \
    -I../../../include

libDDMapper_la_LIBADD =

