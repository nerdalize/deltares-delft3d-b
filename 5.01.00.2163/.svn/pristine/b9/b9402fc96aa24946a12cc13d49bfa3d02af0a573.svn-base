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
bin_PROGRAMS = allocate_local.exe
 
#-------------------------------------------------------------------------------
#Sources
allocate_local_exe_SOURCES = \
         allocate.cpp

#-------------------------------------------------------------------------------
#Libraries
allocate_local_exe_LDADD = \
         $(top_builddir)/extern/esmfsm/libsrc/esm/libesm.a \
         $(top_builddir)/extern/esmfsm/libsrc/fsm/libfsm.a

