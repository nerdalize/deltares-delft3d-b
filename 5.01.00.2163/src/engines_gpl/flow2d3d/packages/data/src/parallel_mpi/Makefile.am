#-------------------------------------------------------------------------------
#   Makefile for delftflow
#
#   Irv.Elshoff@wldelft.nl
#   Adri.Mourits@wldelft.nl
#   Fedor.Baart@wldelft.nl
#   26 aug 10
#
#   Copyright (C) 2007-2010, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libparallel.la

#-------------------------------------------------------------------------------
#Includes
# TODO:/opt/mpich2/include should be configured....
FINCLUDES += \
        -I$(MPI_INCLUDE) \
        -I$(top_builddir)/utils_lgpl/parallel_mpi/include \
        -I$(top_builddir)/utils_lgpl/precision/include \
        -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/include

FMODULES += \
        -I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/parallel_mpi \
	-I$(top_builddir)/engines_gpl/flow2d3d/packages/data/src/basics \
        -I$(top_builddir)/utils_lgpl/ec_module/packages/ec_module/src \
        -I$(top_builddir)/utils_lgpl/inifiles/packages/inifiles/src \
        -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

# fpp is compiler dependent (convention is to use .F90 files for preprocessing)
AM_FCFLAGS += \
        -DDFMPI

#-------------------------------------------------------------------------------
#Sources
libparallel_la_SOURCES = \
         dfparall.F90 \
         initdfparall.F90 \
         dfinitmpi.F90 \
         dfsync.F90 \
         dfexitmpi.F90 \
         dfbroadc.F90 \
         dfsendd_nm_pos1.F90 \
         dfsendd_nm_pos2.F90 \
         dfsendi_nm_pos1.F90 \
         dfsendi_nm_pos2.F90 \
         dfsendr_nm_pos1.F90 \
         dfsendr_nm_pos2.F90 \
         dfupdd_nm_pos1.F90 \
         dfupdd_nm_pos2.F90 \
         dfupdi_nm_pos1.F90 \
         dfupdi_nm_pos2.F90 \
         dfupdr_nm_pos1.F90 \
         dfupdr_nm_pos2.F90 \
         dfwaitd_nm_pos1.F90 \
         dfwaitd_nm_pos2.F90 \
         dfwaiti_nm_pos1.F90 \
         dfwaiti_nm_pos2.F90 \
         dfwaitr_nm_pos1.F90 \
         dfwaitr_nm_pos2.F90 \
         dfsendnb.F90 \
         dfrecvnb.F90 \
         dfreduce.F90 \
         cparri.F90 \
         cparrd.F90 \
         cparrr.F90 \
         dfexchg.F90 \
         dfdecomp.F90 \
         dfpartit.F90 \
         dfbladm.F90 \
         dforb.F90 \
         dfupdgeo.F90 \
         dfstrip.F90 \
         dfmassc.F90 \
         dffunctionals.F90 \
         dfgather.F90 \
         dfgather_grddim.F90 \
         dfwrirst.F90 \
         dfwrmorm1.F90 \
         dfwrmorm2.F90 \
         dfwrsedm.F90 \
         dfwrsedmavg.F90 \
         dfwrsedh.F90 \
         adjlin.F90 \
         rdencl.F90 \
         strgrd.F90

#-------------------------------------------------------------------------------
#Libraries

# MPI
FC=$(MPIFC)
