#-------------------------------------------------------------------------------
#   Makefile for d_hydro - Flow2D3D Component
#
#   Irv.Elshoff@Deltares.NL
#   31 may 11
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

lib_LTLIBRARIES = \
    libDDMapper.la

libDDMapper_la_SOURCES = \
    context-flowside.cpp \
    context-gawsside.cpp \
    context-mapside.cpp \
    context_distrib_functs.cpp \
    context_general_functs.cpp \
    flow_nxtstp.cpp \
    gaws.cpp \
    gaws_wang_eq.cpp \
    hymapper.cpp \
    map_debug.cpp \
    map_messages.cpp \
    mapper_config.cpp \
    mapper_general.cpp \
    mapper_statemachine.cpp \
    mapper_uvz.cpp \
    varinfocoll.cpp

INCLUDES = \
    $(STANDARD_C_INCLUDES) \
    -I../../../include
