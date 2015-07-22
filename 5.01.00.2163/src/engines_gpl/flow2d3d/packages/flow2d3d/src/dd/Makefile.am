#-------------------------------------------------------------------------------
#   Makefile for d_hydro - Flow2D3D Component
#
#   Irv.Elshoff@Deltares.NL
#   25 may 12
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

SUBDIRS = \
    iterators \
    mapper

lib_LTLIBRARIES = \
    libDD.la

libDD_la_SOURCES = \
    blob.cpp \
    category.cpp \
    dd.cpp \
    dictionary.cpp \
    distributed.cpp \
    iterator.cpp \
    list.cpp \
    node.cpp \
    semaphore.cpp \
    threads.cpp

INCLUDES = \
    $(STANDARD_C_INCLUDES) \
    -I../../include

libDD_la_LIBADD = \
    iterators/*.lo \
    mapper/*.lo

AM_CPPFLAGS = \
    -Wno-int-to-pointer-cast


