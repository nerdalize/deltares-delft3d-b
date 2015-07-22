#-------------------------------------------------------------------------------
#   Makefile for DelftOnline
#
#   Irv.Elshoff@Deltares.NL
#   26 may 12
#-------------------------------------------------------------------------------


include $(top_srcdir)/common.am

lib_LTLIBRARIES = \
    libDelftOnline.la

INCLUDES = \
    $(STANDARD_C_INCLUDES) \
    -I../../include

libDelftOnline_la_SOURCES = \
    arrayshape.cpp \
    barrier.cpp \
    client.cpp \
    clock.cpp \
    connection.cpp \
    dataelement.cpp \
    delftonline_version.cpp \
    dictionary.cpp \
    directory.cpp \
    exception.cpp \
    function.cpp \
    linkedlist.cpp \
    log.cpp \
    print.cpp \
    server.cpp \
    sortedbag.cpp \
    util.cpp


AM_CPPFLAGS = \
    -Wno-int-to-pointer-cast

BUILT_SOURCES = ./always \
	delftonline_version.h

./always:
	rm -rf delftonline_version.h

delftonline_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh delftonline_version.h $(srcdir) version_number.ini $(top_builddir)

