#-------------------------------------------------------------------------------
#   Makefile for version_number
#
#   jan.mooiman@deltares.nl
#   22 jul 2008
#
#   Copyright (C) 2008, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LTLIBRARIES = libnefis_version_number.la

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../include \
	-fpic

#-------------------------------------------------------------------------------
#Sources
libnefis_version_number_la_SOURCES = \
	../src/c2c.c \
	../src/f2c.c \
	../src/version.c

BUILT_SOURCES =  ../include/always \
	../include/version_number.h

../include/always:
	rm -f ../include/version_number.h

../include/version_number.h:
	$(top_srcdir)/scripts_lgpl/linux/make_revision.sh $(top_builddir) ../../.. ../include/version_number.ini ../include/version_number.h.svn ../include/version_number.h
#-------------------------------------------------------------------------------
#Libraries

