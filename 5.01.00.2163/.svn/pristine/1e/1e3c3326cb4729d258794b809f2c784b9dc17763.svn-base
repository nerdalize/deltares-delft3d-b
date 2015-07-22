#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
lib_LIBRARIES = libdelwaq1_version_number.a

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
        -I../include
#FMODULES += \
#        -I$(top_srcdir)/...

#-------------------------------------------------------------------------------
#Sources
libdelwaq1_version_number_a_SOURCES = c2c.c \
       f2c.c \
       version.c

BUILT_SOURCES = ../include/always \
        ../include/version_number.h

../include/always:
	rm -f ../include/version_number.h

../include/version_number.h:
	$(top_srcdir)/scripts_lgpl/linux/make_revision.sh $(top_srcdir) ../../.. ../include/version_number.ini ../include/version_number.h.svn ../include/version_number.h

#-------------------------------------------------------------------------------
# Libraries
