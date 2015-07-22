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
lib_LIBRARIES = libsemaphore.a

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../include

#-------------------------------------------------------------------------------
#Sources
libsemaphore_a_SOURCES = \
         sems.cpp \
         semaphore_version.cpp

BUILT_SOURCES = ./always \
	../include/semaphore_version.h

./always:
	rm -rf ../include/semaphore_version.h

../include/semaphore_version.h:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh ../include/semaphore_version.h $(srcdir) ../include/version_number.ini $(top_builddir)

#-------------------------------------------------------------------------------
#Libraries

