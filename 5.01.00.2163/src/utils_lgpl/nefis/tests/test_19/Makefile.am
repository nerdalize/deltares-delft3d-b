#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   18 sep 2008
#
#   Copyright (C) 2008, Deltares
#-------------------------------------------------------------------------------
include $(top_srcdir)/common.am
bin_PROGRAMS = test_19

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include 

test_19_SOURCES = \
	test_19.c
test_19_LDADD = \
	../../packages/nefis/src/libnefis.a \
	../../packages/nefis_version_number/src/libnefis_version_number.a \
	$(FCLIBS)
