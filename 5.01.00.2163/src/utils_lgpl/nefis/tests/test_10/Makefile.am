#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_10

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include 

test_10_SOURCES = \
	test_10.c
test_10_LDADD = \
	../../packages/nefis/src/libnefis.a \
	../../packages/nefis_version_number/src/libnefis_version_number.a \
	$(CCLIBS)
