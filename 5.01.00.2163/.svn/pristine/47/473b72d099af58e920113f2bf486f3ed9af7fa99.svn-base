#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_13

#-------------------------------------------------------------------------------
#Includes
INCLUDES = -I../../packages/nefis/include 

test_13_SOURCES = \
	test_13.c
test_13_LDADD = \
	../../packages/nefis/src/libnefis.a \
	../../packages/nefis_version_number/src/libnefis_version_number.a \
	$(CCLIBS)

