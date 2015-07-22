#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   23 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = test_08

test_08_SOURCES = \
	test_08.f
test_08_LDADD = \
	../../packages/nefis/src/libnefis.a \
	../../packages/nefis_version_number/src/libnefis_version_number.a \
	$(FCLIBS)
