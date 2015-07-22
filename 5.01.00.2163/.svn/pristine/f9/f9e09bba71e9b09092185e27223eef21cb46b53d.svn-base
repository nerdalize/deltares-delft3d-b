#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   26 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = demo_01

demo_01_SOURCES = \
	demo_01.f
demo_01_LDADD = \
	../../packages/nefis/src/libnefis.a \
	../../packages/nefis_version_number/src/libnefis_version_number.a \
	$(FCLIBS)
	