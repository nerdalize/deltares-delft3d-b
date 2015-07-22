#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   26 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = demo_00

demo_00_SOURCES = \
	demo_00.f
demo_00_LDADD = \
	../../packages/nefis/src/libnefis.a \
	../../packages/nefis_version_number/src/libnefis_version_number.a \
	$(FCLIBS)
