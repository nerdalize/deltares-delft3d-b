#-------------------------------------------------------------------------------
#
#   Jan.Mooiman@deltares.nl
#   26 oct 2007
#
#   Copyright (C) 2007, Deltares
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
bin_PROGRAMS = demo_02

demo_02_SOURCES = \
	demo_02.f
demo_02_LDADD = \
	../../packages/nefis/src/libnefis.a \
	../../packages/nefis_version_number/src/libnefis_version_number.a \
	$(FCLIBS)
