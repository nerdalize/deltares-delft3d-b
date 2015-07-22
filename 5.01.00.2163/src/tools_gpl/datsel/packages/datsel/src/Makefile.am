include $(top_srcdir)/common.am
bin_PROGRAMS = datsel

datsel_SOURCES = \
	main.cpp

#-------------------------------------------------------------------------------
#Libraries 

datsel_LDADD = \
	$(top_builddir)/tools_gpl/datsel/packages/datsel_f/src/libdatsel_f.a \
	$(top_builddir)/tools_gpl/datsel/packages/datsel_version_number/src/libdatsel_version_number.a \
	$(top_builddir)/utils_lgpl/nefis/packages/nefis/src/libnefis.a \
	$(top_builddir)/utils_lgpl/nefis/packages/nefis_version_number/src/libnefis_version_number.a \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src/libdeltares_common.a \
	$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libdeltares_common_c.a \
	-ldl \
	$(FCLIBS) $(FCLIBS_ADDITIONAL)
