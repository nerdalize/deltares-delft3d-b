include $(top_srcdir)/common.am
bin_PROGRAMS = stream02test.exe

stream02test_exe_SOURCES = \
	streamtest.cpp

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/stream/include

#-------------------------------------------------------------------------------
#Libraries 

stream02test_exe_LDADD = \
	$(top_builddir)/utils_lgpl/stream/packages/stream/src/libstream.a \
	$(top_builddir)/utils_lgpl/stream/packages/stream_version_number/src/libstream_version_number.a


#	-ldl \
#	$(FCLIBS) -limf
