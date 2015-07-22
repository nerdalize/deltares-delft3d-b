include $(top_srcdir)/common.am
bin_PROGRAMS = esm_info

esm_info_SOURCES = \
	esm_info.c

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/esmfsm/include

#-------------------------------------------------------------------------------
#Libraries 

esm_info_LDADD = \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libfsm.a \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libfsm_c.a \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libesm_c.a \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libesmfsm_version_number.a
