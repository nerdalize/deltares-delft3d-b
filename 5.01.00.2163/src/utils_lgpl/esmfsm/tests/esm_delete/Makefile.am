include $(top_srcdir)/common.am
bin_PROGRAMS = esm_delete

esm_delete_SOURCES = \
	esm_delete.c

#-------------------------------------------------------------------------------
#Includes
INCLUDES = \
	-I$(top_builddir)/utils_lgpl/esmfsm/include

#-------------------------------------------------------------------------------
#Libraries 

esm_delete_LDADD = \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libfsm.a \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libfsm_c.a \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libesm_c.a \
	$(top_builddir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libesmfsm_version_number.a
