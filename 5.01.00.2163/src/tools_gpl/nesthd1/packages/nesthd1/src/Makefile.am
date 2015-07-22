include $(top_srcdir)/common.am
bin_PROGRAMS = nesthd1

#-------------------------------------------------------------------------------
#Includes
FINCLUDES += \
        -I$(top_srcdir)/tools_gpl/nesthd1/packages/nesthd1/include \
        -I$(top_srcdir)/utils_lgpl/esmfsm/include 

nesthd1_SOURCES = \
        nesthd1_version.F90 \
        distance.f90 \
        nesthd1.f90 \
        memlib.f90 \
        small.f90 \
        newlun.f90 \
        dattim.f90 \
        detang.f \
        detnst.f \
        detxy.f \
        dimbnd.f \
        dimgrd.f \
        files.f \
        findmn.f \
        getdim.f \
        getlun.f \
        gtpoint.f \
        increm.f \
        inigrd_n.f \
        mkpoint.f \
        nearmn.f \
        pinpol.f \
        reabnd.f \
        reargf.f \
        wrinst.f \
        wrista.f \
        zero.f 

#-------------------------------------------------------------------------------
#Libraries

nesthd1_LDADD = \
        $(top_srcdir)/utils_lgpl/nefis/packages/nefis/src/libnefis.a \
        $(top_srcdir)/utils_lgpl/nefis/packages/nefis_version_number/src/libnefis_version_number.a \
        $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm/src/fsm/libfsm.a \
        $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/libfsm_c.a \
        $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/libesm_c.a \
        $(top_srcdir)/utils_lgpl/esmfsm/packages/esmfsm_version_number/src/libesmfsm_version_number.a \
        $(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common/src/libdeltares_common.a \
        $(top_srcdir)/utils_lgpl/deltares_common/packages/deltares_common_c/src/libdeltares_common_c.a \
        -ldl \
        $(FCLIBS) $(FCLIBS_ADDITIONAL)


BUILT_SOURCES = ./always \
	./nesthd1_version.F90 

./always:
	rm -rf ./nesthd1_version.F90 

./nesthd1_version.F90:
	$(top_srcdir)/scripts_lgpl/linux/update_version.sh $(srcdir)/nesthd1_version.F90 $(srcdir) ../include/version_number.ini $(top_builddir)
