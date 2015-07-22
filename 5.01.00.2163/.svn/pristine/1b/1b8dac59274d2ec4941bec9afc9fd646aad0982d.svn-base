#						-*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
AC_PREREQ(2.59)
AC_INIT(Delft3D, 0.0, delft3d.support@deltares.nl)
dnl Store generated files in config dir
AC_CONFIG_AUX_DIR(config)
dnl Define a unique file to check if we're in the right directory
AC_CONFIG_SRCDIR([engines_gpl/flow2d3d/packages/kernel/src/compute/adi.f90])
AC_CONFIG_HEADER([config.h])
dnl m4 macro's in m4 directory.
AC_CONFIG_MACRO_DIR([m4])

dnl Look up target and host info (java needs this, should be called before init automake)
AC_CANONICAL_SYSTEM

dnl Initialise automake
AM_INIT_AUTOMAKE

dnl Initialize LIBTOOL 
dnl Make sure we have an up to date version.
dnl We're going to make it work with older versions also, this might not work (libtool < 2.2, not sure how to check)

## ----------------------- ##
## Libtool initialisation. ##
## ----------------------- ##

#LT_PREREQ(2.4)
LT_INIT
dnl LTDL_INIT dnl I don't think this is required anymore
dnl AC_PROG_LIBTOOL
AC_SUBST([LIBTOOL_DEPS])
AC_CONFIG_SUBDIRS(libltdl)
AC_SUBST(LIBLTDL)
AC_SUBST(INCLTDL)

# c, c++ and c preprocessor
# We should prefer gcc-mp-#.# for OSX (prefer macports over default compiler), Use CXX=g++-mp-#.# CC=gcc-mp-#.# to use the macports compiler..
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP

#These are required for java
#AC_PROG_JAVA
#AC_PROG_JAVA_CC
#AC_PROG_JAVAC
#this is needed when sun java is unavailable
#AM_PROG_GCJ

# Other programs
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
# These standard macros don't work well (don't throw an error on missing yacc)
# According to docs we can better use AM_PROG_LEX and AC_PROG_YACC
AC_PROG_YACC 
AM_PROG_LEX
# So we'll double check
AC_CHECK_PROGS(LEXPROG, flex lex, :)
if test "x$LEXPROG" = "x:"; then
   AC_MSG_ERROR([Can't find lex, please install or set the LEX variable])
fi
AC_CHECK_PROGS(YACCPROG, 'bison -y' byacc yacc, :)
if test "x$YACCPROG" = "x:"; then
   AC_MSG_ERROR([Can't find yacc, please install or set the YACC variable])
fi

AC_LANG_PUSH([C++])
dnl Check for MPI (sets MPIFC, MPILIBS, HAVE_MPI)
ACX_MPI
AC_CHECK_FUNCS([strdup vsnprintf])
AC_MSG_CHECKING([whether to enable dolcli])
AC_ARG_ENABLE(dolcli,
AS_HELP_STRING([--enable-dolcli],
[Enable the compilation of test executable delftonline/dolcli @<:@default no@:>@]),[enableval='yes'],[enableval='no'])
dnl set compiler specific options here: 
if test "$enableval" = yes ; then
        AC_MSG_RESULT([yes])
        AC_CHECK_HEADERS([readline/history.h readline/readline.h],[],[AC_MSG_ERROR([Can't find readline header files. Please install libreadline6-dev (Ubuntu) or readline-devel (RedHat)])])
	dnl define a variable in the header file
	AC_DEFINE([ENABLE_DOLCLI], 1, [Use command line interface precision])
	dnl define the lowercase variable dolcli (see the substitute)
	dolcli=true
else
        AC_MSG_RESULT([no])
	dolcli=false
fi
dnl this variable ends up in the makefile.
AC_SUBST(dolcli)
AM_CONDITIONAL([ENABLE_DOLCLI], [test x$enableval = xyes])

AC_LANG_POP

## This is copied from the libtool examples
## -------------------------- ##
## Fortran90 compiler checks. ##
## -------------------------- ##
AC_LANG_PUSH([Fortran])
dnl http://www.astro.gla.ac.uk/users/norman/star/ssn78/ssn78.htx/N-x1a1b12.html
AC_PROG_FC([ifort gfortran-mp-4.6 gfortran-mp-4.5 gfortran-mp-4.4 gfortran g95])
dnl Check the flags needed to link fc programs with ld (i.e. cc)
AC_FC_LIBRARY_LDFLAGS
dnl Check for openmp for fortran
AX_OPENMP
dnl prepend the found flags
FCFLAGS="$FCFLAGS $OPENMP_FCFLAGS"

dnl Check for MPI (sets MPIFC, MPILIBS, HAVE_MPI)
ACX_MPI

dnl Check for underscoring of external names
AC_FC_WRAPPERS
dnl We need to use .f90 and not .f to enable Automake FC support
dnl Some Intel ifc/ifort do not understand .f95.  :-/
AC_FC_SRCEXT([f90])
dnl Yes, we want free form Fortran!
AC_FC_FREEFORM
dnl Set unlimited line length (don't create lines longer than 132, because it breaks on some compilers,
dnl but because ifort and gfortran allow it let's not raise errors by default)
AC_FC_LINE_LENGTH([unlimited])
dnl check to see if we can write binary files
ACX_FC_FORM_BINARY
dnl add preprocess option
AC_FC_PREPROCESS
AC_LANG_POP

## -------------------------- ##
## Fortran77 compiler checks. ##
## -------------------------- ##
AC_LANG_PUSH([Fortran 77])
AC_PROG_F77([ifort gfortran-mp-4.6 gfortran-mp-4.5 gfortran-mp-4.4 gfortran g77])
AC_F77_LIBRARY_LDFLAGS
AX_OPENMP
FFLAGS="$FFLAGS $OPENMP_FFLAGS"
AC_LANG_POP

dnl Fortran compiler defaults (this could be a separate m4 file)
AC_MSG_CHECKING([whether using Fortran preset flags])
AC_ARG_ENABLE(fortran-flags-preset,
AS_HELP_STRING([--enable-fortran-flags-preset],
[Enable Fortran compiler flags preset @<:@default yes@:>@]),[],[enableval='yes'])
dnl set compiler specific options here: 
if test "$enableval" = yes ; then
        AC_MSG_RESULT([yes])
	case "$F77" in
                *gfortran*) dnl GNU Fortran 95 compiler
		dnl no custom flags
                ;;
                *ifort*) dnl Intel 8.x Fortran compiler
                        FFLAGS="$FFLAGS -recursive -reentrancy threaded -traceback"
                ;;
        esac
	case "$FC" in
	     *gfortran*) dnl GNU Fortran 95 compiler
	     dnl no custom flags
	     ;;
	     *ifort*) dnl Intel 8.x Fortran compiler
		      FCFLAGS="$FCFLAGS -recursive -reentrancy threaded -traceback"
             ;;
	esac
else
        AC_MSG_RESULT([no])
fi

			

   


# As of the writing of this demo, GNU Autoconf's AC_OBJEXT and
# AC_EXEEXT macros only works for C compilers!
# Libtool's setup macro calls AC_OBJEXT and AC_EXEEXT without setting
# the test language to C.  We do it before any libtool setup macros are
# called so that the proper values are cached beforehand.  We also do
# it before any linker flags (LDFLAGS) are set so that fortran specific
# ones don't break the tests.
AC_LANG_PUSH([C])
AC_OBJEXT
AC_LANG_POP


# executables will get an .exe extension 
AC_EXEEXT([.exe])




dnl define options
dnl Use double support
AC_MSG_CHECKING([whether to disable double support])
AC_ARG_ENABLE(double,
[  --disable-double           Disable double support],
   [ AC_MSG_RESULT([yes]) ],
   [ AC_MSG_RESULT([no]); AC_DEFINE(WITH_DOUBLE, 1, [Use double precision]) ]
)


# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([libgen.h arpa/inet.h fcntl.h netdb.h netinet/in.h stdlib.h string.h sys/socket.h sys/time.h errno.h unistd.h malloc.h malloc/malloc.h sys/ipc.h sys/shm.h])

#check for jni.h (for later)
#AC_JNI_INCLUDE_DIR

#check for c++ standard library
AC_CHECK_LIB([stdc++], [main],, [AC_MSG_ERROR([Compiler can't find libstdc++])])
AC_CHECK_TYPES([struct shm_info])
#Check for expat
AC_CHECK_HEADER(expat.h,, [AC_MSG_ERROR([Compiler can't find <expat.h>])])
AC_CHECK_LIB(expat, XML_Parse,, [AC_MSG_ERROR([Compiler can't find libexpat])])
AC_FUNC_VPRINTF
# When using the Intel Fortran compiler, check for the needed libraries
####if test $FC == ifort; then
####    AC_CHECK_LIB([irc], [main], [HAVE_LIBirc=1] [FCLIBS="$FCLIBS -lirc"],)
####    AC_CHECK_LIB([svml], [main], [HAVE_LIBsvml=1] [FCLIBS="$FCLIBS -lsvml"],)
####    AC_CHECK_LIB([mcheck], [main], [HAVE_LIBmcheck=1] [FCLIBS="$FCLIBS -lmcheck"],)
####fi

# checks for pthreads
ACX_PTHREAD([enable_threads="pthread"],[enable_threads="no"])

if test $enable_threads != "pthread"; then
    AC_MSG_ERROR([unable to find pthreads, currently this is required])
else
    AC_DEFINE(HAVE_PTHREAD,1,[Define if you have POSIX threads libraries and header files.])
    LIBS="$PTHREAD_LIBS $LIBS"
    FCLIBS="$PTHREAD_LIBS $FCLIBS"
    CFLAGS="$PTHREAD_CFLAGS $CFLAGS"
    CXXFLAGS="$PTHREAD_CFLAGS $CXXFLAGS"
fi


# Make sure we throw errors when using invalid architecture files (otherwise OSX version might get linked against linux libs)


# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
dnl AC_FUNC_MALLOC must probably be removed on OSX
AC_FUNC_MALLOC
AC_HEADER_MAJOR
dnl AC_FUNC_REALLOC must probably be removed on OSX
AC_FUNC_REALLOC
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([gethostname gettimeofday memset strerror])

# Checks for QT library functions.
# gw_CHECK_QT


dnl Already create the libtool script here, because we're using it to detect .dylib/.so extension (works for >=2.2)
LT_OUTPUT 

dnl Lookup the library extension (usually: so,sl,dylib,dll)
AC_MSG_CHECKING(for the extension of Delft3D plugins)
shrext_cmds=$(./libtool --config | grep '^shrext_cmds=')
eval $shrext_cmds
module=no eval pluginext="$shrext_cmds"
AC_MSG_RESULT($pluginext)
if test -z "$pluginext"; then
  AC_MSG_ERROR([can't detect plugin extension])
fi
AC_DEFINE_UNQUOTED(D3D_PLUGIN_EXT, ["$pluginext"], [Delft3D plugin extension])

#-------------------------------------------------------------------------------
#   List of makefiles to generate.  This list can be created with the command:
#   find . -name Makefile.am | egrep '/.+/' | sort | sed 's/.am$//'

AC_CONFIG_FILES([Makefile
engines_gpl/Makefile
engines_gpl/d_hydro/Makefile
engines_gpl/d_hydro/packages/Makefile
engines_gpl/d_hydro/packages/d_hydro/Makefile
engines_gpl/d_hydro/packages/d_hydro/src/Makefile
engines_gpl/flow2d3d/Makefile
engines_gpl/flow2d3d/packages/Makefile
engines_gpl/flow2d3d/packages/data/Makefile
engines_gpl/flow2d3d/packages/data/src/Makefile
engines_gpl/flow2d3d/packages/data/src/allocation/Makefile
engines_gpl/flow2d3d/packages/data/src/basics/Makefile
engines_gpl/flow2d3d/packages/data/src/gdp/Makefile
engines_gpl/flow2d3d/packages/data/src/general/Makefile
engines_gpl/flow2d3d/packages/data/src/parallel_mpi/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/src/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/src/dd/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/src/dd/iterators/Makefile
engines_gpl/flow2d3d/packages/flow2d3d/src/dd/mapper/Makefile
engines_gpl/flow2d3d/packages/flow2d3d_openda/Makefile
engines_gpl/flow2d3d/packages/flow2d3d_openda/src/Makefile
engines_gpl/flow2d3d/packages/io/Makefile
engines_gpl/flow2d3d/packages/io/src/Makefile
engines_gpl/flow2d3d/packages/io/src/input/Makefile
engines_gpl/flow2d3d/packages/io/src/output/Makefile
engines_gpl/flow2d3d/packages/io/src/preprocessor/Makefile
engines_gpl/flow2d3d/packages/io_dol_f/Makefile
engines_gpl/flow2d3d/packages/io_dol_f/src/Makefile
engines_gpl/flow2d3d/packages/kernel/Makefile
engines_gpl/flow2d3d/packages/kernel/src/Makefile
engines_gpl/flow2d3d/packages/kernel/src/compute/Makefile
engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/Makefile
engines_gpl/flow2d3d/packages/kernel/src/compute_roller/Makefile
engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/Makefile
engines_gpl/flow2d3d/packages/kernel/src/dd/Makefile
engines_gpl/flow2d3d/packages/kernel/src/general/Makefile
engines_gpl/flow2d3d/packages/kernel/src/inichk/Makefile
engines_gpl/flow2d3d/packages/kernel/src/main/Makefile
engines_gpl/flow2d3d/packages/kernel/src/non_hydro/Makefile
engines_gpl/flow2d3d/packages/kernel/src/taylor/Makefile
engines_gpl/flow2d3d/packages/kernel/src/timedep/Makefile
engines_gpl/flow2d3d/packages/kernel_dd_f/Makefile
engines_gpl/flow2d3d/packages/kernel_dd_f/src/Makefile
engines_gpl/flow2d3d/packages/manager/Makefile
engines_gpl/flow2d3d/packages/manager/src/Makefile
engines_gpl/flow2d3d/packages/plugin_3dmortrafrm_c/Makefile
engines_gpl/flow2d3d/packages/plugin_3dmortrafrm_c/src/Makefile
engines_gpl/flow2d3d/packages/plugin_user/Makefile
engines_gpl/flow2d3d/packages/plugin_user/src/Makefile
engines_gpl/wave/Makefile
engines_gpl/wave/packages/Makefile
engines_gpl/wave/packages/data/Makefile
engines_gpl/wave/packages/data/src/Makefile
engines_gpl/wave/packages/io/Makefile
engines_gpl/wave/packages/io/src/Makefile
engines_gpl/wave/packages/kernel/Makefile
engines_gpl/wave/packages/kernel/src/Makefile
engines_gpl/wave/packages/manager/Makefile
engines_gpl/wave/packages/manager/src/Makefile
engines_gpl/wave/packages/wave/Makefile
engines_gpl/wave/packages/wave/src/Makefile
plugins_lgpl/Makefile
plugins_lgpl/plugin_culvert/Makefile
plugins_lgpl/plugin_culvert/src/Makefile
plugins_lgpl/plugin_delftflow_traform/Makefile
plugins_lgpl/plugin_delftflow_traform/src/Makefile
scripts_lgpl/Makefile
scripts_lgpl/deltares_hydro/Makefile
scripts_lgpl/linux/Makefile
third_party_open/Makefile
third_party_open/triangle/Makefile
third_party_open/version_number/Makefile
third_party_open/version_number/packages/Makefile
third_party_open/version_number/packages/version_number/Makefile
third_party_open/version_number/packages/version_number/src/Makefile
tools_gpl/Makefile
tools_gpl/datsel/Makefile
tools_gpl/datsel/packages/Makefile
tools_gpl/datsel/packages/datsel/Makefile
tools_gpl/datsel/packages/datsel/src/Makefile
tools_gpl/datsel/packages/datsel_f/Makefile
tools_gpl/datsel/packages/datsel_f/src/Makefile
tools_gpl/datsel/packages/datsel_version_number/Makefile
tools_gpl/datsel/packages/datsel_version_number/src/Makefile
tools_gpl/kubint/Makefile
tools_gpl/kubint/packages/Makefile
tools_gpl/kubint/packages/kubint/Makefile
tools_gpl/kubint/packages/kubint/src/Makefile
tools_gpl/kubint/packages/kubint_f/Makefile
tools_gpl/kubint/packages/kubint_f/src/Makefile
tools_gpl/kubint/packages/kubint_version_number/Makefile
tools_gpl/kubint/packages/kubint_version_number/src/Makefile
tools_gpl/lint/Makefile
tools_gpl/lint/packages/Makefile
tools_gpl/lint/packages/lint/Makefile
tools_gpl/lint/packages/lint/src/Makefile
tools_gpl/lint/packages/lint_f/Makefile
tools_gpl/lint/packages/lint_f/src/Makefile
tools_gpl/lint/packages/lint_version_number/Makefile
tools_gpl/lint/packages/lint_version_number/src/Makefile
tools_gpl/mormerge/Makefile
tools_gpl/mormerge/packages/Makefile
tools_gpl/mormerge/packages/mormerge/Makefile
tools_gpl/mormerge/packages/mormerge/src/Makefile
tools_gpl/mormerge/packages/mormerge_version_number/Makefile
tools_gpl/mormerge/packages/mormerge_version_number/src/Makefile
tools_gpl/nesthd1/Makefile
tools_gpl/nesthd1/packages/Makefile
tools_gpl/nesthd1/packages/nesthd1/Makefile
tools_gpl/nesthd1/packages/nesthd1/src/Makefile
tools_gpl/nesthd2/Makefile
tools_gpl/nesthd2/packages/Makefile
tools_gpl/nesthd2/packages/nesthd2/Makefile
tools_gpl/nesthd2/packages/nesthd2/src/Makefile
tools_gpl/vs/Makefile
tools_gpl/vs/packages/Makefile
tools_gpl/vs/packages/vs/Makefile
tools_gpl/vs/packages/vs/src/Makefile
tools_gpl/vs/packages/vs_version_number/Makefile
tools_gpl/vs/packages/vs_version_number/src/Makefile
utils_lgpl/Makefile
utils_lgpl/constants/Makefile
utils_lgpl/constants/packages/Makefile
utils_lgpl/constants/packages/mathconsts/Makefile
utils_lgpl/constants/packages/mathconsts/src/Makefile
utils_lgpl/d_hydro_lib/Makefile
utils_lgpl/d_hydro_lib/packages/Makefile
utils_lgpl/d_hydro_lib/packages/d_hydro_lib/Makefile
utils_lgpl/d_hydro_lib/packages/d_hydro_lib/src/Makefile
utils_lgpl/delftio/Makefile
utils_lgpl/delftio/packages/Makefile
utils_lgpl/delftio/packages/delftio/Makefile
utils_lgpl/delftio/packages/delftio/src/Makefile
utils_lgpl/delftio/packages/delftio/src/diof90/Makefile
utils_lgpl/delftio/packages/delftio/src/diof90Nefis/Makefile
utils_lgpl/delftio/packages/delftio_shm/Makefile
utils_lgpl/delftio/packages/delftio_shm/src/Makefile
utils_lgpl/delftio/packages/delftio_shm/src/diof90/Makefile
utils_lgpl/delftio/packages/delftio_version_number/Makefile
utils_lgpl/delftio/packages/delftio_version_number/src/Makefile
utils_lgpl/delftio_sync/Makefile
utils_lgpl/delftio_sync/packages/Makefile
utils_lgpl/delftio_sync/packages/delftio_sync/Makefile
utils_lgpl/delftio_sync/packages/delftio_sync/src/Makefile
utils_lgpl/delftio_sync/packages/delftio_sync_version_number/Makefile
utils_lgpl/delftio_sync/packages/delftio_sync_version_number/src/Makefile
utils_lgpl/delftonline/Makefile
utils_lgpl/delftonline/src/delftonline/Makefile
utils_lgpl/delftonline/src/dolcli/Makefile
utils_lgpl/delftonline/src/dolprint/Makefile
utils_lgpl/deltares_common/Makefile
utils_lgpl/deltares_common/packages/Makefile
utils_lgpl/deltares_common/packages/deltares_common/Makefile
utils_lgpl/deltares_common/packages/deltares_common/src/Makefile
utils_lgpl/deltares_common/packages/deltares_common_c/Makefile
utils_lgpl/deltares_common/packages/deltares_common_c/include/Makefile
utils_lgpl/deltares_common/packages/deltares_common_c/src/Makefile
utils_lgpl/ec_module/Makefile
utils_lgpl/ec_module/packages/Makefile
utils_lgpl/ec_module/packages/ec_module/Makefile
utils_lgpl/ec_module/packages/ec_module/src/Makefile
utils_lgpl/ec_module/packages/ec_module/src/ectest/Makefile
utils_lgpl/ec_module/packages/ec_module_version_number/Makefile
utils_lgpl/ec_module/packages/ec_module_version_number/src/Makefile
utils_lgpl/esmfsm/Makefile
utils_lgpl/esmfsm/packages/Makefile
utils_lgpl/esmfsm/packages/esmfsm/Makefile
utils_lgpl/esmfsm/packages/esmfsm/src/Makefile
utils_lgpl/esmfsm/packages/esmfsm/src/fsm/Makefile
utils_lgpl/esmfsm/packages/esmfsm_c/Makefile
utils_lgpl/esmfsm/packages/esmfsm_c/src/Makefile
utils_lgpl/esmfsm/packages/esmfsm_c/src/esm/Makefile
utils_lgpl/esmfsm/packages/esmfsm_c/src/fsm/Makefile
utils_lgpl/esmfsm/packages/esmfsm_version_number/Makefile
utils_lgpl/esmfsm/packages/esmfsm_version_number/src/Makefile
utils_lgpl/esmfsm/tests/Makefile
utils_lgpl/esmfsm/tests/allocate_local/Makefile
utils_lgpl/esmfsm/tests/esm_create/Makefile
utils_lgpl/esmfsm/tests/esm_delete/Makefile
utils_lgpl/esmfsm/tests/esm_info/Makefile
utils_lgpl/esmfsm/tests/test_01/Makefile
utils_lgpl/esmfsm/tests/test_02/Makefile
utils_lgpl/esmfsm/tests/test_03/Makefile
utils_lgpl/esmfsm/tests/testesm/Makefile
utils_lgpl/esmfsm/tests/testfill/Makefile
utils_lgpl/esmfsm/tests/testfsm/Makefile
utils_lgpl/ftnunit/Makefile
utils_lgpl/ftnunit/packages/Makefile
utils_lgpl/ftnunit/packages/ftnunit/Makefile
utils_lgpl/ftnunit/packages/ftnunit/src/Makefile
utils_lgpl/inifiles/Makefile
utils_lgpl/inifiles/packages/Makefile
utils_lgpl/inifiles/packages/inifiles/Makefile
utils_lgpl/inifiles/packages/inifiles/src/Makefile
utils_lgpl/inifiles/tests/01_initest/Makefile
utils_lgpl/inifiles/tests/02_treetest/Makefile
utils_lgpl/inifiles/tests/03_dredgedumptest/Makefile
utils_lgpl/meteo/Makefile
utils_lgpl/meteo/packages/Makefile
utils_lgpl/meteo/packages/meteo/Makefile
utils_lgpl/meteo/packages/meteo/src/Makefile
utils_lgpl/meteo/packages/meteo_version_number/Makefile
utils_lgpl/meteo/packages/meteo_version_number/src/Makefile
utils_lgpl/nefis/Makefile
utils_lgpl/nefis/packages/Makefile
utils_lgpl/nefis/packages/nefis/Makefile
utils_lgpl/nefis/packages/nefis/src/Makefile
utils_lgpl/nefis/packages/nefis/src_so/Makefile
utils_lgpl/nefis/packages/nefis_version_number/Makefile
utils_lgpl/nefis/packages/nefis_version_number/src/Makefile
utils_lgpl/nefis/packages/nefis_version_number/src_so/Makefile
utils_lgpl/nefis/tests/Makefile
utils_lgpl/nefis/tests/demo_00/Makefile
utils_lgpl/nefis/tests/demo_01/Makefile
utils_lgpl/nefis/tests/demo_02/Makefile
utils_lgpl/nefis/tests/observations/Makefile
utils_lgpl/nefis/tests/test_00/Makefile
utils_lgpl/nefis/tests/test_01/Makefile
utils_lgpl/nefis/tests/test_02/Makefile
utils_lgpl/nefis/tests/test_03/Makefile
utils_lgpl/nefis/tests/test_04/Makefile
utils_lgpl/nefis/tests/test_05/Makefile
utils_lgpl/nefis/tests/test_06/Makefile
utils_lgpl/nefis/tests/test_07/Makefile
utils_lgpl/nefis/tests/test_08/Makefile
utils_lgpl/nefis/tests/test_09/Makefile
utils_lgpl/nefis/tests/test_10/Makefile
utils_lgpl/nefis/tests/test_11/Makefile
utils_lgpl/nefis/tests/test_12/Makefile
utils_lgpl/nefis/tests/test_13/Makefile
utils_lgpl/nefis/tests/test_14/Makefile
utils_lgpl/nefis/tests/test_15/Makefile
utils_lgpl/nefis/tests/test_16/Makefile
utils_lgpl/nefis/tests/test_17/Makefile
utils_lgpl/nefis/tests/test_18/Makefile
utils_lgpl/nefis/tests/test_19/Makefile
utils_lgpl/nefis/tests/test_20/Makefile
utils_lgpl/ods/packages/ods/src/Makefile
utils_lgpl/precision/Makefile
utils_lgpl/precision/packages/Makefile
utils_lgpl/precision/packages/precision/Makefile
utils_lgpl/precision/packages/precision/src/Makefile
utils_lgpl/semaphore/Makefile
utils_lgpl/semaphore/packages/Makefile
utils_lgpl/semaphore/packages/semaphore/Makefile
utils_lgpl/semaphore/packages/semaphore/src/Makefile
utils_lgpl/stream/Makefile
utils_lgpl/stream/packages/Makefile
utils_lgpl/stream/packages/stream/Makefile
utils_lgpl/stream/packages/stream/src/Makefile
utils_lgpl/stream/packages/stream_version_number/Makefile
utils_lgpl/stream/packages/stream_version_number/src/Makefile
utils_lgpl/stream/tests/01-smalltest/Makefile
utils_lgpl/stream/tests/02-streamtest/Makefile
utils_lgpl/stream/tests/Makefile
		 ])

AC_OUTPUT
