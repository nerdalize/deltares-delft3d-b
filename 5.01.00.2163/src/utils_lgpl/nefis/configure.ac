#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([nefis5], [0.00.00], delft3d.support@deltares.nl)
dnl Store generated files in config dir
AC_CONFIG_AUX_DIR(config)
dnl Define a unique file to check if we're in the right directory
AC_CONFIG_SRCDIR([packages/nefis/src/oc.c])
AC_CONFIG_HEADER([config.h])
AC_CONFIG_MACRO_DIR([m4])

dnl Initialiseer automake
AM_INIT_AUTOMAKE

## This is copied from the libtool examples
## ------------------------ ##
## Fortran compiler checks. ##
## ------------------------ ##
AC_LANG_PUSH([Fortran])
dnl http://www.astro.gla.ac.uk/users/norman/star/ssn78/ssn78.htx/N-x1a1b12.html
AC_PROG_F77([gfortran g95])
AC_PROG_FC([gfortran g95])
dnl Check the flags needed to link fc programs with ld (i.e. cc)
AC_FC_LIBRARY_LDFLAGS
dnl Check for underscoring of external names
AC_FC_WRAPPERS
dnl We need to use .f90 and not .f to enable Automake FC support
dnl Some Intel ifc/ifort do not understand .f95.  :-/
AC_FC_SRCEXT([f90])
dnl Yes, we want free form Fortran!
AC_FC_FREEFORM
AC_LANG_POP 
AC_PROG_LIBTOOL

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC
AC_PROG_CPP

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_C_CONST
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_VPRINTF
AC_CHECK_FUNCS([strchr strdup strrchr strstr])

#which makefiles are we gonna make?
AC_CONFIG_FILES([Makefile
                 packages/Makefile
                 packages/nefis/Makefile
                 packages/nefis/src/Makefile
                 packages/nefis_version_number/Makefile
                 packages/nefis_version_number/src/Makefile
                 tests/Makefile
                 tests/test_00/Makefile
                 tests/test_01/Makefile
                 tests/test_02/Makefile
                 tests/test_03/Makefile
                 tests/test_04/Makefile
                 tests/test_05/Makefile
                 tests/test_06/Makefile
                 tests/test_07/Makefile
                 tests/test_08/Makefile
                 tests/test_09/Makefile
                 tests/test_10/Makefile
                 tests/test_11/Makefile
                 tests/test_12/Makefile
                 tests/test_13/Makefile
                 tests/test_14/Makefile
                 tests/test_15/Makefile
                 tests/test_16/Makefile
                 tests/test_17/Makefile
                 tests/test_18/Makefile
                 tests/test_19/Makefile
                 tests/demo_00/Makefile
                 tests/demo_01/Makefile
                 tests/demo_02/Makefile
                 ])
AC_OUTPUT
