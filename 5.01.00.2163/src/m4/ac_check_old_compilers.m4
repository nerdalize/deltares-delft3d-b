dnl @synopsis AC_CHECK_OLD_COMPILER
dnl
dnl Here is a summary of the main macros:
dnl
dnl AC_CHECK_OLD_COMPILER: checks if you use a compiler which isn't allowed
dnl

AC_DEFUN([AC_CHECK_OLD_COMPILER],[
 AC_CHECK_PROG(IFORT, ifort, ifort)
 if test x$IFORT != x; then
   IFORT_VERSION=`$IFORT -v 2>&1 | sed 's/.*\(8\).*/\1/'`
   if test x$IFORT_VERSION = x8; then 
     AC_MSG_ERROR([Old intel fortran compiler (8.x)found. Please upgrade to a newer compiler])
   fi
 fi
])
