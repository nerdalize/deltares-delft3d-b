dnl @synopsis AC_CHECK_JAVA_HOME
dnl
dnl Check for Sun Java (JDK / JRE) installation, where the 'java' VM is
dnl in. If found, set environment variable JAVA_HOME = Java
dnl installation home, else left JAVA_HOME untouch, which in most case
dnl means JAVA_HOME is empty.
dnl
dnl @category InstalledPackages
dnl @author Gleen Salmon <gleensalmon@yahoo.com>
dnl @version 2002-10-10
dnl @license GPLWithACException

AC_DEFUN([AC_CHECK_JAVA_HOME],[
AC_REQUIRE([AC_EXEEXT])dnl
TRY_JAVA_HOME=`ls -dr /usr/java/* 2> /dev/null | head -n 1`
if test x$TRY_JAVA_HOME != x; then
	PATH=$PATH:$TRY_JAVA_HOME/bin
fi
AC_PATH_PROG(JAVA_PATH_NAME, java$EXEEXT)
if test x$JAVA_PATH_NAME != x; then
	JAVA_HOME=`echo $JAVA_PATH_NAME | sed "s/\(.*\)[[/]]bin[[/]]java$EXEEXT$/\1/"`
fi;dnl
])
