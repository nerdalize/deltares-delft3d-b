dnl check to see if we can write to binary output
AC_DEFUN([ACX_FC_FORM_BINARY],
 [AC_CACHE_CHECK([whether $FC supports writing in form "binary" features],
 [ax_cv_fc_supports_form_binary],
 [AC_REQUIRE([AC_PROG_FC])
  AC_LANG([Fortran])
  AC_COMPILE_IFELSE([dnl
   program binaryformtest
     open(66,file='binaryformtest',form='binary')
   end program binaryformtest
 ],
 [ax_cv_fc_supports_form_binary="yes"],
 [ax_cv_fc_supports_form_binary="no"])
])
if test x"$ax_cv_fc_supports_form_binary" = xyes; then 
AC_DEFINE([FC_FORM_BINARY], 1, 
[Define if Fortran supports output(...,form="binary").]) 
fi])


