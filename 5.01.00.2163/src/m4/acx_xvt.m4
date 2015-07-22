dnl Check if --with-xvt[=PREFIX] is specified and
dnl Xvt >= 1.95.0 is installed in the system.
dnl If yes, substitute XVT_CFLAGS, XVT_LIBS with regard to
dnl the specified PREFIX and set with_xvt to PREFIX, or 'yes' if PREFIX
dnl has not been specified. Also HAVE_LIBXVT, HAVE_XVT_H are defined.
dnl If --with-xvt has not been specified, set with_xvt to 'no'.
dnl In addition, an Automake conditional XVT_INSTALLED is set accordingly.
dnl This is necessary to adapt a whole lot of packages that have xvt
dnl bundled as a static library.
AC_DEFUN([AM_WITH_XVT],
[
  AC_MSG_RESULT(                                      )	     
  AC_MSG_RESULT(         Check XVT installation ......)	     

  AM_CONDITIONAL(XVT_INSTALLED, [test "x$with_xvt" != "xno"])

  XVT_VERSION=570
  XVT_CFLAGS=
  XVT_LIBS=

dnl		--- check for XVT environment  
  if test "x$XVT_DSC_DIR" = "x" ; then
        AC_MSG_ERROR([Environment variable XVT_DSC_DIR not defined; \
		      Please check XVT installation ??])   
  fi                               

  if test "x$XVT_DSC_DIR" != x; then
        AC_MSG_RESULT([Environment variable XVT_DSC_DIR: $XVT_DSC_DIR])
        
dnl		--- check for proper XVT installation 
	XVT_XMAPI=$XVT_DSC_DIR/lib/libxvtxmapi.a
	AC_CHECK_FILE($XVT_XMAPI, 
		     [ xvt_found=yes ],
		     [ xvt_found=no  ])
	if test $xvt_found = no; then
		AC_MSG_ERROR([Could not find the XVT library $XVT_XMAPI; \                                     
		              Please check XVT installation ??])
	fi

	XVT_HEADER=$XVT_DSC_DIR/include/xvt.h
	AC_CHECK_FILE($XVT_HEADER,
		     [ xvt_found=yes ],
		     [ xvt_found=no  ])
	if test $xvt_found = no; then
		AC_MSG_ERROR([Could not find the XVT header file $XVT_HEADER; \                                  
		              Please check XVT installation ??])
	fi

dnl		--- include directories for XVT ---
        XVT_INCLUDES=$XVT_DSC_DIR/include

dnl		--- libraries for XVT ---
	XVT_LIBS="$XVT_DSC_DIR/lib/libstatbar.a\
	     	  $XVT_DSC_DIR/lib/libtable.a\
                  $XVT_DSC_DIR/lib/libtoglbutn.a\
                  $XVT_DSC_DIR/lib/libtoolbar.a\
                  $XVT_DSC_DIR/lib/libXmHTML.a\
		  $XVT_DSC_DIR/lib/libxvtxmba$XVT_VERSION.so\
		  $XVT_DSC_DIR/lib/libxvtxmhb$XVT_VERSION.so\
		  $XVT_DSC_DIR/lib/libxvtxmhi$XVT_VERSION.so\
		  $XVT_DSC_DIR/lib/libxvtxmapi.a"
dnl     AC_MSG_RESULT([XVT_LIBS: $XVT_LIBS])

dnl		--- extra loader(LD) flags for XVT
        XVT_LDFLAGS=$XVT_LIBS

dnl		--- X11 window system
dnl     X11_LDFLAGS="$X_PRE_LIBS -lXt $XPLIB $X_LIBS -lX11 $X_EXTRA_LIBS"
        X11_LDFLAGS="$X_PRE_LIBS  $XPLIB $X_LIBS $X_EXTRA_LIBS"
	X11_INCLUDES="/usr/include/X11"                                               
        
dnl		--- UIL native resource compiler 
	UIL="/usr/bin/X11/uil"
	UIL_OPTS="-I$XVT_INCLUDES -I$X11_INCLUDES"
	
dnl		--- CURL compiler 
        CURL="$XVT_DSC_DIR/bin/curl"
	AC_CHECK_FILE($CURL, 
		     [ xvt_found=yes ],
		     [ xvt_found=no  ])
	if test $xvt_found = no; then
		AC_MSG_ERROR([Could not find the CURL compiler $CURL; \                                     
		              Please check XVT installation ??])
	fi

	CURL_OPTS="-r mtf20 -I$X11_INCLUDES -I$XVT_INCLUDES -I../include -s2 -DCURL"
	AC_MSG_RESULT(" CURL settings: $CURL $CURL_OPTS ")	     

	
dnl		--- Help compiler 
        HELP=$XVT_DSC_DIR/bin/helpc
        HELP_OPTS="-f xvt"
        
        AC_MSG_RESULT(         XVT installation OK)
        AC_MSG_RESULT(                            )	             
  fi
dnl  AC_MSG_RESULT([libs: $XVT_LIBS cflags: $XVT_INCLUDES])
  AC_SUBST(XVT_INCLUDES)
  AC_SUBST(XVT_LDFLAGS)
  AC_SUBST(XVT_LIBS)
  AC_SUBST(CURL)
  AC_SUBST(CURL_OPTS)  
  AC_SUBST(HELP)  
  AC_SUBST(HELP_OPTS)    
  AC_SUBST(UIL)  
  AC_SUBST(UIL_OPTS)    
  AC_SUBST(X11_LDFLAGS)
])
