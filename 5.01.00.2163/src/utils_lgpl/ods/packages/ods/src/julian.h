/*
 *  julian.h  -  ODS prototype for julian.c
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/julian.h,v $
*/
/*
 *
 */

#ifdef SUN
#   define JULIAN julian_
#else
#   define JULIAN julian
#endif

#ifndef NOPROT

void FUNTYPE JULIAN ( TInt4 *iyear,
                      TInt4 *imonth,
                      TInt4 *iday,
                      TInt4 *ihour,
                      TInt4 *imin,
                      TInt4 *isec,
                      TReal8 *jdate) ;

#endif

