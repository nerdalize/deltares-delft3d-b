/*
 *  utils.h  -  header file for `misc' library
 *		formerly called 'misc.h' but renamed to resolve a name
 *		with /usr/include/misc.h on an HP.
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:50p $
 *  $Source: /u/cvsroot/gpp/include/utils.h,v $
*/
/*
 *
 */

#ifndef GPP_UTILS_H_INCLUDED
#define GPP_UTILS_H_INCLUDED

#include "portable.h"


/*  Convert Calendar to Julian date.
 */
TReal8 Calendar2Julian(
    TInt4 year, TInt4 month, TInt4 day,
    TInt4 hour, TInt4 min, TInt4 sec );

/*  Convert Julian to Calendar date.
 */
void Julian2Calendar(
    TReal8 julian,
    TInt4 * year, TInt4 * month, TInt4 * day,
    TInt4 * hour, TInt4 * min, TInt4 * sec );

/*  Produce human-readable date-string from a Julian date.
 */
TString Julian2String(
    TReal8 julian );
TString Julian2String(
    TReal8 julian );
TString Time2String(
    TReal8 time );

/*  Produce human-readable date-string from a Calendar date.
 */
TString Calendar2String(
    TInt4 year, TInt4 month, TInt4 day,
    TInt4 hour, TInt4 min, TInt4 sec );

/*  Convert a date-string to a Julian date.
 */
TReal8 String2Julian(
    TString date_string );

/*  Convert a date-string to a Calendar date.
 */
void String2Calendar(
    TString date_string,
    TInt4 * year, TInt4 * month, TInt4 * day,
    TInt4 * hour, TInt4 * min, TInt4 * sec );

TString Julian2TaString( TReal8 julian );

TString UT_StripSpaces( TString string );

TString UT_StripAndReplaceSpaces( TString string );

TString UT_Real8ToString( TReal8 value );

#endif  /* GPP_UTILS_H_INCLUDED */

