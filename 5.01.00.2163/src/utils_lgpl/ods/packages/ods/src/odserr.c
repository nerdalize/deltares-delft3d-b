/* @begin@ */
/*
 *  odserr.c -  ODS routine to return error text
 *
 *  Copyright (C) 1996 Delft Hydraulics
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the following functions:
 *  - odserr():    Return error text
 */

/*
 *  $Author: Markus $
 *  $Date: 1-04-03 10:52 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/odserr.c,v $
 */

/* @end@ */

/*
 * Include files and definitions
 */

#include <stdlib.h>
#include "ods.h"

/*
 * Static variables
 */

static char *ods_error_text[100] ;

#ifndef NOPROTO
   void DllExport odserr( long ierror , char **text )
#else
   void DllExport odserr( ierror , text )
   long ierror ;
   char **text  ;
#endif
{
    static int init = 1 ;

/* Initialise the array
*/
    if ( init == 1 )
    {
       init = 0 ;
       ods_error_text[IEOK  ] = "Okay" ;
       ods_error_text[IEUNDE] = "Unable to determine filetype" ;
       ods_error_text[IEUNKN] = "File type not implemented in this version" ;
       ods_error_text[IETYPE] = "File is not of indicated type" ;
       ods_error_text[IENOFI] = "File does not exist" ;
       ods_error_text[IENOHA] = "No free handle (too many open files)" ;
       ods_error_text[IEFIAO] = "File already open" ;
       ods_error_text[IEFLCK] = "File locked by other program" ;
       ods_error_text[IEFIRO] = "Access denied/file is read-only" ;
       ods_error_text[IERLCK] = "Record locked by other program" ;
       ods_error_text[IENLCK] = "Cannot lock file/record/share not installed" ;
       ods_error_text[IEFNNW] = "File not new" ;
       ods_error_text[IEINFO] = "File does not contain wanted information" ;
       ods_error_text[IEUEOF] = "Unexpected end of file" ;
       ods_error_text[IEPMNY] = "Too many parameters found for array space" ;
       ods_error_text[IELMNY] = "Too many locations found for array space" ;
       ods_error_text[IETMNY] = "Too many times found for array space" ;
       ods_error_text[IEPLOW] = "Nr. of parameters < 1" ;
       ods_error_text[IELLOW] = "Nr. of locations < 1" ;
       ods_error_text[IETLOW] = "Nr. of times < 1" ;
       ods_error_text[IETIME] = "Bad date/time format" ;
       ods_error_text[IERUNI] = "RunId not equal" ;
       ods_error_text[IEPARI] = "Par.  not equal" ;
       ods_error_text[IELOCI] = "Loc.  not equal" ;
       ods_error_text[IETIMI] = "Time  not equal" ;
       ods_error_text[IESTEP] = "Timestep too small (time can not be represented)" ;
       ods_error_text[IEBUFF] = "Buffer space too small (warn WL technical support)" ;
       ods_error_text[IEDISK] = "No space left on device" ;
       ods_error_text[IEOTHR] = "Error of unspecified nature" ;
    }

/* Set a pointer to the error text
*/
    if ( ierror < 0 || ierror > 99 )
    {
       *text = NULL ;
    }
    else
    {
       *text = ods_error_text[ierror] ;
    }

/* Make sure an understandable text is returned
*/
    if ( *text == NULL )
    {
       *text = "Unknown error code" ;
    }
    return ;
}

/* Quick hack: solve unresolved external problem in Delft-FEWS
*/
void DllExport odserr2( long ierror , char **text ) {
    odserr( ierror, text );
}
