/*
 *  odsver.c  -  return ODS library version number
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/odsver.c,v $
*/
/*
 *
 */

#include "portable.h"
#include "ods.h"

#ifdef SUN
#   define ODSVER odsver_
#else
#   define ODSVER odsver
#endif


/*************************************************************************/
/*    SUBROUTINE ODS version                                             */
/*************************************************************************/
void FUNTYPE ODSVER ( float *version)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       version -          O   Version number as a float (real).        */
/*                                                                       */
/*************************************************************************/
   {
   *version = (float) 1.04 ;
   return ;
   }

