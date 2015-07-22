/*
 *  itrans.c  -  ODS translate C file error code to ODS error code
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/itrans.c,v $
*/
/*
 *
 */


/*   Date:       5 Aug 1993                                           */
/*   Time:       11:25                                                */
/*   Program:    ITRANS.C                                             */
/*   Version:    1.04                                                 */
/*   Programmer: Andr‚ Hendriks                                       */
/*   (c) Copyright 1993 Delft Hydraulics                              */
/*   Previous version(s):                                             */
/*   1.03 -- 4 Aug 1993 -- 09:58 -- Operating System: DOS             */
/*   1.02 -- 4 Aug 1993 -- 09:57 -- Operating System: DOS             */
/*   1.01 -- 4 Aug 1993 -- 09:56 -- Operating System: DOS             */
/*   1.00 -- 4 Aug 1993 -- 09:46 -- Operating System: DOS             */
/*   0.0 -- 4 Aug 1993 -- 08:21 -- Operating System: DOS              */
/*   Project:    Open Data Structuur                                  */
/*   Module:     ferror_Translate                                     */
/*   Function:   Translate ferror error into error from ODSErr.h      */
/*   Comment:    MICROSOFT C 6.00 VERSION                             */
/*   Reference:                                                       */
/*   Review:                                                          */

#include <errno.h>

#include "portable.h"
#include "ods.h"

#define FALSE 0
#define TRUE  1

void itrans ( int iostat, TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    I/O  Description                                       */
/*        ------  ---  -----------------------------                     */
/*        ierror   O   Errorcode from odserr.h                           */
/*        iostat   I   C ferror() code                                   */
/*                                                                       */
/*************************************************************************/

   {

   *ierror = (TInt4) iostat ;

   return ;

   }
