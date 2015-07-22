//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2012.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: util_mf.h 2107 2013-01-17 08:13:03Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/deltares_common/packages/deltares_common_c/include/util_mf.h $
/*------------------------------------------------------------------------------
//  Delft3D - C Utilities
//  Global definitions
//
//  Irv.Elshoff@deltares.nl
//  2 mar 05
//
//------------------------------------------------------------------------------*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>

#if defined (WIN32)
#   include <sys/types.h>
#   include <sys/stat.h>
#   include <windows.h>
#else
#   include <sys/times.h>
#   include <unistd.h>
#   include <libgen.h>
#endif

#define MAX_CMD 1000


/* FTN_CAPITAL is assumed to be the default value */

#if HAVE_CONFIG_H
#   include "config.h"
#   define STDCALL  /* nothing */
#   define CUTIL_GETHW  FC_FUNC(cutil_gethw,CUTIL_GETHW)
#   define CUTIL_GETMP  FC_FUNC(cutil_getmp,CUTIL_GETMP)
#   define CUTIL_GETENV FC_FUNC(cutil_getenv,CUTIL_GETENV)
#   define CUTIL_SYSTEM FC_FUNC(cutil_system,CUTIL_SYSTEM)
#   define CUTIL_CGETCP FC_FUNC(cutil_cgetcp,CUTIL_CGETCP)
#   define CUTIL_CDATE  FC_FUNC(cutil_cdate,CUTIL_CDATE)
#   define CUTIL_CSTOP  FC_FUNC(cutil_cstop,CUTIL_CSTOP)
#else
// WIN32
#   define STDCALL  /* nothing */
#   define CUTIL_GETHW  CUTIL_GETHW
#   define CUTIL_GETMP  CUTIL_GETMP
#   define CUTIL_GETENV CUTIL_GETENV
#   define CUTIL_SYSTEM CUTIL_SYSTEM
#   define CUTIL_CGETCP CUTIL_CGETCP
#   define CUTIL_CDATE  CUTIL_CDATE
#   define CUTIL_CSTOP  CUTIL_CSTOP
#endif


#if defined (__cplusplus)
    extern "C" {
#endif

#if !defined (WIN32)
void    STDCALL         CUTIL_GETMP (char *, int *, int *);
void    STDCALL         CUTIL_GETHW (char *, int *, char *, int *, int *, char *, int *, char *, int *, int *);
void    STDCALL         CUTIL_GETENV(char *, int *, char *, int *);
void    STDCALL         CUTIL_SYSTEM(char *, int *);
#else
void    STDCALL         CUTIL_GETMP (char *, int *, int *, int);
void    STDCALL         CUTIL_GETHW (char *, int *, char *, int *, int *, char *, int *, char *, int *, int *, int, int, int, int);
void    STDCALL         CUTIL_GETENV(char *, int *, char *, int *, int, int);
void    STDCALL         CUTIL_SYSTEM(char *, int *, int);
#endif

#if defined (__cplusplus)
    }
#endif


int     isdir           (char *);
void    fstr2cstr       (char *, int, char *);
void    cstr2fstr       (char *, int, char *);

