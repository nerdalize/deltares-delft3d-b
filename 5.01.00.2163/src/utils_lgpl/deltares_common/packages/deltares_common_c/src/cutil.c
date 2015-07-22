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
// $Id: cutil.c 2098 2013-01-11 10:44:36Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/deltares_common/packages/deltares_common_c/src/cutil.c $
//------------------------------------------------------------------------------*/
//  Delft3D - C Utilities
//  Internal (legacy) utility functions
//
//  Irv.Elshoff@deltares.nl
//  27 apr 11
//------------------------------------------------------------------------------*/


#include "util_mf.h"


/*----- Function to determine with a path name is a directory or not.*/

int
isdir (
    char *  path
    ) {

#if defined (WIN32)
    struct _stat     statbuf;
    if (_stat (path, &statbuf) == 0 && (statbuf.st_mode & _S_IFDIR))
#else
    struct stat     statbuf;
    if (stat (path, &statbuf) == 0 && S_ISDIR (statbuf.st_mode))
#endif
        return 1;
    else
        return 0;
    }


/*----- Function to convert a space-padded FORTRAN string to a null-terminated C string.*/

void
fstr2cstr (
    char *  fstr,
    int     len,
    char *  cstr
    ) {

    int     i;

    /*  Look for the last non-space character in the character array */

    for (i = len ; i >= 0 ; i--)
        if (fstr[i] != ' ')
            break;

    /*  Copy backwards all the relevant characters */

    cstr[i+1] = '\0';
    for ( ; i >= 0 ; i--)
        cstr[i] = fstr[i];
    }


/*----- Function to convert a null-terminated C string to a space-padded FORTRAN string. */

void
cstr2fstr (
    char *  cstr,
    int     len,
    char *  fstr
    ) {

    int     i;

    /*  Copy to to but not including the C string terminator */

    for (i = 0 ; i < len && cstr[i] != '\0' ; i++)
        fstr[i] = cstr[i];

    /*  Pad the FORTRAN character array with spaces */

    for ( ; i < len ; i++)
        fstr[i] = ' ';
    }


/*------------------------------------------------------------------------------*/


void STDCALL
CUTIL_CDATE (
#if defined (WIN32)
    char *  date,
    int     date_length
#else
    char *  date
#endif
    ) {

   time_t timer = time(NULL) ;
   size_t max_len = 30 ;
   strncpy (date, asctime (localtime (&timer)), max_len);
   }


void STDCALL
CUTIL_CGETCP (
    double *  cpu
    ) {

#if defined (WIN32)
    *cpu = (double) clock() / CLOCKS_PER_SEC ;
#else
    struct tms buf;
    times (&buf);
    *cpu = (double) ((buf.tms_utime + buf.tms_cutime) * 100.0 / (sysconf(_SC_CLK_TCK)));
    *cpu = *cpu / 100.0;
#endif
    }


void STDCALL
CUTIL_CSTOP (
#if defined (WIN32)
    long *  exitcode,
    char *  message,
    int     message_length
#else
    long *  exitcode,
    char *  message
#endif
    ) {

    printf ("%s\n", message);
    exit (*exitcode);
    }


void STDCALL
CUTIL_GETENV (
#if !defined (WIN32)
    char *  name,
    int *   lenname,
    char *  value,
    int *   lenvalue
#else
    char *  name,
    int *   lenname,
    char *  value,
    int *   lenvalue,
    int     name_LENGTH,
    int     value_LENGTH
#endif
    ) {

    char    buf [10000];
    char *  enval;

    /*----  Convert environment variable name to a C string, look it up,
            and if the result exists convert it to a Fortran string. */

    fstr2cstr (name, *lenname, buf);

    if ((enval = getenv (buf)) != NULL)
        cstr2fstr (enval, *lenvalue, value);
    }


void STDCALL
CUTIL_SYSTEM (
#if !defined (WIN32)
    char *  command,
    int *   len
#else
    char *  command,
    int *   len,
    int     command_LENGTH
#endif
    ) {

    char    localcmd [MAX_CMD+1];
#if defined (WIN32)
    int                 success ;
    STARTUPINFO         start ;
    PROCESS_INFORMATION process ;
#endif

    fstr2cstr (command, *len, localcmd);

#if defined (WIN32)
    start.cb         = sizeof(STARTUPINFO) ;
    start.lpReserved = NULL ;
    start.lpDesktop  = NULL ;
    start.lpTitle    = NULL ;
    start.dwFlags    = STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW ;
    start.cbReserved2= 0    ;
    start.lpReserved2= NULL ;

    start.wShowWindow= SW_HIDE ;
    start.hStdInput  = GetStdHandle( STD_INPUT_HANDLE ) ;
    start.hStdOutput = GetStdHandle( STD_OUTPUT_HANDLE ) ;
    start.hStdError  = GetStdHandle( STD_ERROR_HANDLE ) ;

    success = CreateProcess( NULL, localcmd, NULL, NULL, 1, 0,
                             NULL, NULL    , &start, &process ) ;
    if ( success ) {
        WaitForSingleObject( process.hProcess, INFINITE ) ;
        CloseHandle( process.hProcess ) ;
        CloseHandle( process.hThread ) ;
    }
#else
    system (localcmd);
#endif
    }


/*------------------------------------------------------------------------------
/*  Routines to determine the location of D3D program and data files based */
/*  on the values of the D3D_HOME and ARCH environment variables. */


#define SUCCESS     0
#define FAILURE     1


static    int     getpath         (char **, char **, char **, char *);
static    void    report_error    (char *);

void STDCALL
CUTIL_GETMP (
#if !defined (WIN32)
    char *  path,
    int *   lenpath,
    int *   result
#else
    char *  path,
    int *   lenpath,
    int *   result,
    int     path_LENGTH
#endif
    ) {

    char    slash;                    /* UNIX or Windows directory separator */
    char    buf [1000];
    char    path_buffer[1000];
    char    drive[1000];
    char    dir[1000];
    char    fname[1000];
    char    ext[1000];
    int     err;
    int     len;
    len = 1000;
    /*----  Get and validate default directory using the location of this binary */

#ifdef WIN32
    slash = '\\';
    GetModuleFileName(NULL,path_buffer,len);
    err = _splitpath_s(path_buffer, drive, len, dir, len, fname, len, ext, len);
    if (err != 0) {
        report_error ("Unable to read/split the executable directory");
        *result = FAILURE;
        return;
        }
    sprintf (path_buffer, "%s%s", drive, dir);
#else
    slash = '/';
    readlink("/proc/self/exe", path_buffer,len);
    sprintf (path_buffer, "%s%c", dirname(path_buffer), slash);
#endif
    /*---- release version: directory default should be next to directory bin */
    sprintf (buf, "%s..%cdefault", path_buffer, slash);

    if (!isdir(buf)) {
        /*---- Try the (Windows) debug location way down in the source code tree itself */
        sprintf (buf, "%s..%c..%c..%cflow2d3d%cdefault", path_buffer, slash, slash, slash, slash);
        if (!isdir(buf)) {
            report_error ("Directory \"default\" does not exist");
            *result = FAILURE;
            return;
            }
        }
    /*---- The path should end with a slash. Must be added after being checked with isdir. */
    sprintf (buf, "%s%c", buf, slash);
    cstr2fstr (buf, *lenpath, path);
    *result = SUCCESS;
    }


void STDCALL
CUTIL_GETHW (
#if !defined (WIN32)
    char *  pathp,
    int *   lenpathp,
    char *  pathd,
    int *   lenpathd,
    int *   aloneint,
    char *  fpathp,
    int *   lenfpathp,
    char *  fpathd,
    int *   lenfpathd,
    int *   result
#else
    char *  pathp,
    int *   lenpathp,
    char *  pathd,
    int *   lenpathd,
    int *   aloneint,
    char *  fpathp,
    int *   lenfpathp,
    char *  fpathd,
    int *   lenfpathd,
    int *   result
#endif
    ) {

    char *  modname;
    char *  arch        = NULL;
    char *  d3d_home    = NULL;

    char *  envpath        = NULL;
    char    slash;                    /* UNIX or Windows directory separator */
    char    pathp_buf  [10000];
    char    pathd_buf  [10000];
    char    fpathp_buf [10000];
    char    fpathd_buf [10000];


    modname = "flow";

    /*----  Get and validate paths from environment variables */

    if (getpath (&arch, &d3d_home, &envpath, &slash) == FAILURE) {
    *result = FAILURE;
    return;
        }

    /*----  Assemble final path strings */

    if (envpath = d3d_home) {
        if (arch == NULL) {
            sprintf (pathp_buf,  "%s%c%s%cbin%c",       envpath, slash, modname, slash, slash);
            sprintf (pathd_buf,  "%s%c%s%cdefault%c",   envpath, slash, modname, slash, slash);
            sprintf (fpathp_buf, "%s%cflow%cbin%c",     envpath, slash, slash, slash);
            sprintf (fpathd_buf, "%s%cflow%cdefault%c", envpath, slash, slash, slash);
        }
    else {
            sprintf (pathp_buf,  "%s%c%s%c%s%cbin%c",       envpath, slash, arch, slash, modname, slash, slash);
            sprintf (pathd_buf,  "%s%c%s%c%s%cdefault%c",   envpath, slash, arch, slash, modname, slash, slash);
            sprintf (fpathp_buf, "%s%c%s%cflow%cbin%c",     envpath, slash, arch, slash, slash, slash);
            sprintf (fpathd_buf, "%s%c%s%cflow%cdefault%c", envpath, slash, arch, slash, slash, slash);
            }
    }
    else {
        sprintf (pathp_buf,  "%s%cbin%c",     envpath, slash, slash);
        sprintf (pathd_buf,  "%s%cdefault%c", envpath, slash, slash);
        sprintf (fpathp_buf, "%s%cbin%c",     envpath, slash, slash);
        sprintf (fpathd_buf, "%s%cdefault%c", envpath, slash, slash);
    }

    if ((int) strlen (pathp_buf)  >= *lenpathp  ||
        (int) strlen (pathd_buf)  >= *lenpathd  ||
        (int) strlen (fpathp_buf) >= *lenfpathp ||
        (int) strlen (fpathd_buf) >= *lenfpathd
    ) {
        report_error ("The D3D_HOME environment variable is too long");
        *result = FAILURE;
        return;
      }

    cstr2fstr (pathp_buf,  *lenpathp,  pathp);
    cstr2fstr (pathd_buf,  *lenpathd,  pathd);
    cstr2fstr (fpathp_buf, *lenfpathp, fpathp);
    cstr2fstr (fpathd_buf, *lenfpathd, fpathd);

    *result = SUCCESS;
    }


static int
getpath (
    char *  *arch,
    char *  *d3d_home,
    char *  *envpath,
    char    *slash
    ) {

    //  The ARCH and D3D_HOME envars are no longer necessary.
    //  This routine now just returns dummy values.
    //  Irv.Elshoff@Deltares.NL, 27 apr 11

    *arch     = "ARCH";     // getenv ("ARCH");
    *d3d_home = "D3D_HOME"; // getenv ("D3D_HOME");
    *envpath  = *d3d_home;

#ifdef WIN32
    *slash = '\\';
#else
    *slash = '/';
#endif

    return SUCCESS;
}

static void
report_error (
    char *  message
    ) {
    printf ("*** ERROR %s. Check installation procedure\n", message);
    fflush (stdout);
    }

