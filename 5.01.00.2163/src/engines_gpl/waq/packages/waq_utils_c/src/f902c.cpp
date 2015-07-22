//  Copyright(C) Stichting Deltares, 2012.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License version 3,
//  as published by the Free Software Foundation.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program. If not, see <http://www.gnu.org/licenses/>.
//
//  contact: delft3d.support@deltares.nl
//  Stichting Deltares
//  P.O. Box 177
//  2600 MH Delft, The Netherlands
//
//  All indications and logos of, and references to registered trademarks 
//  of Stichting Deltares remain the property of Stichting Deltares. All 
//  rights reserved.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef min
#  define min(a,b) (a)<(b) ? (a) : (b)
#  define max(a,b) (a)>(b) ? (a) : (b)
#endif

#if defined(WIN32) || defined(SALF)
#  include <windows.h>
#elif HAVE_CONFIG_H
#  define LINUX
#  include <dlfcn.h>
#endif

#if defined(WIN32)
#  define OPEN_SHARED_LIBRARY  OPEN_SHARED_LIBRARY
#  define CLOSE_SHARED_LIBRARY CLOSE_SHARED_LIBRARY
#  define PERFORM_FUNCTION PERF_FUNCTION
#  define LOAD____FUNCTION LOAD_FUNCTION
#  define STDCALL /* nothing */
#elif defined(SALF)
#  define OPEN_SHARED_LIBRARY  open_shared_library_
#  define CLOSE_SHARED_LIBRARY close_shared_library_
#  define PERFORM_FUNCTION perf_function_
#  define LOAD____FUNCTION load_function_
#  define STDCALL
#elif defined(LINUX)
#  define OPEN_SHARED_LIBRARY  open_shared_library_
#  define CLOSE_SHARED_LIBRARY close_shared_library_
#  define PERFORM_FUNCTION perf_function_
#  define LOAD____FUNCTION load_function_
#  define STDCALL
#endif

/*
 * Connection routine between F90 (main) -> C (interface) -> F90 (DLL).
 * Special attention to the WINAPI define, which is needed if the DLL is written in F90
 *
 */

#if defined(WIN32) || defined (SALF)
    typedef HMODULE DllHandle;
#elif defined(LINUX)
    typedef void * DllHandle;
#endif

typedef struct {
    DllHandle   dllHandle;
} SharedDLL;

/*
 * ============================================================================
 */
char * strFcpy(char * str_1, int len)
{
    int m;
    char * str_2;
    m = min( len, (int) strlen(str_1));
    str_2 = (char *) malloc( sizeof(char)*(m+1));
    strncpy(str_2, str_1, m);
    str_2[m] = '\0';
    return str_2;
}

void RemoveTrailingBlanksXX(char * String)
{
  int i;
  i = strlen(String)-1;
  while ( String[i] == ' '  ||
          String[i] == '\n' ||
          String[i] == '\t'    )
  {
    String[i] = '\0';
    i--;
  }
  return;
}
/*
 * ============================================================================
 */
extern "C" {
long STDCALL OPEN_SHARED_LIBRARY(long * sharedDLLHandle, char * library, long length_lib)
{
    long error = 1;
    SharedDLL * tmpSharedDLL = NULL;
    char * lib_name = strFcpy(library, length_lib);

    *sharedDLLHandle = 0;

    RemoveTrailingBlanksXX(lib_name);

    tmpSharedDLL = (SharedDLL *) malloc(sizeof(SharedDLL));
#if defined(WIN32)
    tmpSharedDLL->dllHandle = LoadLibrary(lib_name);
#elif defined(LINUX)
    tmpSharedDLL->dllHandle = dlopen(lib_name, RTLD_LAZY);
#endif

    if (tmpSharedDLL->dllHandle != NULL)
    {
        error = 0;
        *sharedDLLHandle = (long) tmpSharedDLL;
    }

    free(lib_name); lib_name = NULL;

    return error;
}
/*
 * ============================================================================
 */
long STDCALL CLOSE_SHARED_LIBRARY(long * sharedDLLHandle)
{
    SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

#if defined(WIN32)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(SALF)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(LINUX)
    (void) dlclose(sharedDLL->dllHandle);
#endif

    /*
     * dllHandle not set to NULL, because FreeLibrary counts the number of 'LoadLibrary's
     */

    return 0;
}
/*
 * ============================================================================
 */
#if defined(XWIN32)
long STDCALL PERFORM_FUNCTION(long  * sharedDLLHandle  ,
                              char  * function,
                              long    length_function,
                              float * pmsa    ,
                              float * fl      ,
                              long  * ipoint  ,
                              long  * increm  ,
                              long  * noseg   ,
                              long  * noflux  ,
                              long  * iexpnt  ,
                              long  * iknmrk  ,
                              long  * noq1    ,
                              long  * noq2    ,
                              long  * noq3    ,
                              long  * noq4    )
#elif defined(LINUX) || defined(SALF) || defined(WIN32)
long STDCALL PERFORM_FUNCTION(long  * sharedDLLHandle ,
                              char  * function,
                              float * pmsa    ,
                              float * fl      ,
                              long  * ipoint  ,
                              long  * increm  ,
                              long  * noseg   ,
                              long  * noflux  ,
                              long  * iexpnt  ,
                              long  * iknmrk  ,
                              long  * noq1    ,
                              long  * noq2    ,
                              long  * noq3    ,
                              long  * noq4    ,
                              long    length_function)
#endif
{
  int error = 1;
  typedef void * (STDCALL * MyProc)(float *, float *,
                            long *, long *, long *, long *, long *,
                            long *, long *, long *, long *, long *);
  MyProc proc;
  char * fun_name;
  SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

  fun_name = strFcpy(function, length_function);
  RemoveTrailingBlanksXX(fun_name);

#if defined(WIN32) || defined (SALF)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(LINUX)
  proc = (MyProc) dlsym( sharedDLL->dllHandle, fun_name);
#endif

  if ( proc != NULL )
  {
     error = 0;
     (void *) (*proc)(pmsa    , fl     ,
                      ipoint  , increm , noseg  , noflux , iexpnt ,
                      iknmrk  , noq1   , noq2   , noq3   , noq4   );
  }
  free(fun_name); fun_name = NULL;

  return error;
}

/*
 * ============================================================================
 */
}
