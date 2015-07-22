//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2012.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
// $Id: f902c.c 1180 2012-01-13 17:05:48Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/plugin_3dmortrafrm_c/src/f902c.c $
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef min
#  define min(a,b) (a)<(b) ? (a) : (b)
#  define max(a,b) (a)>(b) ? (a) : (b)
#endif

#if defined(WIN32)
#  include <windows.h>
#elif defined(salford32)
#  include <windows.h>
#elif defined(HAVE_CONFIG_H)
#  include <dlfcn.h>
#endif

#if defined(WIN32)
#  define OPEN_SHARED_LIBRARY  OPEN_SHARED_LIBRARY
#  define CLOSE_SHARED_LIBRARY CLOSE_SHARED_LIBRARY
#  define PERFORM_FUNCTION_EQTRAN  PERF_FUNCTION_EQTRAN
#  define PERFORM_FUNCTION_EROSILT PERF_FUNCTION_EROSILT
#  define PERFORM_FUNCTION_FALLVE  PERF_FUNCTION_FALLVE
#  define PERFORM_FUNCTION_CULVERT PERF_FUNCTION_CULVERT
#  define STDCALL
#elif defined(salford32)
#  define OPEN_SHARED_LIBRARY  OPEN_SHARED_LIBRARY
#  define CLOSE_SHARED_LIBRARY CLOSE_SHARED_LIBRARY
#  define PERFORM_FUNCTION_EQTRAN  PERF_FUNCTION_EQTRAN
#  define PERFORM_FUNCTION_EROSILT PERF_FUNCTION_EROSILT
#  define PERFORM_FUNCTION_FALLVE  PERF_FUNCTION_FALLVE
#  define PERFORM_FUNCTION_CULVERT PERF_FUNCTION_CULVERT
#  define STDCALL __stdcall
#elif defined(HAVE_CONFIG_H)
#   include "config.h"
#  define OPEN_SHARED_LIBRARY      FC_FUNC(open_shared_library,OPEN_SHARED_LIBRARY)
#  define CLOSE_SHARED_LIBRARY     FC_FUNC(close_shared_library,CLOSE_SHARED_LIBRARY)
#  define PERFORM_FUNCTION         FC_FUNC(perf_function,PERFORM_FUNCTION)
#  define PERFORM_FUNCTION_EQTRAN  FC_FUNC(perf_function_eqtran,PERFORM_FUNCTION_EQTRAN)
#  define PERFORM_FUNCTION_EROSILT FC_FUNC(perf_function_erosilt,PERFORM_FUNCTION_EROSILT)
#  define PERFORM_FUNCTION_FALLVE  FC_FUNC(perf_function_fallve,PERFORM_FUNCTION_FALLVE)
#  define PERFORM_FUNCTION_CULVERT FC_FUNC(perf_function_culvert,PERFORM_FUNCTION_CULVERT)
#  define STDCALL
#endif

/*
 *
 * Connection routine between F90 (main) -> C (interface) -> F90 (DLL).
 * Special attention to the WINAPI define, which is needed if the DLL is written in F90
 *
 */

#if defined(WIN32)
    typedef HMODULE DllHandle;
#elif defined(salford32)
    typedef HMODULE DllHandle;
#elif defined(HAVE_CONFIG_H)
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

void RemoveTrailingBlanks_dll(char * String)
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
#if defined(WIN32) || defined (HAVE_CONFIG_H)
long STDCALL OPEN_SHARED_LIBRARY(long * sharedDLLHandle, char * library, long length_lib)
#elif defined (salford32)
extern "C" OPEN_SHARED_LIBRARY(long * sharedDLLHandle, char * library, long length_lib)
#endif
{
    long error = 1;
    SharedDLL * tmpSharedDLL = NULL;
    char * lib_name = strFcpy(library, length_lib);

    *sharedDLLHandle = 0;

    RemoveTrailingBlanks_dll(lib_name);

    tmpSharedDLL = (SharedDLL *) malloc(sizeof(SharedDLL));
#if defined(WIN32)
    tmpSharedDLL->dllHandle = LoadLibrary(lib_name);
#elif defined(salford32)
    tmpSharedDLL->dllHandle = LoadLibrary(lib_name);
#elif defined(HAVE_CONFIG_H)
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

#if defined (WIN32) || defined (HAVE_CONFIG_H)
long STDCALL CLOSE_SHARED_LIBRARY(long * sharedDLLHandle)
#elif defined (salford32)
extern "C" CLOSE_SHARED_LIBRARY(long * sharedDLLHandle)
#endif
{
    SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

#if defined(WIN32)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(salford32)
    (void) FreeLibrary(sharedDLL->dllHandle);
#elif defined(HAVE_CONFIG_H)
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
#if defined(WIN32)
long STDCALL PERFORM_FUNCTION_EQTRAN(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              long   * sbc_total          ,
                              double * sbc                ,
                              double * sbcu               ,
                              double * sbcv               ,
                              double * sbwu               ,
                              double * sbwv               ,
                              long   * equi_conc          ,
                              double * cesus              ,
                              double * ssus               ,
                              double * sswu               ,
                              double * sswv               ,
                              double * t_relax            ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined(salford32)
extern "C" PERFORM_FUNCTION_EQTRAN(  long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              long   * sbc_total          ,
                              double * sbc                ,
                              double * sbcu               ,
                              double * sbcv               ,
                              double * sbwu               ,
                              double * sbwv               ,
                              long   * equi_conc          ,
                              double * cesus              ,
                              double * ssus               ,
                              double * sswu               ,
                              double * sswv               ,
                              double * t_relax            ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined (HAVE_CONFIG_H)
long STDCALL PERFORM_FUNCTION_EQTRAN(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              long   * sbc_total          ,
                              double * sbc                ,
                              double * sbcu               ,
                              double * sbcv               ,
                              double * sbwu               ,
                              double * sbwv               ,
                              long   * equi_conc          ,
                              double * cesus              ,
                              double * ssus               ,
                              double * sswu               ,
                              double * sswv               ,
                              double * t_relax            ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#endif
{

  long error = 1;
  long len = -1;
#if defined(WIN32)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    long   *, double *, double *, double *, double *, double *,
                                    long   *, double *, double *, double *, double *, double *,
                                    char   *, long    , long    );
#elif defined (HAVE_CONFIG_H)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    long   *, double *, double *, double *, double *, double *,
                                    long   *, double *, double *, double *, double *, double *,
                                    char   *, long    , long    );
#endif
  MyProc proc;
  char * fun_name;
  SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

  fun_name = strFcpy(function, length_function);
  RemoveTrailingBlanks_dll(fun_name);

#if defined(WIN32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(salford32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(HAVE_CONFIG_H)
  proc = (MyProc) dlsym( sharedDLL->dllHandle, fun_name);
#endif

  if ( proc != NULL )
  {
     error = 0;
#if defined(WIN32)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      sbc_total   ,
                      sbc         ,
                      sbcu        ,
                      sbcv        ,
                      sbwu        ,
                      sbwv        ,
                      equi_conc   ,
                      cesus       ,
                      ssus        ,
                      sswu        ,
                      sswv        ,
                      t_relax     ,
                      message     ,
                      length_dll_strings, length_message );
#elif defined (HAVE_CONFIG_H)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      sbc_total   ,
                      sbc         ,
                      sbcu        ,
                      sbcv        ,
                      sbwu        ,
                      sbwv        ,
                      equi_conc   ,
                      cesus       ,
                      ssus        ,
                      sswu        ,
                      sswv        ,
                      t_relax     ,
                      message     , length_dll_strings, length_message );
#endif
  }
  free(fun_name); fun_name = NULL;

  return error;
}
/*
 * ============================================================================
 */
#if defined(WIN32)
long STDCALL PERFORM_FUNCTION_EROSILT(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * sink               ,
                              double * source             ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined(salford32)
extern "C" PERFORM_FUNCTION_EROSILT(  long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * sink               ,
                              double * source             ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined (HAVE_CONFIG_H)
long STDCALL PERFORM_FUNCTION_EROSILT(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * sink               ,
                              double * source             ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#endif
{

  long error = 1;
  long len = -1;
#if defined(WIN32)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, double *,
                                    char   *, long    , long    );
#elif defined (HAVE_CONFIG_H)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, double *,
                                    char   *, long    , long    );
#endif
  MyProc proc;
  char * fun_name;
  SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

  fun_name = strFcpy(function, length_function);
  RemoveTrailingBlanks_dll(fun_name);

#if defined(WIN32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(salford32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(HAVE_CONFIG_H)
  proc = (MyProc) dlsym( sharedDLL->dllHandle, fun_name);
#endif

  if ( proc != NULL )
  {
     error = 0;
#if defined(WIN32)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      sink        , source      ,
                      message     , length_dll_strings, length_message );
#elif defined (HAVE_CONFIG_H)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      sink        , source      ,
                      message     , length_dll_strings, length_message );
#endif
  }
  free(fun_name); fun_name = NULL;

  return error;
}
/*
 * ============================================================================
 */
#if defined(WIN32)
long STDCALL PERFORM_FUNCTION_FALLVE(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * ws                 ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined(salford32)
extern "C" PERFORM_FUNCTION_FALLVE(  long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * ws                 ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined (HAVE_CONFIG_H)
long STDCALL PERFORM_FUNCTION_FALLVE(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * ws                 ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#endif
{

  long error = 1;
  long len = -1;
#if defined(WIN32)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, char   *, long    , long    );
#elif defined (HAVE_CONFIG_H)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, char   *, long    , long    );
#endif
  MyProc proc;
  char * fun_name;
  SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

  fun_name = strFcpy(function, length_function);
  RemoveTrailingBlanks_dll(fun_name);

#if defined(WIN32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(salford32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(HAVE_CONFIG_H)
  proc = (MyProc) dlsym( sharedDLL->dllHandle, fun_name);
#endif

  if ( proc != NULL )
  {
     error = 0;
#if defined(WIN32)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      ws          , message     ,
                      length_dll_strings, length_message );
#elif defined (HAVE_CONFIG_H)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      ws          , message     ,
                      length_dll_strings, length_message );
#endif
  }
  free(fun_name); fun_name = NULL;

  return error;
}
/*
 * ============================================================================
 */
#if defined(WIN32)
long STDCALL PERFORM_FUNCTION_CULVERT(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * discharge          ,
                              double * zpos1              ,
                              double * zpos2              ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined(salford32)
extern "C" PERFORM_FUNCTION_CULVERT(  long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * discharge          ,
                              double * zpos1              ,
                              double * zpos2              ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#elif defined (HAVE_CONFIG_H)
long STDCALL PERFORM_FUNCTION_CULVERT(long   * sharedDLLHandle    ,
                              char   * function           ,
                              long   * dll_integers       ,
                              long   * max_integers       ,
                              double * dll_reals          ,
                              long   * max_reals          ,
                              char   * dll_strings        ,
                              long   * max_strings        ,
                              double * discharge          ,
                              double * zpos1              ,
                              double * zpos2              ,
                              char   * message            ,
                              long     length_function    ,
                              long     length_dll_strings ,
                              long     length_message     )
#endif
{

  long error = 1;
  long len = -1;
#if defined(WIN32)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, double *, double *,
                                    char   *, long    , long    );
#elif defined (HAVE_CONFIG_H)
  typedef void * (STDCALL * MyProc)(long   *, long   *,
                                    double *, long   *,
                                    char   *, long   *,
                                    double *, double *, double *,
                                    char   *, long    , long    );
#endif
  MyProc proc;
  char * fun_name;
  SharedDLL * sharedDLL = (SharedDLL *) (*sharedDLLHandle);

  fun_name = strFcpy(function, length_function);
  RemoveTrailingBlanks_dll(fun_name);

#if defined(WIN32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(salford32)
  proc = (MyProc) GetProcAddress( sharedDLL->dllHandle, fun_name);
#elif defined(HAVE_CONFIG_H)
  proc = (MyProc) dlsym( sharedDLL->dllHandle, fun_name);
#endif

  if ( proc != NULL )
  {
     error = 0;
#if defined(WIN32)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      discharge   , zpos1       ,
                      zpos2       , message     ,
                      length_dll_strings, length_message );
#elif defined (HAVE_CONFIG_H)
     (void *) (*proc)(dll_integers, max_integers,
                      dll_reals   , max_reals   ,
                      dll_strings , max_strings ,
                      discharge   , zpos1       ,
                      zpos2       , message     ,
                      length_dll_strings, length_message );
#endif
  }
  free(fun_name); fun_name = NULL;

  return error;
}
