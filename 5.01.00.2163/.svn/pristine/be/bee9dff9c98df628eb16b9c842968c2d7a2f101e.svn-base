/*
 *  opnclose.h  -  ODS file open and close function prototypes
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Eric Verschuur
 */
/*
 *  $Author: Markus $
 *  $Date: 6-06-03 10:40 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/opnclose.h,v $
 */
#ifndef OPNCLOSE_H
#define OPNCLOSE_H

#include "portable.h"

/* Use separate routines for Fortran and C
   - take care of calling conventions
*/
#if defined (SUN) || defined (DEC) || defined(__sgi) || defined(linux)
#    define F_OPENFL openfl_
#    define F_OPNNEF opnnef_
#    define F_CLOSFL closfl_
#    define F_CLOSAL closal_
#endif

#if defined (USE_HPUX)
#    define F_OPENFL openfl
#    define F_OPNNEF opnnef
#    define F_CLOSFL closfl
#    define F_CLOSAL closal
#endif

/* Macro FOR_CALL not empty for WINNT
*/
#if defined (USE_WINNT)
#    define F_OPENFL OPENFL
#    define F_OPNNEF OPNNEF
#    define F_CLOSFL CLOSFL
#    define F_CLOSAL CLOSAL
#endif
/* The order is important
*/
#define OPENFL C_openfl
#define OPNNEF C_opnnef
#define CLOSFL C_closfl
#define CLOSAL C_closal


/* ODS File pointer type (mostly private) */
typedef struct OdsFile
{
        struct OdsFile *next;
        FILE  *fp;
        TInt4 id;         /* program id */
        BInt4 *deffds;    /* allocate in case of Nefis file */
        BInt4 *datfds;    /* allocate in case of Nefis file */
        TInt4 filepos;    /* in case of close&reopen, save position in file */
        int   filestatus; /* codes for closed, re-opened, error, etc. */
        TInt4 ftype;      /* filetype as used by ODS */
        TInt4 unitnr;     /* for fortran calls */
        char  *filename;  /* allocate and copy fname into */
} *OdsFilePtr;

/* Fortran routines - not in header file, because of conflicts on WINNT */
#if 0
void FOR_CALL F_OPENFL(
        TString fname,
#if defined( IN_BETWEEN )
        int     lfname,
#endif
        TInt4  *ftype,
        TInt4  *unitnr,
        TInt4  *ierror
#if ! defined( IN_BETWEEN )
       ,int     lfname
#endif
        ) ;

void FOR_CALL F_OPNNEF (
        TString fname,
#if defined( IN_BETWEEN )
        int     lfname,
#endif
        TInt4  *ftype,
        BInt4  *datfds,
        BInt4  *deffds,
        TInt4  *ierror
#if ! defined( IN_BETWEEN )
       ,int     lfname
#endif
        ) ;

void FOR_CALL F_CLOSFL(
        TString fname,
#if defined( IN_BETWEEN )
        int     lfname,
#endif
        TInt4  *ierror
#if ! defined( IN_BETWEEN )
       ,int     lfname
#endif
        ) ;

void FOR_CALL F_CLOSAL(
        TString fname,
#if defined( IN_BETWEEN )
        int     lfname,
#endif
        TInt4  *ierror
#if ! defined( IN_BETWEEN )
       ,int     lfname
#endif
        ) ;
#endif /* Hide prototypes */

/* C routines */

void OPENFL ( TString fname, TInt4 *ftype, TInt4 *unitnr, TInt4 *ierror );

void OPNNEF ( TString fname, TInt4  *ftype, BInt4 *datfds, BInt4 *deffds,
                       TInt4 *ierror );

OdsFilePtr OdsOpenFile ( TString fname, TInt4 *ftype, FILE **fp, TInt4 *ierror );

void CLOSFL ( TString fname, TInt4 *ierror );

void DllExport CLOSAL ( TString fname, TInt4 *ierror );

#endif  /* OPNCLOSE_H */
