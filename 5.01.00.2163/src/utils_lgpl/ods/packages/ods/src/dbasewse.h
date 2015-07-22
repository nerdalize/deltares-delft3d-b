/*
 *  dbasewse.h  -  ODS function prototypes for DBase (WISE format) files
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Bart Adriaanse
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/dbasewse.h,v $
*/
/*
 *
 */

void ODSGetLocDbf3 ( char *fname,  char *locdef,TInt4 maxdef,  char *loclst,
                    TInt4 maxlst, TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetLocDbf4 ( char *fname,  char *locdef,TInt4 maxdef,  char *loclst,
                    TInt4 maxlst, TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetParDbf3 ( char *fname,  char *pardef, TInt4 maxdef,  char *parlst,
                     char *paruni,TInt4 maxlst,  TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetParDbf4 ( char *fname,  char *pardef, TInt4 maxdef,  char *parlst,
                     char *paruni,TInt4 maxlst,  TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetTmeDbf3 ( char *fname,  double *timdef,TInt4 maxdef,  double *timlst,
                    TInt4 maxlst, TInt4 *nrlst, TInt4 *ierror) ;

void ODSGetValDbf3 ( char *fname,  char *locin,    char *parin,  double *timin,
                    TInt4 maxilo, TInt4 maxipa,   TInt4 maxiti,  float misval,
                     char *loc,    char *par,      double *tim,  float *values,
                    TInt4 maxolo, TInt4 maxopa,   TInt4 maxoti, TInt4 *nrloc ,
                    TInt4 *nrpar ,TInt4 *nrtim ,  TInt4 *ierror) ;

void ODSGetValDbf4 ( char *fname,  char *locin,    char *parin,  double *timin,
                    TInt4 maxilo, TInt4 maxipa,   TInt4 maxiti,  float misval,
                     char *loc,    char *par,      double *tim,  float *values,
                    TInt4 maxolo, TInt4 maxopa,   TInt4 maxoti, TInt4 *nrloc ,
                    TInt4 *nrpar ,TInt4 *nrtim ,  TInt4 *ierror) ;
