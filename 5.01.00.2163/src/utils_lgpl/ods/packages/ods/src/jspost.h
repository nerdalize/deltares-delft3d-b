/*
 *  jspost.h  -  ODS function prototypes for JSPOST files
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *   Peter van den Bosch
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/jspost.h,v $
*/
/*
 *
 */

void FUNTYPE ODSGetLocJSP ( char *fname, char *locdef,TInt4 maxdef, char *loclst,
			   TInt4 *loctyp,TInt4 maxlst, TInt4 *nrlst,TInt4 *locnr,
			   TInt4 *ierror, char *option) ;

void FUNTYPE ODSGetParJSP ( char *fname, char *pardef,TInt4 maxdef, char *parlst,
                            char *paruni,TInt4 maxlst,TInt4 *nrlst,TInt4 *partyp,
                           TInt4 *parcod,TInt4 *ierror, char *option ) ;

void FUNTYPE ODSGetTmeJSP ( char *fname,  double *timdef,TInt4 maxdef,  double *timlst,
                   TInt4 maxlst, TInt4 *nrlst,TInt4 *timtyp, TInt4 *ierror,
		    char *option ) ;

void FUNTYPE ODSGetValJSP ( char *fname,  char *locin,    char *parin,  double *timin,
                   TInt4 maxilo, TInt4 maxipa,   TInt4 maxiti,  float misval,
                    char *loc,    char *par,      double *tim,  float *values,
                   TInt4 maxolo, TInt4 maxopa,   TInt4 maxoti, TInt4 *nrloc ,
                   TInt4 *nrpar ,TInt4 *nrtim ,  TInt4 *ierror) ;

void FUNTYPE ODSGetDimJSP ( char *fname,TInt4 *ftype, char *dim,TInt4 pardep,
			   TInt4 timdep,TInt4 locdep,TInt4 *ndim,TInt4 *ierror,
			    char *option ) ;
void FUNTYPE ODSGetMatJSP ( char    *fname,TInt4    *ftype, TInt4   *parcod,
		            double  *tim  , TInt4   *loc  , float *misval  ,
		           TInt4 *maxdim  , float *values ,TInt4 *ierror   ) ;

void FUNTYPE JspOpen( char *fname, FILE **fd, FILE **fs,TInt4 *ierror);
