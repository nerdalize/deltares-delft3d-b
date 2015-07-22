/*
 *  samples.h -  ODS function prototypes for samples files
 *
 *  Copyright (C) 1997 Delft Hydraulics
 *
 *  Arjen Markus
 */

/*
 *  $Author: Markus $
 *  $Date: 1-04-03 10:52 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/samples.h,v $
*/
/*
 *
 */
void SamplesGetLoc ( char *fname,   TInt4 *ftype,  char *locdef  ,TInt4 *maxdef,
                     TInt4 *pardep, TInt4 *timdep, TInt4 *maxlst ,
                     char *loclst,  TInt4 *loctyp, TInt4 *locnr  , TInt4 *nrlst,
                     TInt4 *ierror , TString option ) ;

void SamplesGetPar ( char *fname, TInt4 *ftype,  char *pardef,TInt4 *maxdef,
                     TInt4 *timdep, TInt4 *locdep, TInt4 *maxlst, TInt4 *lang,
                     char *parlst, char *paruni, TInt4 *partyp,
                     TInt4 *parcod,TInt4 *nrlst,TInt4 *ierror ,TString option);

void SamplesGetTme ( char *fname,  TInt4 *ftype, double *timdef,TInt4 *maxdef,
                     TInt4 *pardep, TInt4 *locdep, TInt4 *maxlst,
                     double *timlst,TInt4 *timtyp, TInt4 *nrtim, TInt4 *ierror,
                     TString option ) ;

void SamplesGetDim ( char  *fname,  TInt4 *ftype,  char *dim,   TInt4 *pardep,
                     TInt4 *timdep, TInt4 *locdep, TInt4 *ndim, TInt4 *ierror,
                     TString option ) ;

void SamplesGetMat ( char *fname,  TInt4 *ftype,  TInt4 *parcod, TInt4 *loc,
                     double *tim,  float *misval, TInt4 *i3gl,   TInt4 *maxdim,
                     float *values,TInt4 *ierror , TString option ) ;

void SamplesGetGrd ( TString   fname,  TInt4 * ftype,   TInt4 * indloc,
                     TInt4   * indx,   TInt4 * nocell,  TInt4 * igisty,
                     TInt4   * ierror ) ;
