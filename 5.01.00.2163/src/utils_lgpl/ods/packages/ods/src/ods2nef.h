/*
 *  ods2nef.h  -  ODS to NEFIS interface function prototypes
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Eric Verschuur
 */
/*
 *  $Author: Markus $
 *  $Date: 1-04-03 10:52 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/ods2nef.h,v $
*/
/*
 *
 */


void OpenNefisDefDat ( char *fname,  BInt4 *datfds,  BInt4 *deffds,
                            TInt4 *ierror);

void CloseNefisDefDat ( char *fname,   BInt4 *datfds,  BInt4 *deffds,
                           TInt4 *ierror);

void OpenNefisFiles ( char *fname,  TInt4 ftype,    BInt4 *datfds,
                            BInt4 *deffds, char *grp_par, char *grp_res,
                           TInt4 *ierror);

void CloseNefisFiles ( char *fname,   BInt4 *datfds,  BInt4 *deffds,
                          TInt4 *ierror);

void GetNefisTme ( char *fname,    TInt4 *ftype,      double *timdef,
                      TInt4 *maxdef,    double *timlst, TInt4 *maxlst,
                      TInt4 *nrtim,    TInt4 *ierror,    char *option);

void GetNefisMat ( char *fname,  TInt4 *ftype,  TInt4 *parcod,
                         double *tim,  TInt4 *loc,     float *misval,
                        TInt4 *maxdim,  float *values,TInt4 *ierror,
                         char *option);

void GetNefisLoc ( char *fname,  TInt4 *ftype,   char *pardef,
                        TInt4 *maxdef,  char *parlst, TInt4 *maxlst,
                        TInt4 *nrlst,  TInt4 *locnr,  TInt4 *ierror,
                         char *option);

void GetNefisPar ( char *fname,  TInt4 *ftype,   char *pardef,
                        TInt4 *maxdef,  char *parlst,  char *paruni,
                        TInt4 *maxlst, TInt4 *nrlst,  TInt4 *partyp,
                        TInt4 *parcod, TInt4 *ierror,  char *option);

void GetNefisDim ( char *fname,  TInt4 *ftype,   char *dim,
                        TInt4 *pardep, TInt4 *timdep, TInt4 *locdep,
                        TInt4 *ndim,   TInt4 *ierror,  char *option);


