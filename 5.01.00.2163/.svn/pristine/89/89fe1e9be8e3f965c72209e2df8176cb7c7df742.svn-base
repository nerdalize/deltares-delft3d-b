/*
 *  morbagr.h  -  ODS: MORSYS "bagger" files (prototypes)
 *
 *  Copyright (C) 1997 Delft Hydraulics
 *
 *  Derived from code by Cor ten Napel (mor_sys.f)
 *  Arjen Markus
 */

#ifndef NOPROT

void MorOdsGetDim( TString fname   , TInt4   *ftype  , TString dim    ,
                   TInt4   *pardep , TInt4   *timdep , TInt4   *locdep ,
                   TInt4   *nodim  , TInt4   *ierror , TString option ) ;
void MorOdsGetPar( TString fname   , TInt4   *ftype  , TString pardef,
                   TInt4   *maxdef , TString parlst  , TString paruni  ,
                   TInt4   *maxlst , TInt4   *nrlst  , TInt4   *partyp ,
                   TInt4   *parcod , TInt4   *ierror , TString option  ) ;
void MorOdsGetLoc( TString fname   , TInt4   *ftype  , TString locdef  ,
                   TInt4   *maxdef , TString loclst  , TInt4   *maxlst ,
                   TInt4   *nrlst  , TInt4   *locnr  , TInt4   *ierror ,
                   TString option                                     ) ;
void MorOdsGetTme( TString fname   , TInt4   *ftype  ,
                   TReal8  *timdef , TInt4   *maxdef , TReal8  *timlst ,
                   TInt4   *maxlst , TInt4   *nrtim  , TInt4   *ierror ,
                   TString option                                    ) ;
void MorOdsGetMat( TString fname   , TInt4   *ftype  , TInt4   *parcod ,
                   TReal8  *tim    , TInt4   *loc    , TReal4  *misval ,
                   TInt4   *maxdim , TReal4  *values , TInt4   *ierror ,
                   TString option                                     ) ;
void MorOdsGetGrd( TString  fname   , TInt4  *ftype  , TInt4   *indloc ,
                   TInt4    *indx   , TInt4  *nocell , TInt4   *igisty ,
                   TInt4    *ierror                                    ) ;
#else

void MorOdsGetDim( ) ;
void MorOdsGetPar( ) ;
void MorOdsGetLoc( ) ;
void MorOdsGetTme( ) ;
void MorOdsGetMat( ) ;
void MorOdsGetGrd( ) ;

#endif
