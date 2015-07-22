/*
 *  dlwnef.h  -  ODS: DELWAQ/DELPAR NEFIS files (prototypes)
 *
 *  Copyright (C) 1996 Delft Hydraulics
 *
 *  Derived from code by Eric Verschuur
 *  Arjen Markus
 */
/*
 *  $Author: Markus $
 *  $Date: 1-04-03 10:52 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/dlwnef.h,v $
*/
/*
 *
 */


void OpenNefisDefDat (   char  *fname  , BInt4 *datfds  , BInt4 *deffds  ,
                         TInt4 *ierror                                   ) ;

void CloseNefisDefDat (  char *fname   , BInt4 *datfds  , BInt4 *deffds  ,
                         TInt4 *ierror                                   ) ;

void DlwOpenNefisFiles ( char  *fname  , TInt4 ftype    , BInt4 *datfds  ,
                         BInt4 *deffds , char  *grp_par , char  *grp_res ,
                         TInt4 *ierror                                   ) ;

void CloseNefisFiles (   char  *fname  , BInt4 *datfds  , BInt4 *deffds  ,
                         TInt4 *ierror                                   ) ;

void DlwGetNefisTme (    char  *fname  , TInt4  *ftype  , TReal8 *timdef ,
                         TInt4 *maxdef , TReal8 *timlst , TInt4  *maxlst ,
                         TInt4 *nrtim  , TInt4  *ierror , char   *option ) ;

void DlwGetNefisMat (    char   *fname , TInt4  *ftype  , TInt4  *parcod ,
                         TReal8 *tim   , TInt4  *loc    , TReal4 *misval ,
                         TInt4  *maxdim, TReal4 *values , TInt4  *ierror ,
                         char   *option                                  ) ;

void DlwGetNefisLoc (    char  *fname  , TInt4  *ftype  , char   *pardef ,
                         TInt4 *maxdef , char   *parlst , TInt4  *maxlst ,
                         TInt4 *nrlst  , TInt4  *locnr  , TInt4  *ierror ,
                         char  *option                                   ) ;

void DlwGetNefisPar (    char  *fname  , TInt4  *ftype  , char   *pardef ,
                         TInt4 *maxdef , char   *parlst , char   *paruni ,
                         TInt4 *maxlst , TInt4  *nrlst  , TInt4  *partyp ,
                         TInt4 *parcod , TInt4  *ierror , char   *option ) ;

void DlwGetNefisDim (    char  *fname  , TInt4  *ftype  , char   *dim    ,
                         TInt4 *pardep , TInt4  *timdep , TInt4  *locdep ,
                         TInt4 *ndim   , TInt4  *ierror , char   *option ) ;

void ODSGetGrdDlprNef( char    *fname,
                       TInt4   *ftype,
                       TInt4   *indloc,
                       TInt4   *indx,
                       TInt4   *nocell,
                       TInt4   *igisty,
                       TInt4   *ierror) ;

