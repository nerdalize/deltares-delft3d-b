/*
 *  dlwbin.h  -  ODS function prototypes for DELWAQ/DELPAR MAP/HIS files
 *               and GRID files
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *   Eric Verschuur
 */

/*
 *  $Author: Markus $
 *  $Date: 11/04/05 11:58a $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/dlwbin.h,v $
*/
/*
 *
 */
void DlwBinOpen ( char *fname, FILE **fp,TInt4 *ierror ) ;

void DlwBinClose ( FILE *fp,TInt4 *ierror ) ;

void ODSGetLocDlwq (  char   *fname  , TInt4  *ftype  , char   *locdef  ,
                      TInt4  maxdef  , char   *loclst , TInt4  maxlst   ,
                      TInt4  *nrlst  , TInt4  *locnr  , TInt4  *ierror  ) ;

void ODSGetParDlwq (  char   *fname  , TInt4  *ftype  , char   *pardef  ,
                      TInt4  maxdef  , char   *parlst , char   *paruni  ,
                      TInt4  maxlst  , TInt4  *nrlst  , TInt4  *partyp  ,
                      TInt4  *parcod , TInt4  *ierror                  ) ;

void ODSGetTmeDlwq (  char   *fname  , TInt4  *ftype  , TReal8 *timdef ,
                      TInt4  maxdef  , TInt4  pardep  , TReal8 *timlst ,
                      TInt4  maxlst  , TInt4  *nrlst  , TInt4  *ierror ) ;

void ODSGetValDlwm (  char   *fname  , TInt4  *ftype  , char   *locin  ,
                      char   *parin  , TReal8 *timin  , TInt4  maxilo  ,
                      TInt4  maxipa  , TInt4  maxiti  , TReal4 misval  ,
                      char   *loc    , char   *par    , TReal8 *tim    ,
                      TReal4 *values , TInt4  maxolo  , TInt4  maxopa  ,
                      TInt4  maxoti  , TInt4  *nrloc  , TInt4  *nrpar  ,
                      TInt4  *nrtim  , TInt4  *ierror                  ) ;

void ODSGetValDlwh (  char   *fname  , TInt4  *ftype  , char   *locin  ,
                      char   *parin  , TReal8 *timin  , TInt4  maxilo  ,
                      TInt4  maxipa  , TInt4  maxiti  , TReal4 misval  ,
                      char   *loc    , char   *par    , TReal8 *tim    ,
                      TReal4 *values , TInt4  maxolo  , TInt4  maxopa  ,
                      TInt4  maxoti  , TInt4  *nrloc  , TInt4  *nrpar  ,
                      TInt4  *nrtim  , TInt4  *ierror                  ) ;

void ODSGetDimDlwq (  char   *fname  , TInt4  *ftype  , char   *dim    ,
                      TInt4  *pardep , TInt4  *ndim   , TInt4  *ierror ) ;

void ODSGetMatDlwq (  char   *fname  , TInt4  *ftype  , TInt4  *parcod ,
                      TReal8 *tim    , TInt4  *loc    , TReal4 *misval ,
                      TInt4  *maxdim , TReal4 *values , TInt4  *ierror ) ;

void ODSGetGrdDlpr (  char    *fname  , TInt4  *ftype  , TInt4   *indloc ,
                      TInt4   *indx   , TInt4  *nocell , TInt4   *igisty ,
                      TInt4   *ierror                                    ) ;

TVoid ODSGetDimDlwg(  TString fname   , TInt4  *ftype  , TString dim     ,
                      TInt4   pardep  , TInt4  timdep  , TInt4   locdep  ,
                      TInt4   *ndim   , TInt4  *ierror , TString option  ) ;


TVoid ODSGetParDlwg ( TString fname   , TInt4   *ftype  , TString pardef ,
                      TInt4   maxdef  , TString parlst  , TString paruni ,
                      TInt4   maxlst  , TInt4   *nrlst  , TInt4   *partyp,
                      TInt4   *parcod , TInt4   *ierror , TString option ) ;

TVoid ODSGetTmeDlwg ( TString fname   , TInt4   *ftype  , TInt4   timdef ,
                      TInt4   maxdef  , TInt4   pardef  , TInt4   locdep ,
                      TReal8  *timlst , TInt4   maxlst  , TInt4   *nrlst ,
                      TInt4   *ierror , TString option                   ) ;

void ODS_DELWAQ_UNF_telmac(
                      char   *fname  , TInt4  lfname  , TInt4  nfname ,
                      TInt4  ftype   , TInt4  parcod  , TReal8 *tim   ,
                      TInt4  *loc    , TReal4 misval  , TInt4  maxdim ,
                      TReal4 *values , TInt4  *ierror                 ) ;

void ODS_DELWAQ_UNF_lgrid(
                      char   *fname  , TInt4  lfname  , TInt4  nfname  ,
                      TInt4  ftype   , TInt4  *indloc , TInt4  *indx   ,
                      TInt4  *nocell , TInt4  *igisty , TInt4  *ierror ) ;


TVoid ODS_Delwaq_Telemac_Grid(
                      TString fname  , TInt4  ftype   , TInt4  *indloc ,
                      TInt4   *indx  , TInt4  *nocell , TInt4  *igisty ,
                      TInt4   *ierror                                  ) ;

TVoid ODS_Delwaq_Telemac_Coords(
                      TString fname , TInt4   ftype   , TInt4  parcod  ,
                      TInt4   *loc  , TReal8  *tim    , TReal4 misval  ,
                      TInt4   i3gl  , TInt4   maxdim  , TReal4 *data   ,
                      TInt4  *ierror, TString option                   ) ;

/* Some FORTRAN routines - unfortunate perhaps, but much easier!
   - to read the grid files for DELWAQ/DELPAR
*/
#ifndef PC
#if defined(SUN) || defined(__sgi) || defined(linux)
#define GETLGA getlga_
#define GETCCO getcco_
#else
#define GETLGA getlga
#define GETCCO getcco
#endif
#endif

#ifndef NOPROT
#ifndef HPOLD
#ifdef PC
   void FUNTYPE GETLGA ( char   *fname  , TInt4  *itype  , TInt4  *indloc ,
                         TInt4  *indx   , TInt4  *nocell , TInt4  *igisty ,
                         TInt4  *ierror                                   ) ;
   void FUNTYPE GETCCO ( char   *fname  , TInt4  *itype  , TInt4  *ipcode ,
                         TReal8 *time   , TInt4  *indloc , TReal4 *misval ,
                         TInt4  *maxdim , TReal4 *data   , TInt4  *ierror ) ;
#else /* not PC */
   void FUNTYPE GETLGA ( char   *fname  , TInt4  *itype  , TInt4  *indloc ,
                         TInt4  *indx   , TInt4  *nocell , TInt4  *igisty ,
                         TInt4  *ierror , int    len_fname                ) ;
   void FUNTYPE GETCCO ( char   *fname  , TInt4  *itype  , TInt4  *ipcode ,
                         TReal8 *time   , TInt4  *indloc , TReal4 *misval ,
                         TInt4  *maxdim , TReal4 *data   , TInt4  *ierror ,
                         int    len_fname                                 ) ;
#endif /* PC */
#else /* HPOLD */
   void FUNTYPE GETLGA ( char   *fname  , int len_fname  , TInt4  *itype  ,
                         TInt4  *indloc , TInt4  *indx   , TInt4  *nocell ,
                         TInt4  *igisty , TInt4  *ierror                  ) ;
   void FUNTYPE GETCCO ( char   *fname  , int len_fname  , TInt4  *itype  ,
                         TInt4  *ipcode , TReal8 *time   , TInt4  *indloc ,
                         TReal4 *misval , TInt4  *maxdim , TReal4 *data   ,
                         TInt4  *ierror                                   ) ;
#endif /* HPOLD */
#else /* NOPROT */
   void FUNTYPE GETLGA ( )
   void FUNTYPE GETCCO ( )
#endif /* NOPROT */
