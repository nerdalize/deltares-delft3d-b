/* Prototypes for PHAROS ODS routines - dummy for the moment */

TVoid ODSGetDimPharMap( TString fname, TInt4   *ftype, TString dim, 
                        TInt4   pardep, TInt4   timdep, TInt4   locdep, 
                        TInt4   *ndim, TInt4   *ierror, TString option) ;

TVoid ODSGetDimPharAmp( TString fname, TInt4   *ftype, TString dim, 
                        TInt4   pardep, TInt4   timdep, TInt4   locdep, 
                        TInt4   *ndim, TInt4   *ierror, TString option) ;

TVoid ODSGetParPharMap ( TString fname, TInt4   *ftype, TString pardef,
                         TInt4   maxdef, TString parlst, TString paruni,
                         TInt4   maxlst, TInt4   *nrlst, TInt4   *partyp,
                         TInt4   *parcod, TInt4   *ierror) ;

TVoid ODSGetParPharAmp ( TString fname, TInt4   *ftype, TString pardef,
                         TInt4   maxdef, TString parlst, TString paruni,
                         TInt4   maxlst, TInt4   *nrlst, TInt4   *partyp,
                         TInt4   *parcod, TInt4   *ierror) ;

TVoid ODSGetLocPharAmp ( TString fname, TInt4   *ftype, TString locdef,
                         TInt4   maxdef, TInt4 pardep, TInt4 timdep,
                         TString loclst, TInt4 * loctyp, TInt4 * locnr,
			 TInt4 maxlst, TInt4   *nrlst,TInt4   *ierror,
			 TString option) ;

TVoid ODSGetTmePharAmp ( TString fname, TInt4   *ftype, TReal8 * timdef,
                         TInt4   maxdef, TInt4   pardef, TInt4   locdep,
                         TReal8  *timlst, TInt4   maxlst, TInt4   *nrlst,
                         TInt4   *ierror, TString option) ;

TVoid ODSGetMatPharMap(  TString fname, TInt4   itype, TInt4   parcod ,
                         TReal8  *tim , TInt4   *loc , TReal4  misval ,
                         TInt4   maxdim , TReal4  *data , TInt4   *ierror ) ;

TVoid ODSGetGrdPharMap(  TString fname, TInt4   itype, TInt4   *indloc ,
                         TInt4   *indx , TInt4   *nocell , TInt4   *igisty ,
                         TInt4   *ierror ) ;

TVoid ODSGetGrdPharAmp(  TString fname, TInt4   itype, TInt4   *indloc ,
                         TInt4   *indx , TInt4   *nocell , TInt4   *igisty ,
                         TInt4   *ierror ) ;
