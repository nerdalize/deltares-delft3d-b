/*
 *  tekasc2d.h  -  ODS function prototypes for TEKAL ASCII files
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 */

TReal8 Julian_from_date_time( TInt4 date , TInt4 time ) ;

void ODSGetParAnyTekAscMap (
                     char   * fname,
                     TInt4  * ftype,
                     char   * pardef,
                     TInt4    maxdef,
                     TInt4    timdep,
                     TInt4    locdep,
                     TInt4    maxlst,
                     TInt4    lang,
                     char   * parlst,
                     char   * paruni,
                     TInt4  * partyp,
                     TInt4  * parcod,
                     TInt4  * nrlst,
                     TInt4  * ierror,
                     char   * option) ;

void ODSGetDimAnyTekAscMap (
                     char   * fname,
                     TInt4  * ftype,
                     char   * dimtyp,
                     TInt4    pardep,
                     TInt4    timdep,
                     TInt4    locdep,
                     TInt4  * ndim,
                     TInt4  * ierror,
                     char   * option) ;

void ODSGetTmeAnyTekAscMap (
                     char   * fname,
                     TInt4  * ftype,
                     double * timdef,
                     TInt4    maxdef,
                     TInt4    pardep,
                     TInt4    locdep,
                     double * timlst,
                     TInt4    maxlst,
                     TInt4  * nrlst,
                     TInt4  * ierror,
                     char   * option) ;

void ODSGetLocAnyTekAscMap (
                     char   * fname,
                     TInt4  * ftype,
                     char   * locdef,
                     TInt4    maxdef,
                     TInt4    pardep,
                     TInt4    timdep,
                     char   * loclst,
                     TInt4  * loctyp,
                     TInt4  * locnr,
                     TInt4    maxlst,
                     TInt4  * nrlst,
                     TInt4  * ierror,
                     char   * option) ;

void ODSGetMatAnyTekAscMap (
                     char   * fname,
                     TInt4  * ftype,
                     TInt4    parcod,
                     double * tim,
                     TInt4  * loc,
                     float    misval,
                     TInt4    maxdim,
                     float  * xdata,
                     TInt4  * ierror)  ;

void ODSGetGrdAnyTekAscMap( char * fname,
		            TInt4  itype,
 		            TInt4 *indloc ,
		            TInt4  *indx ,
		            TInt4  *nocell ,
		            TInt4  *igisty ,
		            TInt4  *ierror );
