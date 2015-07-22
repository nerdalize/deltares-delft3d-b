/*
 *  tekasc.h  -  ODS function prototypes for TEKAL ASCII files
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Pleun Koole
 */

/*
 *  $Author: Markus $
 *  $Date: 1-04-03 10:52 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/tekasc.h,v $
*/
/*
 */

TVoid skip_tekal_record(          FILE *iunit ) ;
TVoid skip_tekal_blocname_record( FILE *iunit ) ;
TVoid skip_tekal_comment_records( FILE *iunit ) ;
TVoid skip_tekal_values_records(  FILE *iunit , TInt4 nrows ) ;
TVoid skip_tekal_block(           FILE *iunit ) ;
TVoid extract_idx_name(           FILE *iunit, TInt4 *idx, TString name ) ;

void ODSGetParAnyTekAscHis (
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

void ODSGetDimAnyTekAscHis (
                     char   * fname,
                     TInt4  * ftype,
                     char   * dimtyp,
                     TInt4    pardep,
                     TInt4    timdep,
                     TInt4    locdep,
                     TInt4  * ndim,
                     TInt4  * ierror,
                     char   * option) ;

void ODSGetTmeAnyTekAscHis (
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

void ODSGetLocAnyTekAscHis (
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

void ODSGetMatAnyTekAscHis (
                     char   * fname,
                     TInt4  * ftype,
                     TInt4    parcod,
                     double * tim,
                     TInt4  * loc,
                     float    misval,
                     TInt4    maxdim,
                     float  * xdata,
                     TInt4  * ierror)  ;

void ODSGetGrdAnyTekAscHis( char * fname,
                            TInt4  itype,
                            TInt4 *indloc ,
                            TInt4  *indx ,
                            TInt4  *nocell ,
                            TInt4  *igisty ,
                            TInt4  *ierror );
