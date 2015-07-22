/*
 *  m3hbuf.h  -  ODS function prototypes for TRISULA NEFIS files
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *   Pleun Koole
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:51p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/m3hbuf.h,v $
*/
/*
 */

#include "portable.h"

#ifndef M3HBUF_H_INCLUDED
#define M3HBUF_H_INCLUDED

void ODS_TRISULA_NEFIS_hismat_buffer(
    char *   fname,
    TInt4 *  parcod,
    float ** zbuffs,
    TInt4 *  ierror );

#endif  /* M3HBUF_H_INCLUDED */

#ifndef M3MBUF_H_INCLUDED
#define M3MBUF_H_INCLUDED

void ODS_TRISULA_NEFIS_mapmat_buffer(
    char *   fname,
    TInt4 *  parcod,
    TInt4 ** ibuffs,
    float ** zbuffs,
    TInt4 *  ierror );

#endif  /* M3MBUF_H_INCLUDED */

void ODSGetGrdTriNefMap (
    char *   fname,
    TInt4 *  ftype,
    TInt4 *  indloc,
    TInt4 *  indx,
    TInt4 *  nocell,
    TInt4 *  igisty,
    TInt4 *  ierror );

TVoid ODSGetGrdMorNefCom (
                      TString fname,
                      TInt4   *ftype,
                      TInt4   *indloc,
                      TInt4   *indx,
                      TInt4   *nocell,
                      TInt4   *igisty,
                      TInt4   *ierror);

TVoid ODSGetGrdPhidias (
                      TString fname,
                      TInt4   *ftype,
                      TInt4   *indloc,
                      TInt4   *indx,
                      TInt4   *nocell,
                      TInt4   *igisty,
                      TInt4   *ierror);

void ODS_MORSYS_NEFIS_commat_buffer(
                      char *   fname,
                      TInt4 *  parcod,
                      TInt4    itype,
                      TInt4 ** ibuffs,
                      float ** zbuffs,
                      TInt4 *  ierror );

void ODS_PHIDIAS_commat_buffer(
                      char *   fname,
                      TInt4 *  parcod,
                      TInt4    itype,
                      TInt4 ** ibuffs,
                      float ** zbuffs,
                      TInt4 *  ierror );


