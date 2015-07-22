/*  Copyright (C) 1994 Delft Hydraulics
 *  Copyright (C) 1999 Delft Hydraulics
 *
 *  See c2for.txt for details
 */

#ifndef C2F_H_morcom_INCLUDED
#   define C2F_H_morcom_INCLUDED

#include "portable.h"

#ifndef FORTRAN_TRUE
#define FORTRAN_TRUE  1
#define FORTRAN_FALSE 0
#endif

/* Define the external names for the linker
*/
#define ODS_MOR_NEF_COM_DIM   ods_mor_nef_com_dim
#define ODS_MOR_NEF_COM_TME   ods_mor_nef_com_tme
#define ODS_MOR_NEF_COM_PAR   ods_mor_nef_com_par
#define ODS_MOR_NEF_COM_MAT   ods_mor_nef_com_mat

#if defined(USE_WINNT) || defined(WINNT)
#undef  ODS_MOR_NEF_COM_DIM
#undef  ODS_MOR_NEF_COM_TME
#undef  ODS_MOR_NEF_COM_PAR
#undef  ODS_MOR_NEF_COM_MAT
#endif

/* We need to check for gcc/g77!
*/
#if defined(USE_SUNOS) || defined(USE_IRIX) || defined(USE_LINUX)
#undef  ODS_MOR_NEF_COM_DIM
#undef  ODS_MOR_NEF_COM_TME
#undef  ODS_MOR_NEF_COM_PAR
#undef  ODS_MOR_NEF_COM_MAT
#define ODS_MOR_NEF_COM_DIM   ods_mor_nef_com_dim_
#define ODS_MOR_NEF_COM_TME   ods_mor_nef_com_tme_
#define ODS_MOR_NEF_COM_PAR   ods_mor_nef_com_par_
#define ODS_MOR_NEF_COM_MAT   ods_mor_nef_com_mat_
#endif

extern TVoid ODS_MORSYS_NEFIS_COMDIM(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    dimtyp,
    TInt4      pardep,
    TInt4      timdep,
    TInt4      locdep,
    TInt4    * ndim,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL ODS_MOR_NEF_COM_DIM(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TString    dimtyp,
#if defined(IN_BETWEEN)
    TInt4      l_dimtyp,
#endif
    TInt4    * pardep,
    TInt4    * timdep,
    TInt4    * locdep,
    TInt4    * ndim,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
    TInt4      l_dimtyp,
#endif
    TInt4      l_option
    );


extern TVoid ODS_MORSYS_NEFIS_COMTME(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TReal8   * timdef,
    TInt4      maxdef,
    TInt4      pardep,
    TInt4      locdep,
    TInt4      maxlst,
    TReal8   * timlst,
    TInt4    * timtyp,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL ODS_MOR_NEF_COM_TME(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TReal8   * timdef,
    TInt4    * maxdef,
    TInt4    * pardep,
    TInt4    * locdep,
    TInt4    * maxlst,
    TReal8   * timlst,
    TInt4    * timtyp,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4      l_option
    );


extern TVoid ODS_MORSYS_NEFIS_COMPAR(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    pardef,
    TInt4      lpardef,
    TInt4      npardef,
    TInt4      maxdef,
    TInt4      timdep,
    TInt4      locdep,
    TInt4      maxlst,
    TInt4      lang,
    TString    parlst,
    TInt4      lparlst,
    TInt4      nparlst,
    TString    paruni,
    TInt4      lparuni,
    TInt4      nparuni,
    TInt4    * partyp,
    TInt4    * parcod,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    );


extern TVoid FOR_CALL ODS_MOR_NEF_COM_PAR(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TString    pardef,
#if defined(IN_BETWEEN)
    TInt4      lpardef,
#endif
    TInt4    * maxdef,
    TInt4    * timdep,
    TInt4    * locdep,
    TInt4    * maxlst,
    TInt4    * lang,
    TString    parlst,
#if defined(IN_BETWEEN)
    TInt4      lparlst,
#endif
    TString    paruni,
#if defined(IN_BETWEEN)
    TInt4      lparuni,
#endif
    TInt4    * partyp,
    TInt4    * parcod,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option,
#if ! defined(IN_BETWEEN)
    TInt4      lfname,
    TInt4      lpardef,
    TInt4      lparlst,
    TInt4      lparuni,
#endif
    TInt4      l_option
    );


extern TVoid ODS_MORSYS_NEFIS_COMMAT(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4      parcod,
    TInt4    * loc,
    TReal8   * tim,
    TReal4     misval,
    TInt4      i3gl,
    TInt4      maxdim,
    TReal4   * xdata,
    TInt4    * ierror,
    TString    option,
    TInt4    * ibuffs,
    TReal4   * rbuffs
    );


extern TVoid FOR_CALL ODS_MOR_NEF_COM_MAT(
    TString    fname,
#if defined(IN_BETWEEN)
    TInt4      lfname,
#endif
    TInt4    * itype,
    TInt4    * parcod,
    TInt4    * loc,
    TReal8   * tim,
    TReal4   * misval,
    TInt4    * i3gl,
    TInt4    * maxdim,
    TReal4   * xdata,
    TInt4    * ierror,
    TString    option,
#if defined(IN_BETWEEN)
    TInt4      l_option,
#endif
    TInt4    * ibuffs,
    TReal4   * rbuffs
#if ! defined(IN_BETWEEN)
    ,TInt4     lfname
    ,TInt4     l_option
#endif
    );


#endif  /* C2F_H_morcom_INCLUDED */
