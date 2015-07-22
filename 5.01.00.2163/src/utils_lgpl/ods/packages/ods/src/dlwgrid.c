/*  Copyright (C) 1994 Delft Hydraulics
    Copyright (C) 1999 Delft Hydraulics

    See c2for.txt for details
 */

#include "dlwgrid.h"


TVoid ODS_DELWAQ_UNF_lgrid(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4    * indloc,
    TInt4    * indx,
    TInt4    * nocell,
    TInt4    * igisty,
    TInt4    * ierror
    )
{

    ODS_DELWAQ_UNF_LGA(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      indloc,
      indx,
      nocell,
      igisty,
      ierror
#if ! defined(IN_BETWEEN)
     ,lfname
#endif
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


TVoid ODS_DELWAQ_UNF_telmac(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4      ipcode,
    TReal8   * time,
    TInt4    * indloc,
    TReal4     valmis,
    TInt4      maxdim,
    TReal4   * data,
    TInt4    * ierror
    )
{

    ODS_DELWAQ_UNF_CCO(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
     &ipcode,
      time,
      indloc,
     &valmis,
     &maxdim,
      data,
      ierror
#if ! defined(IN_BETWEEN)
     ,lfname
#endif
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


