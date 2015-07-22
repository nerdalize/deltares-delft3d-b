/*  ods_spcf.h - File-specific conversion routines for ODS
 *
 *  Copyright (C) 2002 WL | Delft Hydraulics
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the specific routines for converting raw data
 *  in the context of ODS
 */

/*
 *  $Author: Markus $
 *  $Date: 6/09/06 16:13 $
 *  $Source$
*/

#ifndef ODS_SPCF_H_INCLUDED
#define ODS_SPCF_H_INCLUDED

TVoid
ODS_DetermineDepthInCellCentre(
      NefisFileInfoPtr    file_info,
      TInt4             * ndim,
      TInt4             * lgrid,
      TReal4            * depth,
      TReal4              misval,
      TReal4            * dps    ) ;

TVoid
ODS_DetermineZcoordinate(
      NefisFileInfoPtr    file_info,
      ParameterInfoPtr    wl_info,
      ParameterInfoPtr    depth_info,
      TInt4               cell_index[5][3],
      TInt4             * loc,
      TInt4             * ndim,
      TInt4             * lgrid,
      TReal4              misval,
      TReal4           ** zcrd_ptr     ) ;

TVoid
ODS_GetGrdIndices(
      TString             filename,
      TInt4             * ftype,
      TInt4               loc_type,
      TInt4             * ndim,
      TInt4            ** indx,
      TInt4             * truegrid ) ;

TVoid
ODS_EncodeDryWetInformation(
      TInt4             * ndim,
      TInt4             * lgrid,
      TInt4             * udam,
      TInt4             * vdam,
      TReal4            * data,
      TInt4               ordering ) ;

TInt4
ODS_HeuristicCheckAccept(
      NefisFileInfoPtr    file_info,
      ParameterInfoPtr    param,
      TInt4               idx ) ;

/* end ODS_SPCF_H_INCLUDED */
#endif
