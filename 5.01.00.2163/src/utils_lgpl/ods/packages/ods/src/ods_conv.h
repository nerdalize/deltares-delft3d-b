/*  ods_conv.h - Conversion routines for ODS
 *
 *  Copyright (C) 2002 WL | Delft Hydraulics
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the general routines for converting raw data
 *  in the context of ODS
 */

/*
 *  $Author: Markus $
 *  $Date: 5-09-03 14:30 $
 *  $Source$
*/

#ifndef ODS_CONV_H_INCLUDED
#define ODS_CONV_H_INCLUDED

TVoid
ODS_AverageOverDepth(
      TInt4             * ndim,
      TInt4             * lgrid,
      TReal4            * data3d,
      TReal4            * zcrd,
      TReal4              misval,
      TReal4            * data,
      TInt4             * nodata ) ;

TVoid
ODS_AccumulateData(
      TReal4              misval,
      TInt4             * ndim,
      TReal4            * data,
      TReal4            * data_to_add ) ;

TVoid
ODS_AccumulateOverTime(
      TInt4             * ndim,
      TReal4            * data,
      TReal4            * data_to_add,
      TReal4              deltat      ) ;

TVoid
ODS_ConvertVectorComponents(
      TInt4             * ndim,
      TInt4             * lgrid,
      TReal4            * data_u,
      TReal4            * data_v,
      TReal4            * angle,
      TInt4               truegrid,
      TReal4              misval,
      TInt4               result_idx,
      TReal4            * result,
      TInt4             * nodata ) ;

TVoid
   ODS_ConvertKCS(
      TInt4             * dims,
      TInt4             * ibuffer,
      TInt4             * indx,
      TInt4               grid_type ) ;

TVoid
   ODS_ConstructTrivialGrid(
      TInt4             * dims,
      TInt4             * indx   ) ;
/* end ODS_CONV_H_INCLUDED */
#endif
