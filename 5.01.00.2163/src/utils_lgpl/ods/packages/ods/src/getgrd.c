/*
 *  getgrd.c  -  ODS Get index array for grids and other geographical
 *               datatypes
 *
 *  copied from getdim.c AAM
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Arjen Markus
 */


/*   Date:       15 Nov 1994                                          */
/*   Time:       10:00                                                */
/*   Program:    GETGRD.C                                             */
/*   Version:    1.00                                                 */
/*   Programmer: Arjen Markus s                                       */
/*   (c) Copyright 1993 Delft Hydraulics                              */
/*   Previous version(s):                                             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     GetGrid                                              */
/*   Function:                                                        */
/*   Comment:    1.00: first version TRISULA, DELWAQ/DELPAR           */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#include <string.h>
#include <stdio.h>

#ifndef NOPROT

#include "odsmodel.h"

TInt4 iftype ( char *fname) ;

#endif

/* the function to get index array from 'any' file:   */

void DllExport getgrd ( char *fname,  TInt4 *itype,  TInt4 *indloc,
                        TInt4 *indx,  TInt4 *nocell, TInt4 *igisty,
                        TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*        fname   3          I   Full filename, including extension      */
/*        ierror  -          O   Errorcode, see errors.inc for definitio */
/*        igisty  -          O   Type of geographical data (see ods.h)   */
/*        indloc  -          I   location index array (3*3)              */
/*        indx    -          O   array with index information            */
/*        itype   -         I/O  Filetype, see types.inc for definitions */
/*        len_..  -          I   UNIX only: length of CHARACTER variable */
/*        nocell  -          O   Number of cells (model data) in grid etc*/
/*                                                                       */
/*************************************************************************/

{
   int i, inum , length ;
   char *fn;

   *ierror = IEOK;


   /* strip trailing blanks and add terminating 0 to all strings,
      just to make sure:
   */

   for (inum = 0 ; inum < ODS_FNAME_DIM ; inum++)
   {
       fn = &fname[inum*ODS_FILNAMLEN] ;

       fn[ODS_FILNAMLEN-1] = '\0' ;
       length = strlen( fn ) ;
       for ( i = length-1 ; i >= 0 ; i -- )
       {
          if ( fn[i] == ' ')
          {
             fn[i] = '\0' ;
          }
          else
          {
             break ;
          }
       }
   }

   /*  Get filetype, if we don't know, first check extension:          */

   if (*itype == ITUNDE)
   {
      *itype = iftype ( fname) ;
   }

   /*  Now proceed:                                                    */

   switch ( *itype)
      {
      case ITUNDE :

         /* Unable to determine filetype                               */

         *ierror = IEUNDE ;
         break ;

      case ODS_DELWAQ_GRID_UNF  :

         /* FORTRAN Unformatted filetype                               */

         ODS_DELWAQ_UNF_lgrid( fname , 256 , 3 , *itype , indloc , indx ,
                               nocell , igisty , ierror ) ;

         break ;

      case ODS_DELWAQ_TELEMAC   :

         /* TELEMAC grids                                              */

         ODS_Delwaq_Telemac_Grid( fname , *itype , indloc , indx ,
                               nocell , igisty , ierror ) ;
         break ;

      case ODS_DELPAR_PLO_BIN   :

         /* FORTRAN Binary filetype                               */

         ODSGetGrdDlpr( fname , itype , indloc , indx ,
                        nocell , igisty , ierror ) ;
         break ;

      case ODS_DELPAR_PLO_NEFIS :

         /* NEFIS filetype                                        */

         ODSGetGrdDlprNef( fname , itype , indloc , indx ,
                           nocell , igisty , ierror ) ;

         break ;

      case ODS_TRISULA_MAP_NEFIS :
         GNF_GetGrd( fname, itype, indloc, indx, nocell, igisty, ierror ) ;

         break ;

      case ODS_SAMPLES_2D    :
      case ODS_SAMPLES_TABLE :

         /* Samples file (2D or table)                                 */

         SamplesGetGrd( fname , itype , indloc , indx ,
                        nocell , igisty , ierror ) ;

         break ;

      case ODS_MORSYS_MAP_NEFIS :
         GNF_GetGrd( fname, itype, indloc, indx, nocell, igisty, ierror ) ;

         break ;

      case ODS_MORSYS_TRAM_NEFIS :
         GNF_GetGrd( fname, itype, indloc, indx, nocell, igisty, ierror ) ;

         break ;

      case ODS_MORSYS_HWBOD :
         ODSGetGrdMorNefCom (
                 fname, (TInt4 *) itype, (TInt4 *) indloc, (TInt4 *) indx,
                 (TInt4 *) nocell, (TInt4 *) igisty, (TInt4 *) ierror ) ;
         break ;

      case ODS_MORSYS_HWGXY :
         ODSGetGrdMorNefCom (
                 fname, (TInt4 *) itype, (TInt4 *) indloc, (TInt4 *) indx,
                 (TInt4 *) nocell, (TInt4 *) igisty, (TInt4 *) ierror ) ;
         break ;

      case ODS_MORSYS_SWAN_NEFIS :
         GNF_GetGrd( fname, itype, indloc, indx, nocell, igisty, ierror ) ;

         break ;

      case ODS_MORSYS_BAGR_NEFIS :
         MorOdsGetGrd( fname , itype , indloc , indx ,
                       nocell , igisty , ierror ) ;

         break ;


      case ODS_PHIDIAS_MAP :
      case ODS_PHIDIAS_SPECTRAL :
        ODSGetGrdPhidias (
                fname, (TInt4 *) itype, (TInt4 *) indloc, (TInt4 *) indx,
                (TInt4 *) nocell, (TInt4 *) igisty, (TInt4 *) ierror ) ;
         break ;

      case ODS_SHYFEM_GRID :
         ODSGetGrdShyf(
                 fname, (TInt4) itype, (TInt4 *) indloc, (TInt4 *) indx,
                 (TInt4 *) nocell, (TInt4 *) igisty, (TInt4 *) ierror ) ;
         break ;

      case ODS_PHAROS_MAP_NEFIS :

         /* Pharos finite element grid from nefis admin file           */

         ODSGetGrdPharMap( fname, (TInt4) itype, (TInt4 *) indloc,
                           (TInt4 *) indx, (TInt4 *) nocell, (TInt4 *) igisty,
                           (TInt4 *) ierror ) ;

         break ;

      case ODS_PHAROS_AMP_NEFIS :
         /* Pharos finite element grid from nefis admin file           */

         ODSGetGrdPharAmp( fname, (TInt4) itype, (TInt4 *) indloc,
                           (TInt4 *) indx, (TInt4 *) nocell, (TInt4 *) igisty,
                           (TInt4 *) ierror ) ;

         break ;
      case ODS_ANY_TEKAL_ASCII_1D :
      case ODS_ANY_TEKAL_ASCII_1DE :

         /* TEKAL ASCII 1D files                                       */

         ODSGetGrdAnyTekAscHis( fname, (TInt4) itype, (TInt4 *) indloc,
                                (TInt4 *) indx, (TInt4 *) nocell, (TInt4 *)
                                igisty, (TInt4 *) ierror ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_2D :
      case ODS_ANY_TEKAL_ASCII_VEC :

         /* TEKAL ASCII 2D files                                       */

         ODSGetGrdAnyTekAscMap( fname, (TInt4) itype, (TInt4 *) indloc,
                                (TInt4 *) indx, (TInt4 *) nocell, (TInt4 *)
                                igisty, (TInt4 *) ierror ) ;

         break ;

      case ODS_GEOGR_BNA:
      case ODS_GEOGR_DXF:
      case ODS_GEOGR_TEKAL:

         /* BNA, DXF and TEKAL landboundary files                     */
         ODS_BNA_getgrd( fname, itype, indloc, indx, nocell, igisty,
                         ierror                                    ) ;
         break ;

      default :

         /* Filetype not implemented:                                  */

         *ierror = IEUNKN;
      }
}

