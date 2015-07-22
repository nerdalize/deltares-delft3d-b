!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: procs.inc 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd2/packages/nesthd2/include/procs.inc $
!-------------------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    M C M
!
!             Module: INCLUDE FILE PROCS.INC
!               Date: 03-04-2002
!         Programmer: H.H. Leepel, A.J. Mourits, L.Merckelbach
!         CVS header
!            $Author: Mooiman $
!              $Date: 16-04-04 13:23 $
!            $Source: /u/trisula/cvsroot/trisula/include/procs.inc,v $
!          $Revision: 1 $
!-----------------------------------------------------------------------
!   Common variables:
!   -----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! CONST       L*4                  Flag = TRUE if constituent has been
!                                  specified
! DIOCOM      L*4                  Flag = TRUE if communication through
!                                  the DelftIO lib is required: fluid
!                                  mud calculation, for both water layer
!                                  and mud layer
! DROGUE      L*4                  Flag = TRUE if drogues have been spe-
!                                  cified
! MUDLAY      L*4                  Flag = TRUE for mud layer
!                                  (mud input file is defined and found)
! HTUR2D      L*4                  Flag = TRUE if HLES sub grid
!                                  turbulence model has been specified
! IWEFLG      L*4                  Flag = TRUE if interal wave input
!                                  file is defined and found
! ONLINE      L*4                  Flag = TRUE if on-line visualisation
!                                  has been activated
! RTCACT      L*4                  Flag = TRUE if RTC has been activated
!                                  (initialisation has been done)
! RTCMOD      L*4                  Flag = TRUE if flow is run together
!                                  with RTC
! SALIN       L*4                  Flag = TRUE if salinity has been
!                                  specified
! SECFLO      L*4                  Flag = TRUE if secondary flow has been
!                                  specified
! SEDIM       L*4                  Flag = TRUE if sediment has been
!                                  specified
! STRUCT      L*4                  Flag = TRUE for structures read from
!                                  file(s)
! TEMP        L*4                  Flag = TRUE if temperature/heat has
!                                  been specified
! THREED      L*4                  Flag = TRUE if KMAX > 1
! V_RIVR      L*4                  Flag = TRUE if river version (2D) has
!                                  been specified
! WAVE        L*4                  Flag = TRUE if wave process has been
!                                  specified
! WIND        L*4                  Flag =TRUE if wind force is active
!-----------------------------------------------------------------------
!
!  declarations
!
      logical         wind  ,salin ,temp  ,const ,
     *                drogue,wave  ,threed,secflo,
     *                iweflg,struct,sedim ,online,
     *                v_rivr,htur2d,diocom,mudlay,rtcact,rtcmod
!
      common /procs / wind  ,salin ,temp  ,const ,
     *                drogue,wave  ,threed,secflo,
     *                iweflg,struct,sedim ,online,
     *                v_rivr,htur2d,diocom,mudlay,rtcact,rtcmod
