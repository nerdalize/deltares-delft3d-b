      subroutine gtpoint(x1     ,y1     ,icom1 ,
     *                   x2     ,y2     ,icom2 ,
     *                   typbnd ,nambnd ,
     *                   angle  ,mcbsp  ,ncbsp ,
     *                   xbnd   ,ybnd   ,mcnes ,ncnes,
     *                   weight ,
     *                   ipx    ,ipy    ,itotpx,itotpy)                      
     *       
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
!  $Id: gtpoint.f 1982 2012-11-16 13:51:04Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd1/packages/nesthd1/src/gtpoint.f $
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : gtpoint
! version            : v1.0
! date               : Oct 2000
! programmer         : Antoon Koster
! function           : get FMM pointers
! error messages     :
! subroutines called : -
!
!***********************************************************************
!
      EXTERNAL GTIPNT, GTRPNT, GTCPNT
      integer  GTIPNT, GTRPNT, GTCPNT
!
!     FMM pointers
!
      integer x1    ,y1    ,icom1
      integer x2    ,y2    ,icom2
      integer typbnd,nambnd
      integer angle ,mcbsp ,ncbsp 
      integer xbnd  ,ybnd  ,mcnes ,ncnes
      integer weight
      integer ipx   ,ipy
      integer itotpx,itotpy

      x1      = GTRPNT('x1')
      y1      = GTRPNT('y1')
      icom1   = GTIPNT('icom1')

      x2      = GTRPNT('x2')
      y2      = GTRPNT('y2')
      icom2   = GTIPNT('icom2')

      typbnd  = GTCPNT('typbnd')
      nambnd  = GTCPNT('nambnd')

      angle   = GTRPNT('angle')
      mcbsp   = GTIPNT('mcbsp')
      ncbsp   = GTIPNT('ncbsp')
      xbnd    = GTRPNT('xbnd')
      ybnd    = GTRPNT('ybnd')
      mcnes   = GTIPNT('mcnes')
      ncnes   = GTIPNT('ncnes')
      weight  = GTRPNT('weight')

      ipx     = GTIPNT('ipx')
      ipy     = GTIPNT('ipy')
      itotpx  = GTIPNT('itotpx')
      itotpy  = GTIPNT('itotpy')

      return
      end
