!zeroi1
      subroutine zeroi1 (x     , kdim  )
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
!  $Id: zero.f 1342 2012-03-23 13:55:32Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/tools_gpl/nesthd1/packages/nesthd1/src/zero.f $
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : zeroi1
! version            : v1.0
! date               : 28 October 1995
! specs last update  :
! programmer         : Lamber Hulsen
!
! function           : initialize 1-dimensional integer array with
!                      default value
! method             :
! limitations        :
! error messages     : none
! subroutines called : none
!
! global variables
!
! name     type      length   description
! ------   -------   ------   -----------
! kdim     integer   1        length of array x
! x        integer   kdim     1-dimensional integer array
!***********************************************************************
!
      integer       kdim  , k
      integer       x     ( kdim  )
!
!-----------------------------------------------------------------------
!---- initialize with default value
!-----------------------------------------------------------------------
!
      do 10 k = 1, kdim
         x (k) = 0
   10 continue
!
!-----------------------------------------------------------------------
!---- return to calling module
!-----------------------------------------------------------------------
!
      return
      end
!zeror1
      subroutine zeror1 (x     , kdim  , defval)
!***********************************************************************
! Deltares                         marine and coastal management
!
! subroutine         : zeror1
! version            : v1.0
! date               : 28 October 1995
! specs last update  :
! programmer         : Lamber Hulsen
!
! function           : initialize 1-dimensional real array with
!                      default value
! method             :
! limitations        :
! error messages     : none
! subroutines called : none
!
! global variables
!
! name     type      length   description
! ------   -------   ------   -----------
! defval   real      1        default value to initialize x array
! kdim     integer   1        length of array x
! x        real      kdim     1-dimensional real array
!***********************************************************************
!
      integer       kdim  , k
!
      real          x     ( kdim  )
!
      real          defval
!

!
!-----------------------------------------------------------------------
!---- initialize with default value
!-----------------------------------------------------------------------
!
      do 10 k = 1, kdim
         x (k) = defval
   10 continue
!
!-----------------------------------------------------------------------
!---- return to calling module
!-----------------------------------------------------------------------
!
      return
      end
