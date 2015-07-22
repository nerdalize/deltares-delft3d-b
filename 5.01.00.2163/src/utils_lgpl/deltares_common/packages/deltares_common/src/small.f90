subroutine small(string    ,lenstr    )
!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  $Id: small.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/deltares_common/packages/deltares_common/src/small.f90 $
!!--description-----------------------------------------------------------------
!
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer      , intent(in) :: lenstr
    character(*)              :: string
!
!
! Local variables
!
    integer                        :: i
    integer                        :: j
    integer                        :: newlen
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    ! External program name  : SMALL.F
    ! Programmer             : Cor van der Schelde
    ! Function description   : Altering the uppercase characters (in a string)
    !                          to lower case characters (for UNIX)
    ! Called by              : Various routines
    !                    Date: 01-08-2002
    !
    ! description external variables/parameters
    ! ----------------------------------------
    ! name    type      length   description
    ! ------  --------  ------   ------------
    ! lenstr  i*4       1        lengtg of the string
    ! string  ch*(*)    1        string
    !
    !
    ! description local variables
    ! ----------------------------
    ! name    type      length   description
    ! ------  --------  ------   ------------
    ! i       i*4       1        loop variable
    ! j       i*4       1        hulp variable
    ! newlen  i*4       1        actual string length
    !
    !
    !
    ! common blocks          : none
    !
    ! subroutines            : ichar  (intrinsic function)
    !                          char   (intrinsic function)
    !
    ! lun                    : none
    !
    !
    !  declarations
    !
    !
    newlen = min(lenstr, len(string))
    do i = 1, newlen
       j = ichar(string(i:i))
       if ((j>64) .and. (j<91)) then
          j = j + 32
          string(i:i) = char(j)
       endif
    enddo
end subroutine small
