function newunit( )
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
!  $Id: newunit.f90 1304 2012-03-07 08:53:50Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/deltares_common/packages/deltares_common/src/newunit.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: This routine gets an unused logical unit specifier.
!              It returns an error if it didn't succeed. The name
!              of this function has been changed from newlun_nogdp
!              to newunit to match the newunit keyword supported
!              by Fortran 2008 in the OPEN call.
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
! Global variables
!
    integer         :: newunit
                                   !!  Integer function used to attach
                                   !!  a new unit number to a var. LUN...
!
!
! Local variables
!
    integer                        :: iexit                ! Exit return value 
    integer                        :: lunit                ! Help var. 
    logical                        :: opened               ! Logical flag = TRUE if the test file is already opened 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    call psemlun
    lunit = 31
    opened = .true.
    !
    !-----get unit specifier
    !
    !-->
    !
   10 continue
    if (opened .and. lunit<999) then
       lunit = lunit + 1
       inquire (unit = lunit, opened = opened)
       goto 10
    !
    ! <--
    !
    endif
    !
    !-----test if unit number is available
    !
    if (opened) then
       newunit = 0
       write (*, *) ' *** FATAL ERROR - New unit number not available'
       write (*, *) ' Abnormal end'
       !
       !--------stop routine for DELFT3D
       !
       iexit = 2
    else
       newunit = lunit
    endif
    call vsemlun
end function newunit
