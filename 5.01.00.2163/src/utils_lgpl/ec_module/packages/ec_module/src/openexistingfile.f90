function openexistingfile(minp, filename) result(success)
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
!  $Id: openexistingfile.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/openexistingfile.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
!
! adri.mourits@deltares.nl
!
!!--declarations----------------------------------------------------------------
    use ec_message
    implicit none
!
! Global variables
!
    integer         :: minp
    logical         :: success
    character(*)    :: filename
!
! Local variables
!
    integer :: i
    logical :: unitused
!
!! executable statements -------------------------------------------------------
!
    if (len_trim(filename) == 0) then
       call setECMessage("ERROR: openExistingFile: Name is empty")
       success = .false.
       return
    endif
    inquire (file = trim(filename), exist = success)
    if (.not. success) then
       call setECMessage("ERROR: openExistingFile: File does not exist:", trim(filename))
       success = .false.
       return
    endif
    do i = 1, 100
       inquire (unit = i, opened = unitused) 
       if (.not. unitused) exit
    enddo
    if (unitused) then
       call setECMessage("ERROR: openExistingFile: No free unit number available")
       success = .false.
       return
    endif
    minp = i
    open (minp, file = trim(filename), action = 'READ')
    success = .true.
end function openexistingfile
