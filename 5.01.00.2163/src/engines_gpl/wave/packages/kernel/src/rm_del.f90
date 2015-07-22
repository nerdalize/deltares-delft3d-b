subroutine rm_del( filnamin )
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
!  $Id: rm_del.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/kernel/src/rm_del.f90 $
!!--description-----------------------------------------------------------------
! Test existence of file and if delete file, using
! status='delete' in close statement
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    character(*), intent(in) :: filnamin
!
! Local variables
!
    integer           :: istat
    integer           :: luntmp  ! Unit number for file
    integer, external :: new_lun
    logical           :: ex      ! Logical flag used for INQUIRE state-
    character(300)    :: filnam
!
!! executable statements -------------------------------------------------------
!
    filnam = adjustl(filnamin)
    inquire (file = trim(filnam), exist = ex, iostat =  istat)
    if (ex) then
       inquire (file = trim(filnam), opened = ex, iostat =  istat)
       if (ex) then
          inquire (file = trim(filnam), number = luntmp, iostat =  istat)
       else
          luntmp = new_lun()
          open (luntmp, file = trim(filnam), iostat =  istat)
       endif
       close (luntmp, status = 'delete', iostat =  istat)
    endif
end subroutine rm_del
