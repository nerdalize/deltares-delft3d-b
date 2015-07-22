subroutine rmdel(filnam    ,gdp       )
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
!  $Id: rmdel.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/general/rmdel.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Test existence of file and if delete file, using
!              status='delete' in close statement
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    character(*)    :: filnam      !!  File name of file which should be deleted
!
! Local variables
!
    integer                        :: ierr
    integer                        :: lfil        ! Length of character string FILNAM 
    integer                        :: luntmp      ! Unit number for file 
    integer , external             :: newlun
    logical                        :: ex          ! Logical flag used for INQUIRE state- ment for file EXIST and OPENED 
!
!! executable statements -------------------------------------------------------
!
    call noextspaces(filnam    ,lfil      )
    inquire (file = filnam(:lfil), exist = ex)
    if (ex) then
       inquire (file = filnam(:lfil), opened = ex)
       if (ex) then
          inquire (file = filnam(:lfil), number = luntmp)
       else
          luntmp = newlun(gdp)
          open (luntmp, file = filnam(:lfil), iostat = ierr)
       endif
       close (luntmp, status = 'delete', iostat = ierr)
    endif
end subroutine rmdel
