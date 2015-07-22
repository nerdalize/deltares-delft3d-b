subroutine delnef(filnam, gdp)
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
!  $Id: delnef.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/general/delnef.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Delete DELFT3D-NEFIS files if exists
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    character(*), intent(in)       :: filnam !  Name of communication file xxxx-<case><label>
!
! Local variables
!
    integer         :: ind      ! Length of filenam 
    integer         :: luntmp
    integer         :: newlun
    logical         :: exists
    character(256)  :: locfnm   ! Local file name 
!
!! executable statements -------------------------------------------------------
!
    if (parll .and. (inode /= master)) return
    !
    locfnm = ' '
    locfnm = filnam
    call noextspaces(locfnm    ,ind       )
    !
    ! test files existence
    !
    inquire (file = locfnm(:ind) // '.def', exist = exists)
    if (exists) then
       luntmp = newlun(gdp)
       open (luntmp, file = locfnm(:ind) // '.def')
       close (luntmp, status = 'delete')
    endif
    !
    ! test files existence
    !
    inquire (file = locfnm(:ind) // '.dat', exist = exists)
    if (exists) then
       luntmp = newlun(gdp)
       open (luntmp, file = locfnm(:ind) // '.dat')
       close (luntmp, status = 'delete')
    endif
end subroutine delnef
