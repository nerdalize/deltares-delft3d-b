subroutine nefisio_dealloc(gdp)
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
!  $Id: nefisio_dealloc.f90 1848 2012-09-14 17:42:05Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/gdp/nefisio_dealloc.f90 $
!!--description-----------------------------------------------------------------
!
! NONE
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
!
! Local variables
!
    integer                        :: i
    integer                        :: istat
    type (nefiselement) , pointer  :: nefiselem
!
!! executable statements -------------------------------------------------------
!
    do i = 1, nrnefisfiles
       nefiselem => gdp%nefisio%nefiselem(i)
       if (associated(nefiselem%elmdms)) deallocate (nefiselem%elmdms, stat=istat)
       if (associated(nefiselem%nbytsg)) deallocate (nefiselem%nbytsg, stat=istat)
       if (associated(nefiselem%elmunt)) deallocate (nefiselem%elmunt, stat=istat)
       if (associated(nefiselem%elmnms)) deallocate (nefiselem%elmnms, stat=istat)
       if (associated(nefiselem%elmqty)) deallocate (nefiselem%elmqty, stat=istat)
       if (associated(nefiselem%elmtps)) deallocate (nefiselem%elmtps, stat=istat)
       if (associated(nefiselem%elmdes)) deallocate (nefiselem%elmdes, stat=istat)
    enddo
    deallocate (gdp%nefisio, stat=istat)
end subroutine nefisio_dealloc
