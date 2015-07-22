function open_datdef(filnam    ,fds       )
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
!  $Id: open_datdef.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/general/open_datdef.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Detect the number of time steps on filnam (map-
!              or his-file)
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer              :: open_datdef
    integer, intent(out) :: fds
    character(*)         :: filnam
!
! Local variables
!
    integer           :: len_fn
    integer, external :: crenef
    character(1)      :: coding
    character(256)    :: dat_file
    character(256)    :: def_file
!
!! executable statements -------------------------------------------------------
!
    call noextspaces(filnam    ,len_fn    )
    dat_file = filnam(1:len_fn)//'.dat'
    def_file = filnam(1:len_fn)//'.def'
    !
    coding = 'N'
    open_datdef = crenef(fds, dat_file, def_file, coding, 'u')
end function open_datdef
