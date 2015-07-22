subroutine defnewgrp(nefisgroup ,filnam    ,grpnam    ,gdp)
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
!  $Id: defnewgrp.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/defnewgrp.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Read or writes character buffer to nefis files
!              Tests values input consistency for elmnam and
!              elmnms and for local and global dimensions
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                     , pointer :: lundia
    integer, dimension(:, :)    , pointer :: elmdms
    type (nefiselement)         , pointer :: nefiselem
    integer                     , pointer :: nelmx
    integer      , dimension(:) , pointer :: nbytsg
    character(10), dimension(:) , pointer :: elmunt
    character(16), dimension(:) , pointer :: elmnms
    character(16), dimension(:) , pointer :: elmqty
    character(16), dimension(:) , pointer :: elmtps
    character(64), dimension(:) , pointer :: elmdes
!
! Global variables
!
    character(*), intent(in) :: filnam     !!  Name of NEFIS file
    character(*)             :: grpnam     !!  Name of data group. This name is
                                           !!  also used as the name for the cell
                                           !!  and group-definition.
    integer     , intent(in) :: nefisgroup
!
! Local variables
!
    integer               :: error
    integer               :: fds
    integer               :: ie
    integer, external     :: clsnef
    integer, external     :: credat
    integer, external     :: open_datdef
    integer, external     :: defcel
    integer, external     :: defelm
    integer, external     :: defgrp
    integer, external     :: neferr
    character(8)          :: localtyp
    character(10)         :: localunt
    character(16)         :: localnm
    character(16)         :: localqty
    character(64)         :: localdes
    character(1024)       :: error_string
!
!! executable statements -------------------------------------------------------
!
    lundia  => gdp%gdinout%lundia
    nefiselem => gdp%nefisio%nefiselem(nefisgroup)
    elmdms  => nefiselem%elmdms
    nelmx   => nefiselem%nelmx
    nbytsg  => nefiselem%nbytsg
    elmunt  => nefiselem%elmunt
    elmnms  => nefiselem%elmnms
    elmqty  => nefiselem%elmqty
    elmtps  => nefiselem%elmtps
    elmdes  => nefiselem%elmdes
    !
    ! Initialize local variables
    !
    error = open_datdef(filnam   ,fds      )
    if (error /= 0) then
       write(error_string,'(2a)') 'While trying to open dat/def-file ',trim(filnam)
       call prtnefiserr(trim(error_string), gdp)
       error = 0
    endif
    !
    ! Create elements
    !
    do ie = 1, nelmx
       !
       ! Local copies of elementtype/elementquantity should not be necessary
       ! But testcase 17.03-2dred1dump really prefers it
       !
       localtyp = elmtps(ie)(1:8)
       localqty = elmqty(ie)(1:16)
       localunt = elmunt(ie)(1:10)
       localnm  = elmnms(ie)(1:16)
       localdes = elmdes(ie)(1:64)
       error = defelm(fds   , localnm    , localtyp  , nbytsg(ie), &
             & localqty     , localunt   , localdes  ,             &
             & elmdms(1, ie), elmdms(2, ie))
       if (error /= 0) then
          !
          ! Defining an element twice will cause an error code
          ! (Groups 'map-rol-series' and 'map-infsed-serie' both
          !  have the element ITMAPS)
          ! Assume that the same element definition can be
          ! used for both groups and don't report an error
          ! If another type of error occurs, it is assumed that it
          ! will cause a recognisable error during writing.
          !
          error = 0
       endif
    enddo
    !
    ! Create cells
    !
    error = defcel(fds, grpnam, nelmx, elmnms)
    if (error /= 0) then
       write(error_string,'(5a)') 'While trying to define a cell for group ', &
                     & trim(grpnam), ' in file ', trim(filnam), '.def'
       call prtnefiserr(trim(error_string), gdp)
       error = 0
    endif
    !
    ! Create group on definition file
    !
    error = defgrp(fds, grpnam, grpnam, 1, 0, 1)
    if (error /= 0) then
       write(error_string,'(5a)') 'While trying to define group ', &
                     & trim(grpnam), ' in file ', trim(filnam), '.def'
       call prtnefiserr(trim(error_string), gdp)
       error = 0
    endif
    !
    ! Create group on data file
    !
    error = credat(fds, grpnam, grpnam)
    if (error /= 0) then
       write(error_string,'(5a)') 'While trying to create group ', &
                     & trim(grpnam), ' in file ', trim(filnam), '.dat'
       call prtnefiserr(trim(error_string), gdp)
       error = 0
    endif
    error = clsnef(fds)
    if (error /= 0) then
       write(error_string,'(2a)') 'While trying to close def/dat-file ', trim(filnam)
       call prtnefiserr(trim(error_string), gdp)
       write(lundia,*) 'While trying to close file ',trim(filnam)
    endif
end subroutine defnewgrp
