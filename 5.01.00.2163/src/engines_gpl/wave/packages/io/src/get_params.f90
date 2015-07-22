subroutine get_params(tscale, rho, filnam)
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
!  $Id: get_params.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/io/src/get_params.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Local parameters
!
    integer, parameter :: nelmx = 8
!
! Global variables
!
    real, intent(out)               :: tscale
    real, intent(out)               :: rho
    character(256)                  :: filnam  ! filename com-file
!
! Local variables
!
    integer                         :: celidt
    integer                         :: error
    integer                         :: ielem
    integer                         :: ierr
    integer, dimension(6, nelmx)    :: elmdms
    integer, dimension(nelmx)       :: nbytsg
    real, dimension(:), allocatable :: rbuff   ! read buffer
    logical                         :: wrswch
    character(10), dimension(nelmx) :: elmunt
    character(16)                   :: grpnam
    character(16), dimension(nelmx) :: elmnms
    character(16), dimension(nelmx) :: elmqty
    character(16), dimension(nelmx) :: elmtps
    character(64), dimension(nelmx) :: elmdes
    !
    data grpnam/'PARAMS'/
    data elmnms/'AG', 'RHOW', 'DT', 'NFLTYP', 'TSCALE', 'IT01', 'IT02', 'ITLEN'/
    data elmqty/8*' '/
    data elmunt/'[  M/S2 ]', '[ KG/M3 ]', '[ TUNIT ]', '[   -   ]', '[   S   ]', &
              & '[YYYYMMDD]', '[ HHMMSS]', '[ TSCALE]'/
    data elmtps/3*'REAL', 'INTEGER', 'REAL', 3*'INTEGER'/
    data nbytsg/8*4/
!
!! executable statements -------------------------------------------------------
!
    rho    = -999.0
    tscale = -999.0
    allocate(rbuff(1))
    wrswch = .false.
    ielem  = 1
    celidt = 1
    call filldm(elmdms    ,1         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,2         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,3         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,4         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,5         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,6         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,7         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    call filldm(elmdms    ,8         ,1         ,1         ,0         , &
              & 0         ,0         ,0         )
    !
    ielem = 2
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    if (error /= 0) goto 200
    rho = rbuff(1)
    !
    ielem = 5
    call putgtr(filnam    ,grpnam    ,nelmx     ,elmnms    ,elmdms    , &
              & elmqty    ,elmunt    ,elmdes    ,elmtps    ,nbytsg    , &
              & elmnms(ielem)        ,celidt    ,wrswch    ,error     ,rbuff     )
    if (error /= 0) goto 200
    tscale = rbuff(1)
  200 continue
    if (error /= 0) then
       write(*,'(5a)') '*** ERROR: Reading file "', trim(filnam), '.dat" or "', trim(filnam), '.def"'
       stop
    endif
    deallocate(rbuff, stat=ierr)
end subroutine get_params
