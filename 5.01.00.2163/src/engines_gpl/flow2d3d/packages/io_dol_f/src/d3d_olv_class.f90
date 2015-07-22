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
!  $Id: d3d_olv_class.f90 1848 2012-09-14 17:42:05Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io_dol_f/src/d3d_olv_class.f90 $
module d3d_olv_class

use precision

implicit none

type, public ::  olv_handle_t
    integer :: runningFlag = 0  ! 0=Flow not running (waiting); 1=Flow is iterating time steps
    integer :: currentStep = 0
    integer :: endTimeStep = 0
    integer :: endFlag     = 0  ! 0 = Flow simulation has not finished yet; 1 = Flow has finished
    integer :: timeStepInt = -1

    character(len=260)          :: runid
    integer, pointer            :: mmax
    integer, pointer            :: nmax
    integer, pointer            :: nlb, nub, mlb, mub, kmax
    
    integer, pointer            :: ltur
    integer, pointer            :: lstsci
    integer(pntrsize), pointer  :: thick
    integer(pntrsize), pointer  :: xcor
    integer(pntrsize), pointer  :: ycor
    integer(pntrsize), pointer  :: gvz
    integer(pntrsize), pointer  :: guz
    integer(pntrsize), pointer  :: xz
    integer(pntrsize), pointer  :: yz
    integer(pntrsize), pointer  :: kcs
    integer(pntrsize), pointer  :: kfs
    integer(pntrsize), pointer  :: kfu
    integer(pntrsize), pointer  :: kfv
    integer(pntrsize), pointer  :: kfsz1
    integer(pntrsize), pointer  :: alfas
    integer(pntrsize), pointer  :: s1
    integer(pntrsize), pointer  :: dp
    integer(pntrsize), pointer  :: dps
    integer(pntrsize), pointer  :: u1
    integer(pntrsize), pointer  :: v1
    integer(pntrsize), pointer  :: r1
    integer(pntrsize), pointer  :: rtur1
    integer(pntrsize), pointer  :: namcon
 
    real(fp), pointer           :: zbot
    real(fp), pointer           :: ztop
    logical                     :: zmodel
end type

type, public :: OLVHandle
    type(olv_handle_t), pointer :: fields => null()
end type

private

public new_olv
public free_olv
public isNull_olv

contains
!
!------------------------------------------------------------------------------
subroutine new_olv(handle)
    type(OLVHandle) :: handle
    !
    ! locals
    type(olv_handle_t), pointer :: olv
    !
    ! body
    allocate( olv )

    olv%runid  = ''
    
    olv%mmax   => null()
    olv%nmax   => null()
    olv%nlb    => null()
    olv%nub    => null()
    olv%mlb    => null()
    olv%mub    => null()
    olv%kmax   => null()
    olv%thick  => null()
    
    olv%ltur   => null()
    olv%lstsci => null()
    olv%xcor   => null()
    olv%ycor   => null()
    olv%gvz    => null()
    olv%guz    => null()
    olv%xz     => null()
    olv%yz     => null()
    olv%kcs    => null()
    olv%kfs    => null()
    olv%kfu    => null()
    olv%kfv    => null()
    olv%kfsz1  => null()
    olv%alfas  => null()
    olv%s1     => null()
    olv%dp     => null()
    olv%dps    => null()
    olv%u1     => null()
    olv%v1     => null()
    olv%r1     => null()
    olv%rtur1  => null()
    olv%namcon => null()
    
    olv%zbot   => null()
    olv%ztop   => null()
    olv%zmodel = .false.

    handle%fields => olv
end subroutine new_olv
!
!------------------------------------------------------------------------------
subroutine free_olv(handle)
    type(OLVHandle) :: handle

    integer                     :: istat
    type(olv_handle_t), pointer :: olv

    if (isNull_olv(handle)) return

    olv => handle%fields

    olv%runid  = ''
    !
    ! The following pointers point to the GDP structure
    ! Deallocation is the responsibility of the GDP related routines
    ! Here: just stop pointing to the GDP structure
    ! TO DO: check whether these pointers are really necessary: why not using the GDP structure directly?
    !    
    olv%mmax   => null()
    olv%nmax   => null()
    olv%nlb    => null()
    olv%nub    => null()
    olv%mlb    => null()
    olv%mub    => null()
    olv%kmax   => null()
    olv%thick  => null()
    
    olv%ltur   => null()
    olv%lstsci => null()
    olv%xcor   => null()
    olv%ycor   => null()
    olv%gvz    => null()
    olv%guz    => null()
    olv%xz     => null()
    olv%yz     => null()
    olv%kcs    => null()
    olv%kfs    => null()
    olv%kfu    => null()
    olv%kfv    => null()
    olv%kfsz1  => null()
    olv%alfas  => null()
    olv%s1     => null()
    olv%dp     => null()
    olv%dps    => null()
    olv%u1     => null()
    olv%v1     => null()
    olv%r1     => null()
    olv%rtur1  => null()
    olv%namcon => null()
    
    olv%zbot   => null()
    olv%ztop   => null()
    olv%zmodel = .false.
    !
    ! Deallocate the olv structure
    ! Catch stat to avoid aborts; don't bother whether it succeeded or not
    ! 
    deallocate(handle%fields,stat=istat)
    nullify(handle%fields)
end subroutine
!
!------------------------------------------------------------------------------
logical function isNull_olv(handle) result(res)
    type(OLVHandle) :: handle

    res = .NOT. associated(handle%fields)
end function
!
!------------------------------------------------------------------------------
end module d3d_olv_class
