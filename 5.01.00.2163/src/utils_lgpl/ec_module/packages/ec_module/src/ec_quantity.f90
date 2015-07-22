module ec_quantity
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
!  $Id: ec_quantity.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_quantity.f90 $
!!--description-----------------------------------------------------------------
!
! see ec_module.f90
! More information: http://wiki.deltares.nl/display/FLOW/EC-Module
!
!!--pseudo code and references--------------------------------------------------
!
! arjen.markus@deltares.nl
! adri.mourits@deltares.nl
!
!!--declarations----------------------------------------------------------------
  use precision
  use ec_parameters
  use ec_message
  use ec_typedefs
  !
  implicit none

  private
!
! parameters
!
! interfaces
!
  interface init
    module procedure init_quantity
  end interface
  interface dump
    module procedure dump_quantity
  end interface
!
! public entities
!
  public :: tQuantity
  public :: init_quantity
  public :: getQuantity_name
  public :: getQuantityId
  public :: increaseSize_quantities
  public :: dump
contains
!
!
!==============================================================================
function init_quantity(quantity,id,name) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tQuantity), intent(inout) :: quantity
  integer        , intent(in)    :: id
  character(*)   , intent(in)    :: name
  !
  ! locals
  !
  ! body
  quantity%id   = id
  quantity%name = name
  success       = .true.
end function init_quantity
!
!
!==============================================================================
function getQuantity_name(quantities, name) result(quantityId)
  !
  ! result: unique quantity id
  integer :: quantityId
  !
  ! arguments
  type(tQuantity), dimension(:), intent(in) :: quantities
  character(*)                 , intent(in) :: name
  !
  ! locals
  integer :: i
  integer :: retVal
  !
  ! body
  retVal = 0
  do i=1, size(quantities)
    if (quantities(i)%name == name) then
      if (retVal == 0) then
        !
        ! i is the first match found
        !
        retVal = i
      else
        !
        ! retVal >  0: i is the second match found
        ! retVal = -1: i is the third/fourth/... match found
        ! In both cases:
        !
        retVal = -1
        call setECMessage("ERROR: ec_quantity::getQuantity_name: More than one quantity with the name", '"'//trim(name)//'"')
      endif
    endif
  enddo
  if (retVal == -1) then
    retVal = 0
  else if (retVal == 0) then
    call setECMessage("ERROR: ec_quantity::getQuantity_name: Quantity not found with the name", '"'//trim(name)//'"')
  endif
  quantityId = retVal
end function getQuantity_name
!
!
!==============================================================================
function increaseSize_quantities(quantities) result (k)
  !
  ! result
  integer :: k
  !
  ! arguments
  type(tQuantity), dimension(:), pointer :: quantities
  !
  ! locals
  integer                                :: istat
  logical                                :: success
  type(tQuantity), dimension(:), pointer :: newQuantities
  !
  ! body
  k = size(quantities) + 1
  allocate(newQuantities(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_quantity::increaseSize_quantities: Unable to allocate additional memory")
    return
  endif
  success = copyArray_quantities(quantities,newQuantities)
  deallocate (quantities, STAT = istat)
  quantities => newQuantities
  if (.not. success) k=0
end function increaseSize_quantities
!
!
!==============================================================================
function copyArray_quantities(sourceArr, targetArr) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tQuantity), dimension(:), pointer :: sourceArr
  type(tQuantity), dimension(:), pointer :: targetArr
  !
  ! locals
  integer :: k
  integer :: arrLenSource
  integer :: arrLenTarget
  !
  ! body
  arrLenSource = min(size(sourceArr), size(targetArr))
  arrLenTarget = size(targetArr)
  do k = 1,arrLenSource
    targetArr(k) = sourceArr(k)
  enddo
  do k = arrLenSource+1, arrLenTarget
    success = init_quantity(targetArr(k), 0, '')
    if (.not. success) return
  enddo
end function copyArray_quantities
!
!
!==============================================================================
function getQuantityId(quantity) result (id)
  !
  ! result
  integer :: id
  !
  ! arguments
  type(tQuantity) :: quantity
  !
  ! body
  id = quantity%id
end function getQuantityId
!
!
!==============================================================================
subroutine dump_quantity(quantity)
  !
  ! arguments
  type(tQuantity) :: quantity
  !
  ! body
  write (*,'(a,i0)') '    id  : ', quantity%id
  write (*,'(a,a)' ) '    name: ', trim(quantity%name)
end subroutine dump_quantity



end module ec_quantity
