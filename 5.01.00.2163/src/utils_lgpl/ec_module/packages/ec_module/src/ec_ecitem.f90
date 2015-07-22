module ec_ecitem
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
!  $Id: ec_ecitem.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_ecitem.f90 $
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
  use ec_quantity
  use ec_elementset
  use ec_field
  !
  implicit none
!
! parameters
!
  !
  ! enumeration for ECItem types
  integer, parameter :: ECItemType_source  = 1
  integer, parameter :: ECItemType_target  = 2
  integer, parameter :: ECItemType_both    = 3
  private
!
! interfaces
!
  interface ECItemSetValue
    module procedure ECItemSetValue
  end interface
  interface ECRewind
    module procedure ECRewind_item
  end interface
  interface dump
    module procedure dump_ECItem
  end interface
  interface addECItem_viaData_item
    module procedure addECItem_viaData_item_orig
    module procedure addECItem_viaData_item_name
  end interface
  interface getECItem_item
    module procedure getECItem_item1
    module procedure getECItem_item2
    module procedure getECItem_item_name
  end interface
!
! public entities
!
  public :: tECItem
  public :: tECItemPtr
  public :: increaseSize_ECItemPtrs
  public :: getECItem_item
  public :: getECItemId
  public :: ECItemGetQuantityId
  public :: ECItemGetElementSetId
  public :: free_ECItemPtrs
  public :: ECItemAddProvider_item
  public :: addECItem_viaData_item
  public :: ECItemAddConnection_item
  public :: addECItemField_ref_1d
  public :: addECItemField_ref_2d
  public :: ECItemAddFields_val
  public :: setQuantity
  public :: setElementSet
  public :: setType
  public :: ECItemSetValue
  public :: ECRewind
  public :: swapECItem
  public :: ECItemType_source
  public :: ECItemType_target
  public :: dump
contains
!
!
!==============================================================================
function init_ECItem(ECItem) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem) :: ECItem
  !
  ! locals
  !
  ! body
  ECItem%id           = 0
  ECItem%type         = ECItemType_target
  ECItem%quantityId   = 0
  ECItem%elementSetId = 0
  ECItem%t0FieldId    = 0
  ECItem%t1FieldId    = 0
  nullify(ECItem%providerIds)
  nullify(ECItem%connectionIds)
  nullify(ECItem%converterIds)
  nullify(ECItem%quantity)
  nullify(ECItem%elementSet)
  nullify(ECItem%fields)
  success = .true.
end function init_ECItem
!
!
!==============================================================================
function increaseSize_ECItemPtrs(ECItemPtrs) result (k)
  !
  ! result
  integer :: k
  !
  ! arguments
  type(tECItemPtr), dimension(:), pointer :: ECItemPtrs
  !
  ! locals
  integer                                 :: istat
  logical                                 :: success
  type(tECItemPtr), dimension(:), pointer :: newECItemPtrs
  !
  ! body
  k = size(ECItemPtrs) + 1
  allocate(newECItemPtrs(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    k = 0
    call setECMessage("ERROR: ec_ecitem::increaseSize_ECItemPtrs: Unable to allocate additional memory")
    return
  endif
  success = copyArray_ECItemPtrs(ECItemPtrs,newECItemPtrs)
  if (.not. success) k=0
  if (associated(ECItemPtrs)) deallocate (ECItemPtrs, STAT = istat)
  ECItemPtrs => newECItemPtrs
end function increaseSize_ECItemPtrs
!
!
!==============================================================================
function copyArray_ECItemPtrs(sourceArr, targetArr) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItemPtr), dimension(:), pointer :: sourceArr
  type(tECItemPtr), dimension(:), pointer :: targetArr
  !
  ! locals
  integer :: i
  integer :: istat
  integer :: k
  integer :: arrLenSource
  integer :: arrLenTarget
  logical :: retVal
  !
  ! body
  success      = .true.
  arrLenSource = min(size(sourceArr), size(targetArr))
  arrLenTarget = size(targetArr)
  do k = 1,arrLenSource
    targetArr(k)%ECItemPtr => sourceArr(k)%ECItemPtr
  enddo
  do k = arrLenSource+1, arrLenTarget
    allocate(targetArr(k)%ECItemPtr, STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      call setECMessage("ERROR: ec_ecitem::copyArray_ECItemPtrs: Unable to allocate additional memory")
      return
    endif
    retVal = init_ECItem(targetArr(k)%ECItemPtr)
    if (.not. retVal) success=.false.
  enddo
end function copyArray_ECItemPtrs
!
!
!==============================================================================
function free_ECItemPtrs(ECItemPtrs) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItemPtr), dimension(:), pointer :: ECItemPtrs
  !
  ! locals
  integer :: istat
  integer :: k
  !
  ! body
  do k = 1,size(ECItemPtrs)
    if (associated(ECItemPtrs(k)%ECItemPtr)) then
      if (associated(ECItemPtrs(k)%ECItemPtr%connectionIds)) deallocate(ECItemPtrs(k)%ECItemPtr%connectionIds, STAT = istat)
      if (associated(ECItemPtrs(k)%ECItemPtr%fields))        deallocate(ECItemPtrs(k)%ECItemPtr%fields       , STAT = istat)
      deallocate(ECItemPtrs(k)%ECItemPtr, STAT = istat)
    endif
  enddo
  if (associated(ECItemPtrs)) deallocate (ECItemPtrs, STAT = istat)
  success = .true.
end function free_ECItemPtrs
!
!
!==============================================================================
! ECItem subroutines
!==============================================================================
function getECItem_item1(ECItemPtrs, qId, elSetId) result(itemId)
  !
  ! Return id when already present
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECItemPtr), dimension(:), pointer    :: ECItemPtrs
  integer                       , intent(in) :: qId
  integer                       , intent(in) :: elSetId
  !
  ! locals
  integer  :: item
  !
  ! body
  itemId = 0
  do item = 1,size(ECItemPtrs)
    if (      ECItemPtrs(item)%ECItemPtr%quantityId   == qId    &
        .and. ( ECItemPtrs(item)%ECItemPtr%elementSetId == elSetId )) then
      itemId = item
      return
    endif
  enddo
end function getECItem_item1
!
!==============================================================================
function getECItem_item_name(ECItemPtrs, name) result(itemId)
  !
  ! Return id when already present, based on its name
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECItemPtr), dimension(:), pointer    :: ECItemPtrs
  character(len=*)                           :: name
  !
  ! locals
  integer  :: item
  !
  ! body
  itemId = 0
  do item = 1,size(ECItemPtrs)
    if (      ECItemPtrs(item)%ECItemPtr%name   == name ) then
      itemId = item
      return
    endif
  enddo
end function getECItem_item_name
!
!==============================================================================
function getECItem_item2(ECItemPtrs, qId) result(itemId)
  !
  ! Return id when already present
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECItemPtr), dimension(:), pointer    :: ECItemPtrs
  integer                       , intent(in) :: qId
  !
  ! locals
  integer  :: item
  !
  ! body
  itemId = 0
  do item = 1,size(ECItemPtrs)
    if (      ECItemPtrs(item)%ECItemPtr%quantityId   == qId  ) then
      itemId = item
      return
    endif
  enddo
end function getECItem_item2
!
!
!==============================================================================
function ECItemGetQuantityId(ECItem) result(qId)
  !
  ! result
  integer :: qId
  !
  ! arguments
  type(tECItem) :: ECItem
  !
  ! locals
  !
  ! body
  qId = ECItem%quantityId
end function ECItemGetQuantityId
!
!
!==============================================================================
function setQuantity(ECItem, quantity) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem)            :: ECItem
  type(tQuantity), pointer :: quantity
  !
  ! locals
  !
  ! body
  ECItem%quantity => quantity
  success = .true.
end function setQuantity
!
!
!==============================================================================
function ECItemGetElementSetId(ECItem) result(elSetId)
  !
  ! result
  integer :: elSetId
  !
  ! arguments
  type(tECItem) :: ECItem
  !
  ! locals
  !
  ! body
  elSetId = ECItem%elementSetId
end function ECItemGetElementSetId
!
!
!==============================================================================
function setElementSet(ECItem, elSet) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem)               :: ECItem
  type(ecElementSet), pointer :: elSet
  !
  ! locals
  !
  ! body
  ECItem%elementSet => elSet
  success = .true.
end function setElementSet
!
!
!==============================================================================
function setType(ECItem, aType) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem)              :: ECItem
  integer      , intent(in)  :: aType
  !
  ! locals
  !
  ! body
  ECItem%type = aType
  success     = .true.
end function setType
!
!
!==============================================================================
function addECItem_viaData_item_orig(ECItemPtrs, quantity, elementSet) result(itemId)
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECItemPtr), dimension(:), pointer :: ECItemPtrs
  type(tQuantity)               , pointer :: quantity
  type(ecElementSet)            , pointer :: elementSet
  !
  ! locals
  integer          :: elementSetId
  integer          :: quantityId
  integer          :: lenstr1, lenstr2
  character(len=maxNameLen), pointer :: item_name
  !
  ! body
  elementSetId = getElementSetId(elementSet)
  quantityId   = getQuantityId(quantity)
  itemId       = getECItem_item(ECItemPtrs, quantityId, elementSetId)
  if (itemId == 0) then
    itemId                                    = increaseSize_ECItemPtrs(ECItemPtrs)
    ECItemPtrs(itemId)%ECItemPtr%id           = itemId
    ECItemPtrs(itemId)%ECItemPtr%quantity     => quantity
    ECItemPtrs(itemId)%ECItemPtr%quantityId   =  getQuantityId(quantity)
    ECItemPtrs(itemId)%ECItemPtr%elementSet   => elementSet
    ECItemPtrs(itemId)%ECItemPtr%elementSetId =  getElementSetId(elementSet)
    !
    ! built default name: quantity%name // '|' // elementSet%id
    !
    item_name => ECItemPtrs(itemId)%ECItemPtr%name
    write(item_name,'(i0)') elementSet%id
    lenstr1 = len_trim(item_name)
    lenstr2 = len_trim(quantity%name)
    if (lenstr1 + lenstr2 + 1 > maxNameLen) then
        lenstr2 = maxNameLen - 1 - lenstr1
    endif   
    item_name(lenstr2 + 2: lenstr1 + lenstr2 + 1) = item_name(1:lenstr1)
    item_name(lenstr2 + 1 : lenstr2 + 1) = '|'
    item_name(1:lenstr2) = quantity%name(1:lenstr2)
  endif
end function addECItem_viaData_item_orig
!
!==============================================================================
function addECItem_viaData_item_name(ECItemPtrs, quantity, elementSet, name) result(itemId)
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECItemPtr), dimension(:), pointer :: ECItemPtrs
  type(tQuantity)               , pointer :: quantity
  type(ecElementSet)            , pointer :: elementSet
  character(len=*)                        :: name
  !
  ! locals
  integer :: elementSetId
  integer :: quantityId
  !
  ! body
  elementSetId = getElementSetId(elementSet)
  quantityId   = getQuantityId(quantity)
  itemId       = getECItem_item(ECItemPtrs, name)
  if (itemId == 0) then
    itemId                                    = increaseSize_ECItemPtrs(ECItemPtrs)
    ECItemPtrs(itemId)%ECItemPtr%id           = itemId
    ECItemPtrs(itemId)%ECItemPtr%name         = name
    ECItemPtrs(itemId)%ECItemPtr%quantity     => quantity
    ECItemPtrs(itemId)%ECItemPtr%quantityId   =  getQuantityId(quantity)
    ECItemPtrs(itemId)%ECItemPtr%elementSet   => elementSet
    ECItemPtrs(itemId)%ECItemPtr%elementSetId =  getElementSetId(elementSet)
  endif
end function addECItem_viaData_item_name
!
!
!==============================================================================
function ECitemAddProvider_item(ECItem, providerId) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem)             :: ECItem
  integer      , intent(in) :: providerId
  !
  ! locals
  !
  ! body
  call addUniqueInt(ECItem%providerIds, providerId)
  success = .true.
end function ECitemAddProvider_item
!
!
!==============================================================================
function ECitemAddConnection_item(ECItem, connectionId) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem)             :: ECItem
  integer      , intent(in) :: connectionId
  !
  ! locals
  !
  ! body
  call addUniqueInt(ECItem%connectionIds, connectionId)
  success = .true.
end function ECitemAddConnection_item
!
!
!==============================================================================
function ECItemAddFields_val(ECItem, numFields, numDim, dim1Len, dim2Len, dim3Len) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem)                   :: ECItem
  integer            , intent(in) :: numFields
  integer            , intent(in) :: numDim
  integer            , intent(in) :: dim1Len
  integer, optional  , intent(in) :: dim2Len
  integer, optional  , intent(in) :: dim3Len
  !
  ! locals
  type(tECField), pointer :: field
  integer                 :: elementSetId
  integer                 :: i
  integer                 :: istat
  !
  ! body
  allocate(ECItem%fields(0:numFields-1), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_ecitem::ECItemAddFields_val: Unable to allocate additional memory")
    return
  endif
  do i=0, numFields-1
    field   => ECItem%fields(i)
    elementSetId = getElementSetId(EcItem%elementSet)
    success =  init_field(field, elementSetId, numDim, dim1Len, dim2Len, dim3Len)
    if (.not. success) return
  enddo
  ECItem%t0FieldId = 0
  if (numFields == 1) then
    ECItem%t1FieldId = 0
  else
    ECItem%t1FieldId = 1
  endif
end function ECItemAddFields_val
!
!
!==============================================================================
function addECItemField_ref_1d(ECItem, time, array) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem)                      :: ECItem
  real(fp), dimension(:), pointer    :: array
  real(hp)              , intent(in) :: time
  !
  ! locals
  integer                 :: istat
  integer                 :: elementSetId
  type(tECField), pointer :: field
  !
  ! body
  allocate(ECItem%fields(0:0), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_ecitem::addECItemField_ref_1d: Unable to allocate additional memory")
    return
  endif
  field        => ECItem%fields(0)
  elementSetId =  getElementSetId(EcItem%elementSet)
  success      =  init_field(field, time, array, elementSetId)
end function addECItemField_ref_1d
!
!
!==============================================================================
function addECItemField_ref_2d(ECItem, time, array) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem)                        :: ECItem
  real(fp), dimension(:,:), pointer    :: array
  real(hp)                , intent(in) :: time
  !
  ! locals
  integer                 :: istat
  integer                 :: elementSetId
  type(tECField), pointer :: field
  !
  ! body
  allocate(ECItem%fields(0:0), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_ecitem::addECItemField_ref_2d: Unable to allocate additional memory")
    return
  endif
  field        => ECItem%fields(0)
  elementSetId =  getElementSetId(EcItem%elementSet)
  success      =  init_field(field, time, array, elementSetId)
end function addECItemField_ref_2d
!
!
!==============================================================================
function getECItemId(ECItem) result (id)
  !
  ! result
  integer :: id
  !
  ! arguments
  type(tECItem) :: ECItem
  !
  ! body
  id = ECItem%id
end function getECItemId
!
!
!==============================================================================
function ECItemSetValue(ECItem, value) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem)             :: ECItem
  real(fp)     , intent(in) :: value
  !
  ! locals
  integer :: i
  logical :: retVal
  !
  ! body
  success = .true.
  do i=0, size(ECItem%fields) - 1
    retVal = setField(ECItem%fields(i), value)
    if (.not. retVal) success=.false.
  enddo
end function ECItemSetValue
!
!
!==============================================================================
subroutine swapECItem(ECItem)
  !
  ! arguments
  type(tECItem) :: ECItem
  !
  ! body
  ECItem%t0FieldId =     ECItem%t1FieldId
  ECItem%t1FieldId = 1 - ECItem%t0FieldId
end subroutine swapECItem
!
!
!==============================================================================
function ECRewind_item(ECItem) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECItem) :: ECItem
  !
  ! locals
  integer                 :: i
  type(tECField), pointer :: field
  logical                 :: retVal
  !
  ! body
  success = .true.
  do i=0, size(ECItem%fields) - 1
    retVal = emptyField(ECItem%fields(i))
    if (.not. retVal) success=.false.
  enddo
end function ECRewind_item
!
!
!==============================================================================
subroutine dump_ECItem(ECItem)
  !
  ! arguments
  type(tECItem) :: ECItem
  !
  ! locals
  integer :: i
  !
  ! body
  write (*,'(a,i0)') '    id          : ', ECItem%id
  write (*,'(a,i0)') '    type        : ', ECItem%type
  write (*,'(a,i0)') '    quantityId  : ', ECItem%quantityId
  write (*,'(a,i0)') '    elementSetId: ', ECItem%elementSetId
  write (*,'(a,i0)') '    t0FieldId   : ', ECItem%t0FieldId
  write (*,'(a,i0)') '    t1FieldId   : ', ECItem%t1FieldId
  do i=1, size(ECItem%providerIds)
    write (*,'(a,i0,a,i0)') '    providerId  (',i,'): ', ECItem%providerIds(i)
  enddo
  do i=1, size(ECItem%connectionIds)
    write (*,'(a,i0,a,i0)') '    connectionId(',i,'): ', ECItem%connectionIds(i)
  enddo
  do i=1, size(ECItem%converterIds)
    write (*,'(a,i0,a,i0)') '    converterId (',i,'): ', ECItem%converterIds(i)
  enddo
  do i=0, size(ECItem%fields)-1
    write (*,'(a,i0,a,f20.5)') '    field    (',i,')time: ', ECItem%fields(i)%time
  enddo
end subroutine dump_ECItem



end module ec_ecitem
