module ec_data
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
!  $Id: ec_data.f90 2144 2013-01-25 16:35:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_data.f90 $
!!--description-----------------------------------------------------------------
!
! HTML DOC START
!
! <TABLE>
!   <TR>
!     <TD colSpan=2>
!            Module: ec_data
!     </TD>
!     <TD colSpan=2>
!            Description:
!              Low level implementation of the EC-module.
!              Does not need to be changed when adding new providers/converters.
!            More information: http://wiki.deltares.nl/display/FLOW/EC-Module
!     </TD>
!   </TR>
!   <TR>
!     <TD>
!            Dependencies:
!              use ec_quantity
!              use ec_elementset
!              use ec_field
!              use ec_ecitem
!              use ec_connection
!     </TD>
!     <TD>
!            Data:
!              type tECData
!                integer                                       :: internalGrid
!                type(tQuantity)       , dimension(:), pointer :: quantities
!                type(ecElementSet)    , dimension(:), pointer :: elementSets
!                type(tECItemPtr)      , dimension(:), pointer :: ECItemPtrs
!                type(tECConnectionPtr), dimension(:), pointer :: connectionPtrs
!              end type tECData
!     </TD>
!   </TR>
!   <TR>
!     <TD colSpan=2>
!            Typical calls:
!              Get
!              Set
!              Init
!              Free
!              Add
!     </TD>
!   </TR>
! </TABLE>
! HTML DOC END
!
!!--pseudo code and references--------------------------------------------------
!
! adri.mourits@deltares.nl
!
!!--declarations----------------------------------------------------------------
  use precision
  use ec_parameters
  use ec_message
  use ec_quantity
  use ec_elementset
  use ec_field
  use ec_ecitem
  use ec_connection
  use ec_typedefs
  !
  implicit none
!
! parameters
!
!
! variables
!
!
! interfaces
!
  interface init
    !module procedure init_field
    !module procedure init_ECItem
    !module procedure init_connection
    module procedure init_ECData
  end interface
  interface increaseSize
    module procedure increaseSize_quantities
    module procedure increaseSize_elementSets
    module procedure increaseSize_ECItemPtrs
    module procedure increaseSize_connectionPtrs
  end interface
  interface free
    module procedure free_quantities
    module procedure free_elementSets
    module procedure free_ECItemPtrs
    module procedure free_connectionPtrs
    module procedure free_ECData
  end interface
  interface addElementSet
    module procedure ecdataAddElementSet_elementSet      ! ??? is thisone needed ???
    module procedure ecdataAddElementSet_components
    module procedure ecdataAddElementSet_IDbased
  end interface
  interface addQuantity
    module procedure ecdataAddQuantity_name
  end interface
  interface getQuantity
    module procedure ecdataGetQuantity
  end interface
  interface getElementSet
    module procedure ecdataGetElementSet
  end interface
  interface addConnection
    module procedure ecdataAddConnection
    module procedure ecdataAddConnection_ids
  end interface
  interface addECItem
    module procedure ecdataAddECItem_viaData
  end interface
  interface addECItem_viaData
    module procedure ecdataAddECItem_viaData
    module procedure ecdataAddECItem_with_name
  end interface
  interface getECItem
    module procedure ecdataGetECItem
    !module procedure ecdataGetECItem2
  end interface
  interface ECItemAddProvider
    module procedure ecdataECItemAddProvider
  end interface
  interface ECItemAddConnection
    module procedure ecdataECItemAddConnection
  end interface
  interface ECItemAddFields
    module procedure ecdataECItemAddFields
  end interface
  interface ECItemSetValue
    module procedure ecdataECItemSetValue
  end interface
  interface addECItemField
    module procedure addECItemField_ref
    module procedure addECItemField_ref2d
  end interface
  interface ECConnectionInit
    module procedure ecdataECConnectionInit
  end interface
  interface ECConnectionAddSource
    module procedure ecdataECConnectionAddSource
  end interface
  interface ECConnectionAddTarget
    module procedure ecdataECConnectionAddTarget
  end interface
  interface ECItemSetType
    module procedure ecdataECItemSetType
  end interface
  interface dump
    module procedure dump_ECData
  end interface
!
! public entities
!
! TODO: default is private !!
  private :: increaseSize
contains
!
!
!==============================================================================
! elementSet subroutines
!==============================================================================
function ecdataAddElementSet_components(ecData, x, y, kcs, spherical) result(id)
  !
  ! Return id when already present
  ! Create a new elementSet when not present and return id
  !
  ! result
  integer :: id
  !
  ! arguments
  type(tECData)                       :: ecData
  real(hp), dimension(:), intent(in)  :: x
  real(hp), dimension(:), intent(in)  :: y
  integer , dimension(:), intent(in)  :: kcs
  logical               , intent(in)  :: spherical
  !
  ! locals
  logical :: success
  !
  ! body
  id      = addElementSet_elem(ecData%elementSets, x, y, kcs, spherical)
  success = updateECItems(ecData)
  if (.not. success) id = 0
end function ecdataAddElementSet_components
!
!
!==============================================================================
function ecdataAddElementSet_IDbased(ecData, IDs) result(id)
  !
  ! Return id when already present
  ! Create a new elementSet when not present and return id
  !
  ! result
  integer :: id
  !
  ! arguments
  type(tECData)                       :: ecData
  character(len=*), dimension(:), pointer :: IDs
  !
  ! locals
  logical :: success
  !
  ! body
  id      = addElementSet_elem(ecData%elementSets, IDs)
  success = updateECItems(ecData)
  if (.not. success) id = 0
end function ecdataAddElementSet_IDbased
!


!==============================================================================
function ecdataGetElementSet(ecData, x, y, kcs, spherical) result(id)
  !
  ! Return id when already present
  !
  ! result
  integer :: id
  !
  ! arguments
  type(tECData)                       :: ecData
  real(hp), dimension(:), intent(in)  :: x
  real(hp), dimension(:), intent(in)  :: y
  integer , dimension(:), intent(in)  :: kcs
  logical               , intent(in)  :: spherical
  !
  ! locals
  !
  ! body
  id = getElementSet_elem(ecData%elementSets, x, y, kcs, spherical)
end function ecdataGetElementSet
!
!
!==============================================================================
function ecdataAddElementSet_elementSet(elementSet) result(elementSetId)
  !
  ! Return id when already present
  ! Create a new elementSet when not present and return id
  !
  ! result
  integer :: elementSetId
  !
  ! arguments
  type(ecElementSet) :: elementSet
  !
  ! body
  !??? is this function needed ???
  elementSetId = 0
end function ecdataAddElementSet_elementSet
!
!
!==============================================================================
function ecdataSetInternalGrid(ECData, elSetId) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData) :: ECData
  integer       :: elSetId
  !
  ! body
  ECData%internalGrid = elSetId
  success             = .true.
end function ecdataSetInternalGrid
!
!
!==============================================================================
function init_ECData(ECData) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData) :: ECData
  !
  ! locals
  integer :: istat
  !
  ! body
  !
  ECData%internalGrid = -1
                  allocate(ECData%quantities    (0), STAT = istat)
  if (istat == 0) allocate(ECData%elementSets   (0), STAT = istat)
  if (istat == 0) allocate(ECData%ECItemPtrs    (0), STAT = istat)
  if (istat == 0) allocate(ECData%connectionPtrs(0), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_data::init_ECData: Unable to allocate additional memory")
    return
  endif
  ECData%initialized = .true.
end function init_ECData
!
!
!==============================================================================
function free_quantities(quantities) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tQuantity), dimension(:), pointer :: quantities
  !
  ! locals
  integer :: istat
  !
  ! body
  if(associated(quantities)) deallocate (quantities, STAT = istat)
  success = .true.
end function free_quantities
!
!
!==============================================================================
function free_connectionPtrs(connectionPtrs) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConnectionPtr), dimension(:), pointer :: connectionPtrs
  !
  ! locals
  integer :: istat
  integer :: k
  logical :: retVal
  !
  ! body
  success = .true.
  do k = 1,size(connectionPtrs)
    if (associated(connectionPtrs(k)%ECConnectionPtr)) then
      retVal = free_connection(connectionPtrs(k)%ECConnectionPtr)
      if (.not. retVal) success = .false.
      deallocate(connectionPtrs(k)%ECConnectionPtr, STAT = istat)
    endif
  enddo
  if (associated(connectionPtrs)) deallocate (connectionPtrs, STAT = istat)
end function free_connectionPtrs
!
!
!==============================================================================
! quantity subroutines
!==============================================================================
function ecdataAddQuantity_name(ECData, name) result(quantityId)
  !
  ! result: unique quantity id
  integer :: quantityId
  !
  ! arguments
  type(tECData)             :: ECData
  character(*) , intent(in) :: name
  !
  ! local
  logical :: success
  !
  ! body
  quantityId = getQuantity_name(ECData%quantities, name)
  if (quantityId == 0) then
    quantityId = increaseSize(ECData%quantities)
    !
    ! Existing pointers to quantities in ECItems must be updated
    !
    success = updateECItems(ECData)
    if (.not. success) quantityId=0
    success = init_quantity(ECData%quantities(quantityId),quantityId,name)
    if (.not. success) quantityId=0
  endif
end function ecdataAddQuantity_name
!
!
!==============================================================================
function ecdataGetQuantity(ECData, name) result(quantityId)
  !
  ! result: unique quantity id
  integer :: quantityId
  !
  ! arguments
  type(tECData), intent(in) :: ECData
  character(*) , intent(in) :: name
  !
  quantityId = getQuantity_name(ECData%quantities,name)
end function ecdataGetQuantity
!
!
!==============================================================================
! ECItem subroutines
!==============================================================================
function ecdataGetECItem(ECData, qId, elSetId, name) result(itemId)
  !
  ! Return id when already present
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECData) :: ecData
  integer, optional       :: qId
  integer, optional       :: elSetId
  character(len=*), optional :: name
  !
  ! locals
  integer  :: item
  type(tECItem), dimension(:), pointer :: ECItems
  !
  ! body
  if (present(name)) then
     itemId = getECItem_item(ECData%ECItemPtrs, name)
  else if (present(elSetId)) then
     itemId = getECItem_item(ECData%ECItemPtrs, qId, elSetId)
  else
     itemId = getECItem_item(ECData%ECItemPtrs, qId)
  endif
end function ecdataGetECItem
!
!==============================================================================
function ecdataGetECItem2(ECData, qId) result(itemId)
  !
  ! Return id when already present
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECData) :: ecData
  integer       :: qId
  !
  ! locals
  integer  :: item
  type(tECItem), dimension(:), pointer :: ECItems
  !
  ! body
  itemId = getECItem_item(ECData%ECItemPtrs, qId)
end function ecdataGetECItem2
!
!
!==============================================================================
function ecdataAddECItem_viaData(ECData, qId, elSetId) result(itemId)
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECData)   :: ECData
  integer         :: qId
  integer         :: elSetId
  !
  ! locals
  type(tQuantity)   , pointer :: quantity
  type(ecElementSet), pointer :: elementSet
  !
  ! body
  quantity   => ECData%quantities(qId)
  elementSet => ECData%elementSets(elSetId)
  itemId = addECItem_viaData_item(ECData%ECItemPtrs, quantity, elementSet)
end function ecdataAddECItem_viaData
!
!==============================================================================
function ecdataAddECItem_with_name(ECData, qId, elSetId, name) result(itemId)
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECData)   :: ECData
  integer         :: qId
  integer         :: elSetId
  character(len=*) :: name
  !
  ! locals
  type(tQuantity)   , pointer :: quantity
  type(ecElementSet), pointer :: elementSet
  !
  ! body
  quantity   => ECData%quantities(qId)
  elementSet => ECData%elementSets(elSetId)
  itemId = addECItem_viaData_item(ECData%ECItemPtrs, quantity, elementSet, name)
end function ecdataAddECItem_with_name
!!
!==============================================================================
function ecdataECitemAddProvider(ECData, itemId, providerId) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)   :: ECData
  integer         :: itemId
  integer         :: providerId
  !
  ! locals
  !
  ! body
  success = ECItemAddProvider_item(ECData%ECItemPtrs(itemId)%ECItemPtr, providerId)
end function ecdataECitemAddProvider
!
!
!==============================================================================
function ecdataECitemAddConnection(ECData, itemId, connectionId) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)   :: ECData
  integer         :: itemId
  integer         :: connectionId
  !
  ! locals
  !
  ! body
  success = ECItemAddConnection_item(ECData%ECItemPtrs(itemId)%ECItemPtr, connectionId)
end function ecdataECitemAddConnection
!
!
!==============================================================================
function ecdataECItemAddFields(ECData, itemId, numFields, numDim, dim1Len, dim2Len, dim3Len) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)                   :: ECData
  integer            , intent(in) :: itemId
  integer            , intent(in) :: numFields
  integer            , intent(in) :: numDim
  integer            , intent(in) :: dim1Len
  integer, optional  , intent(in) :: dim2Len
  integer, optional  , intent(in) :: dim3Len
  !
  ! locals
  !
  ! body
  success = ECItemAddFields_val(ECData%ECItemPtrs(itemId)%ECItemPtr, numFields, numDim, dim1Len, dim2Len, dim3Len)
end function ecdataECItemAddFields
!
!
!==============================================================================
function addECItemField_ref(ECData, ECItemId, array) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)                   :: ECData
  integer                         :: ECItemId
  real(fp), dimension(:), pointer :: array
  !
  ! locals
  !
  ! body
  success = addECItemField_ref_1d(ECData%ECItemPtrs(ECItemId)%ECItemPtr, 0.0_hp, array)
end function addECItemField_ref
!
!
!==============================================================================
function addECItemField_ref2d(ECData, ECItemId, array) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)                   :: ECData
  integer                         :: ECItemId
  real(fp), dimension(:,:), pointer :: array
  !
  ! locals
  !
  ! body
  success = addECItemField_ref_2d(ECData%ECItemPtrs(ECItemId)%ECItemPtr, 0.0_hp, array)
end function addECItemField_ref2d
!
!
!==============================================================================
function updateECItems(ECData) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData) :: ECData
  !
  ! locals
  integer                    :: i
  integer                    :: quantityId
  integer                    :: elementSetId
  type(tQuantity)  , pointer :: quantity
  type(ecElementSet),pointer :: elSet
  logical                    :: retVal
  !
  ! body
  success = .true.
  do i=1,size(ECData%ECItemPtrs)
    quantityId   =  ECItemGetQuantityId(ECData%ECItemPtrs(i)%ECItemPtr)
    quantity     => ECData%quantities(QuantityId)
    elementSetId =  ECItemGetElementSetId(ECData%ECItemPtrs(i)%ECItemPtr)
    elSet        => ECData%elementSets(ElementSetId)
    retVal = setQuantity  (ECData%ECItemPtrs(i)%ECItemPtr, quantity)
    if (.not. retVal) success = .false.
    retVal = setElementSet(ECData%ECItemPtrs(i)%ECItemPtr, elSet   )
    if (.not. retVal) success = .false.
  enddo
end function updateECItems
!
!
!==============================================================================
function ecdataECItemSetValue(ECData, itemId, value) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)             :: ECData
  integer      , intent(in) :: itemId
  real(fp)     , intent(in) :: value
  !
  ! locals
  !
  ! body
  success = ECItemSetValue(ECData%ECItemPtrs(itemId)%ECItemPtr, value)
end function ecdataECItemSetValue
!
!
!==============================================================================
function ecdataECItemSetType(ECData, itemId, aType) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)             :: ECData
  integer      , intent(in) :: itemId
  integer      , intent(in) :: aType
  !
  ! locals
  !
  ! body
  success = setType(ECData%ECItemPtrs(itemId)%ECItemPtr, aType)
end function ecdataECItemSetType
!
!
!==============================================================================
! ECData subroutines
!==============================================================================
!
!
!==============================================================================
function free_ECData(ECData) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData), pointer :: ECData
  !
  ! locals
  integer :: istat
  integer :: k
  logical :: retVal
  !
  ! body
  success = .true.
  retVal  = free(ECData%elementSets)
  if (.not. retVal) success=.false.
  retVal  = free(ECData%quantities)
  if (.not. retVal) success=.false.
  retVal  = free(ECData%ECItemPtrs)
  if (.not. retVal) success=.false.
  retVal  = free(ECData%connectionPtrs)
  if (.not. retVal) success=.false.
  deallocate(ECData, STAT = istat)
end function free_ECData
!
!
!==============================================================================
subroutine dump_ECData(ECData)
  !
  ! arguments
  type(tECData) :: ECData
  !
  ! locals
  integer :: i
  !
  ! body
  write (*,'(a,i0)') '  internal grid: ', ECData%internalGrid
  write(*,'(a)') '  --- Connections -------------------------'
  do i=1, size(ECData%connectionPtrs)
    call dump(ECData%connectionPtrs(i)%ECConnectionPtr)
    write(*,'(a)') '    --------------------'
  enddo
  write(*,'(a)') '  --- ECItems -----------------------------'
  do i=1, size(ECData%ECItemPtrs)
    call dump(ECData%ECItemPtrs(i)%ECItemPtr)
    write(*,'(a)') '    --------------------'
  enddo
  write(*,'(a)') '  --- Quantities --------------------------'
  do i=1, size(ECData%quantities)
    call dump(ECData%quantities(i))
    write(*,'(a)') '    --------------------'
  enddo
  write(*,'(a)') '  --- ElementSets -------------------------'
  do i=1, size(ECData%elementSets)
    call dump(ECData%elementSets(i))
    write(*,'(a)') '    --------------------'
  enddo
end subroutine dump_ECData
!
!
!==============================================================================
! connection subroutines
!==============================================================================
function ecdataECConnectionInit(ECData, ECConnectionId) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)             :: ECData
  integer      , intent(in) :: ECConnectionId
  !
  ! locals
  !
  ! body
  success = init(ECData%connectionPtrs(ECConnectionId)%ECConnectionPtr)
end function ecdataECConnectionInit
!
!
!==============================================================================
function ecdataGetConnection(ECData, sourceItemId, targetItemId) result(id)
  !
  ! result: unique connection id or zero if it does not exist
  integer :: id
  !
  ! arguments
  type(tECData)              :: ECData
  integer      , intent(in)  :: targetItemId
  integer      , intent(in)  :: sourceItemId
  !
  ! locals
  type(tECItem), pointer :: sourceItem
  type(tECItem), pointer :: targetItem
  !
  ! body
  sourceItem => ECData%ECItemPtrs(sourceItemId)%ECItemPtr
  targetItem => ECData%ECItemPtrs(targetItemId)%ECItemPtr
  id = getConnection(ECData%connectionPtrs, sourceItem, targetItem)
end function ecdataGetConnection
!
!
!==============================================================================
function ecdataAddConnection(ECData) result(id)
  !
  ! result: unique new connection id
  integer :: id
  !
  ! arguments
  type(tECData) :: ECData
  !
  ! locals
  !
  ! body
  id = addConnection(ECData%connectionPtrs)
end function ecdataAddConnection
!
!
!==============================================================================
function ecdataAddConnection_ids(ECData, sourceItemId, targetItemId) result(id)
  !
  ! result: unique new connection id
  integer :: id
  !
  ! arguments
  type(tECData)              :: ECData
  integer      , intent(in)  :: targetItemId
  integer      , intent(in)  :: sourceItemId
  !
  ! locals
  type(tECItem), pointer :: sourceItem
  type(tECItem), pointer :: targetItem
  !
  ! body
  sourceItem => ECData%ECItemPtrs(sourceItemId)%ECItemPtr
  targetItem => ECData%ECItemPtrs(targetItemId)%ECItemPtr
  id = addConnection(ECData%connectionPtrs, sourceItem, targetItem)
end function ecdataAddConnection_ids
!
!
!==============================================================================
function ecdataECConnectionAddSource(ECData, ECConnectionId, ECItemId) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)             :: ECData
  integer      , intent(in) :: ECConnectionId
  integer      , intent(in) :: ECItemId
  !
  ! locals
  type(tECItem)      , pointer :: ECItemPtr
  type(tECConnection), pointer :: connectionPtr
  !
  ! body
  ECItemPtr     => ECData%ECItemPtrs(ECItemId)%ECItemPtr
  connectionPtr => ECData%connectionPtrs(ECConnectionId)%ECConnectionPtr
  success       =  ECConnectionAddSource(connectionPtr, ECItemPtr)
end function ecdataECConnectionAddSource
!
!
!==============================================================================
function ecdataECConnectionAddTarget(ECData, ECConnectionId, ECItemId) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECData)             :: ECData
  integer      , intent(in) :: ECConnectionId
  integer      , intent(in) :: ECItemId
  !
  ! locals
  type(tECItem)      , pointer :: ECItemPtr
  type(tECConnection), pointer :: connectionPtr
  !
  ! body
  ECItemPtr     => ECData%ECItemPtrs(ECItemId)%ECItemPtr
  connectionPtr => ECData%connectionPtrs(ECConnectionId)%ECConnectionPtr
  success       =  ECConnectionAddTarget(connectionPtr, ECItemPtr)
end function ecdataECConnectionAddTarget
!
!
!==============================================================================
function getInternalGrid(ECData) result(id)
  !
  ! result: ID of the internal grid
  integer :: id
  !
  ! arguments
  type(tECData) :: ECData
  !
  ! body
  id = ECData%internalGrid
end function getInternalGrid



end module ec_data
