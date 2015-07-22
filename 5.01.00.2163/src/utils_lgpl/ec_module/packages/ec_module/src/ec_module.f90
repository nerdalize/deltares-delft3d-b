module ec_module
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
!  $Id: ec_module.f90 1848 2012-09-14 17:42:05Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_module.f90 $
!!--description-----------------------------------------------------------------
!
! HTML DOC START
!
! Dependencies:
!  kernel
!  |- ec_module
!     |- ec_converter
!        |- ec_provider
!           |- ec_provider_read
!              |- ec_data
!                 |- ec_connection
!                    |- ec_ecitem
!                       |- ec_quantity
!                       |- ec_elementset
!                       |- ec_field
!                          |- ec_parameters
!                             |- ec_message
!                             |- precision
!
! <TABLE>
!   <TR>
!     <TD colSpan=2>
!            Module: KERNEL
!     </TD>
!     <TD colSpan=2>
!            Description:
!              This is the module using the EC-module
!              All calls from the kernel go via ec_module and have an ECHandle as parameter
!            More information: http://wiki.deltares.nl/display/FLOW/EC-Module
!     </TD>
!   </TR>
!   <TR>
!     <TD>
!            Dependencies:
!              use ec_module
!     </TD>
!     <TD>
!            Data:
!              type(tECHandle) :: ECHandle
!              logical         :: success
!     </TD>
!   </TR>
!   <TR>
!     <TD colSpan=2>
!            Typical calls:
!              success = create(ECHandle, private)
!              gridId  = addElementSet(ECHandle, x, y, kcs, sferic)
!              success = setInternalGrid(ECHandle, gridId)
!              success = ECCheck(ECHandle, endTime)
!              qId     = addQuantity(ECHandle, 'air_pressure')
!              itemId  = addECItem(ECHandle, qId, gridId)
!              success = addECItemField(ECHandle, itemId, parray)
!
!              provId  = addProvider(ECHandle, provType_file, filename, provFile_unimagdir)
!              success = initProvider(ECHandle, provId)
!
!              success = getVal(ECHandle, itemId , curtim, parray)
!
!              success = free(ECHandle)
!              call checkResult(ECHandle, success)
!     </TD>
!   </TR>
! </TABLE>
! <TABLE>
!   <TR>
!     <TD colSpan=2>
!            Module: EC_MODULE
!     </TD>
!     <TD colSpan=2>
!            Description:
!              Top level (Fortran-)module of the EC-module.
!              This is the only module taking care of ECHandles.
!              All calls from the kernel to the EC-module go via this module.
!            More information: http://wiki.deltares.nl/display/FLOW/EC-Module
!     </TD>
!   </TR>
!   <TR>
!     <TD>
!            Dependencies:
!              use ec_data
!              use ec_converter
!              use ec_provider
!     </TD>
!     <TD>
!            Data:
!              type tEC
!                type(tECData)                   , pointer :: ECData
!                type(tECConverter), dimension(:), pointer :: converters
!                type(tECProvider) , dimension(:), pointer :: providers
!              end type tEC
!
!              type tECHandle
!                type(tEC), pointer       :: fields
!              end type
!
!              type(tEC)  , pointer, save :: public_fields
!     </TD>
!   </TR>
!   <TR>
!     <TD colSpan=2>
!            Typical calls:
!              function ECCheck(ECHandle, endTime) result(success)
!                do i=1, size(ECHandle%fields%converters)
!                  collect all contibuting providerIds
!                  do for all targetItems in this converter
!                    add this converter to the targetItems converterIds
!                    add all contributing providerIds to the targetItems providerIds
!                  enddo
!                enddo
!                do for all targetItems
!                  success = getVal(ECHandle, targetItem, endTime)
!                enddo
!                do for all Items
!                  success = ECRewind(item)
!                enddo
!                do for all Providers
!                  success = ECRewind(provider)
!                enddo
!              end function ECCheck
!
!              function getVal(ECHandle, ECItemId, curtim) result(success)
!                if (item%time == curtim) return
!                do for all providers in items providerIds
!                  success = ECUpdate(provider, curtim)
!                enddo
!                do for all converters in items converterIds
!                  success = ECConvert(converter, curtim)
!                enddo
!              end function getVal
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
  use ec_message
  use ec_ecitem
  use ec_data
  use ec_converter
  use ec_provider
  !
  implicit none
  !
  private
!
! parameters
!
!
  type(tEC), pointer, save :: public_fields => null()
!
! interfaces
!
  interface free
    module procedure free_EC
  end interface
  interface isNull
    module procedure isNull_EC
  end interface
  interface create
    module procedure create_EC
  end interface
  interface copyArray
    module procedure copyArray_providers
  end interface
  interface addECItem
    module procedure addECItem_viaHandle
  end interface
  interface addElementSet
    module procedure addElementSet_components_viaHandle
    module procedure addElementSet_dims
    module procedure addElementSet_IDbased
  end interface
  interface addQuantity
    module procedure addQuantity_components_viaHandle
  end interface
  interface addECItemField
    module procedure addECItemField_ref_viaHandle
    module procedure addECItemField_ref2d_viaHandle
  end interface
  interface getVal
    module procedure getECValue
    module procedure getECValue_array
    module procedure getECValue_array_dims
    module procedure getECValue_2darray
  end interface
  interface addProvider
    module procedure ecAddProvider
    module procedure ecAddProvider_withString
  end interface
  interface putValues
    module procedure putValues1
    module procedure putValues2
  end interface
  interface getConnections
    module procedure getECConnections
  end interface
  interface addConverters
    module procedure addECConverters
  end interface
  interface increaseSize
    module procedure ecmoduleIncreaseSize_converters
  end interface
  interface ECCheck
    module procedure ECCheck
  end interface
  interface ECItemSetType
    module procedure ecmoduleECItemSetType
  end interface
!
! public entities
!
  public :: tECHandle
  public :: getECMessage
  public :: maxMessageLen
  public :: free
  public :: isNull
  public :: create
  public :: addECItem
  public :: addElementSet
  public :: addQuantity
  public :: addECItemField
  public :: setInternalGrid
  public :: getVal
  public :: addProvider
  public :: initProvider
  public :: putValues
  public :: provType_default
  public :: provType_file
  public :: provType_PutById
  public :: getConnections
  public :: ECCheck
  public :: ECItemSetType
  public :: ECDump
  public :: checkResult
  !
  public :: provFile_undefined
  public :: provFile_uniform
  public :: provFile_unimagdir
  public :: provFile_svwp
  public :: provFile_arcinfo
  public :: provFile_spiderweb
  public :: provFile_curvi
  public :: provFile_triangulation
  public :: provFile_triangulationmagdir
  public :: provFile_poly_tim
  public :: provFile_fourier
  public :: provFile_grib
  public :: provFile_table
contains
!
!
!==============================================================================
! EC subroutines
!==============================================================================
function free_EC(handle) result(success)
  !
  ! result
  logical :: success
  !
  ! parameters
  type(tECHandle) :: handle
  !
  ! locals
  integer            :: i
  integer            :: istat
  type(tEC), pointer :: this
  !
  ! body
  success = .true.
  if (isNull(handle)) then
     !
     ! Nothing to free
     !
  else
     this => handle%fields
     !
     success = free(this%ECData)
     success = free(this%converters)
     success = free(this%providers)
     ! Do not use the "this" pointer here
     deallocate(handle%fields, STAT = istat)
     nullify(handle%fields)
  endif
  !
  ! This is not the right place to free public_fields
  ! But, as long as it is not seriously used....
  !
  if (associated(public_fields)) then
     this => public_fields
     !
     success = free(this%ECData)
     success = free(this%converters)
     success = free(this%providers)
     ! Do not use the "this" pointer here
     deallocate(public_fields, STAT = istat)
     nullify(public_fields)
  endif
end function free_EC
!
!
!==============================================================================
function isNull_EC(handle) result(res)
  !
  ! result
  logical :: res
  !
  ! parameters
  type(tECHandle) :: handle
  !
  ! body
  res = .NOT. associated(handle%fields)
end function isNull_EC
!
!
!==============================================================================
function create_EC(ECHandle, private) result(success)
  !
  ! result
  logical :: success
  !
  ! parameters
  type(tECHandle)  , intent(out) :: ECHandle
  logical, optional, intent(in)  :: private
  !
  ! locals
  integer :: istat
  logical :: private_
  !
  ! body
  ! AM: make threadsafe
  if ( .not. associated(public_fields) ) then
    allocate(public_fields, STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      call setECMessage("ERROR: ec_module::create_EC: Unable to allocate additional memory (public_fields)")
      return
    endif
    success = init_EC(public_fields)
    if (.not. success) return
  endif
  private_ = .true.
  if ( present(private) ) then
    private_ = private
  endif
  if ( private_ ) then
    allocate(ECHandle%fields, STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      call setECMessage("ERROR: ec_module::create_EC: Unable to allocate additional memory (fields)")
      return
    endif
    success = init_EC(ECHandle%fields)
    if (.not. success) return
  else
    ECHandle%fields => public_fields
    success = .true.
  endif
end function create_EC
!
!
!==============================================================================
function init_EC(fields) result(success)
  !
  ! result
  logical :: success
  !
  ! parameters
  type(tEC), pointer :: fields
  !
  ! locals
  integer :: istat
  !
  ! body
  success = .false.
                  allocate(fields%ecData       , STAT = istat)
  if (istat == 0) allocate(fields%converters(0), STAT = istat)
  if (istat == 0) allocate(fields%providers(0) , STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_module::init_EC: Unable to allocate additional memory")
    return
  endif
  success = initECParameters()
  if (.not. success) return
  success = init(fields%ECData)
end function init_EC
!
!
!==============================================================================
function setInternalGrid(ECHandle, gridId) result(success)
   !
   ! result
   logical :: success
   !
   ! parameters
   type(tECHandle)             :: ECHandle
   integer        , intent(in) :: gridId
   !
   ! body
   ! TODO: check that the grid ID exists
   success = ecdataSetInternalGrid(ECHandle%fields%ECData, gridId)
end function setInternalGrid
!
!
!==============================================================================
function ECCheck(ECHandle, endTime) result(success)
  !
  ! result
  logical :: success
  !
  ! parameters
  type(tECHandle)             :: ECHandle
  real(hp)       , intent(in) :: endTime
  !
  ! locals
  integer                        :: i
  integer                        :: istat
  integer                        :: j
  integer                        :: k
  integer                        :: m
  integer, dimension(:), pointer :: provIdArr
  integer                        :: itemId
  logical                        :: retVal
  type(tECConnection)  , pointer :: aConnection
  !
  ! body
  success = .true.
  !
  ! For each converter:
  ! - Add all related providerIds to all targetItems inside the converter
  ! - Add the converterId to all targetItems inside the converter
  !
  do i=1, size(ECHandle%fields%converters)
    !
    ! Loop over the sourceItems
    ! Collect all related providerIds in provIdArr
    !
    do j=1, size(ECHandle%fields%converters(i)%connections)
      aConnection => ECHandle%fields%converters(i)%connections(j)%ECConnectionPtr
      do k=1, size(aConnection%sourceItems)
        call addUniqueInt(provIdArr,aConnection%sourceItems(k)%ECItemPtr%providerIds(1))
      enddo
    enddo
    !
    ! Loop over the targetItems
    !
    do j=1, size(ECHandle%fields%converters(i)%connections)
      aConnection => ECHandle%fields%converters(i)%connections(j)%ECConnectionPtr
      do k=1, size(aConnection%targetItems)
        !
        ! Add the converterId to the targetItem
        !
        call addUniqueInt(aConnection%targetItems(k)%ECItemPtr%converterIds,ECHandle%fields%converters(i)%id)
        !
        ! Add provIdArr to all targetItems
        !
        do m=1, size(provIdArr)
          call addUniqueInt(aConnection%targetItems(k)%ECItemPtr%providerIds,provIdArr(m))
        enddo
      enddo
    enddo
    if (associated(provIdArr)) deallocate(provIdArr, STAT = istat)
  enddo
  !
  ! Now the EC-module is fully initialized
  !
  ECHandle%fields%initialized = .true.
  !
  ! Call getVal for each targetItem
  !
  do i=1, size(ECHandle%fields%ECData%connectionPtrs)
    do j=1, size(ECHandle%fields%ECData%connectionPtrs(i)%ECConnectionPtr%targetItems)
      itemId = ECHandle%fields%ECData%connectionPtrs(i)%ECConnectionPtr%targetItems(j)%ECItemPtr%id
      retVal = getVal(ECHandle, itemId, endTime)
      if (.not. retVal) success = .false.
    enddo
  enddo
  !
  ! Rewind all ECItems
  !
  do i=1, size(ECHandle%fields%ECData%ECItemPtrs)
    if (ECHandle%fields%providers(ECHandle%fields%ECData%ECItemPtrs(i)%ECItemPtr%providerIds(1))%type /= provType_default) then
      retVal = ECRewind(ECHandle%fields%ECData%ECItemPtrs(i)%ECItemPtr)
      if (.not. retVal) success = .false.
    endif
  enddo
  !
  ! Rewind all providers
  !
  do i=1, size(ECHandle%fields%providers)
    retVal = ECRewind(ECHandle%fields%providers(i))
    if (.not. retVal) success = .false.
  enddo
end function ECCheck
!
!
!==============================================================================
subroutine ECDump(ECHandle)
  !
  ! parameters
  type(tECHandle) :: ECHandle
  !
  ! locals
  integer :: i
  !
  ! body
  write(*,'(a)') '=============================================================='
  write(*,'(a)') '=== ECDUMP start . . .                                     ==='
  if (.not. ECHandle%fields%initialized) then
    write(*,'(a)') 'EC-module is not initialized'
  else
    write(*,'(a)') '===== converterters: ========================================='
    do i=1, size(ECHandle%fields%converters)
      call dump(ECHandle%fields%converters(i))
      write(*,'(a)') '  ----------------------------'
    enddo
    write(*,'(a)') '===== providers: ============================================='
    do i=1, size(ECHandle%fields%providers)
      call dump(ECHandle%fields%providers(i))
      write(*,'(a)') '  ----------------------------'
    enddo
    write(*,'(a)') '===== ECData: ================================================'
    call dump(ECHandle%fields%ECData)
  endif
  write(*,'(a)') '=== ECDUMP . . . end                                       ==='
  write(*,'(a)') '=============================================================='
end subroutine ECDump
!
!
!==============================================================================
! ECHandle -> ECData subroutines
!==============================================================================
function addQuantity_components_viaHandle(ECHandle, name) result(id)
  !
  ! result
  integer :: id
  !
  ! arguments
  type(tECHandle)             :: ECHandle
  character(*)   , intent(in) :: name
  !
  ! body
  id = addQuantity(ECHandle%fields%ECData, name)
end function addQuantity_components_viaHandle
!
!
!==============================================================================
function addElementSet_components_viaHandle(ECHandle, x, y, kcs, spherical) result(id)
  !
  ! result
  integer :: id
  !
  ! arguments
  type(tECHandle)                     :: ECHandle
  real(hp), dimension(:), intent(in)  :: x
  real(hp), dimension(:), intent(in)  :: y
  integer , dimension(:), intent(in)  :: kcs
  logical               , intent(in)  :: spherical
  !
  ! body
  id = addElementSet(ECHandle%fields%ECData, x, y, kcs, spherical)
end function addElementSet_components_viaHandle
!
function addElementSet_dims(ECHandle, x, y, kcs, spherical, nmax, mmax, mub, mlb) result(id)
  !
  ! result
  integer :: id
  !
  ! arguments
  integer                                  , intent(in)  :: mmax
  integer                                  , intent(in)  :: nmax
  integer                                  , intent(in)  :: mub
  integer                                  , intent(in)  :: mlb
  integer , dimension(nmax*(1 + mub - mlb)), intent(in)  :: kcs
  logical                                  , intent(in)  :: spherical
  real(fp), dimension(nmax*(1 + mub - mlb)), intent(in)  :: x
  real(fp), dimension(nmax*(1 + mub - mlb)), intent(in)  :: y
  type(tECHandle)                                        :: ECHandle
  !
  ! locals
  real(hp), dimension(:), pointer     :: xx, yy
  integer , dimension(:), pointer     :: mykcs
  integer                             :: i, j, istat
  !
  ! body
  !
  allocate(xx(nmax*mmax), yy(nmax*mmax), mykcs(nmax*mmax), STAT = istat)
  if (istat /= 0) then
    call setECMessage("ERROR: ec_module::addElementSet: Unable to allocate additional memory")
    id = 0
  else
    do j = 1, mmax
      do i = 1, nmax
        xx(i + (j-1)*nmax) = x(i+(j-mlb)*nmax)
        yy(i + (j-1)*nmax) = y(i+(j-mlb)*nmax)
        mykcs(i + (j-1)*nmax) = kcs(i+(j-mlb)*nmax)
      enddo
    enddo
    id = addElementSet(ECHandle%fields%ECData, xx, yy, mykcs, spherical)
  endif
end function addElementSet_dims
!
!
!==============================================================================
function addElementSet_IDbased(ECHandle, IDs) result(id)
  !
  ! result
  integer :: id
  !
  ! arguments
  type(tECHandle)                     :: ECHandle
  character(len=*), dimension(:), pointer :: IDs
  !
  ! body
  id = addElementSet(ECHandle%fields%ECData, IDs)
end function addElementSet_IDbased
!
!
!==============================================================================
function addECItem_viaHandle(ECHandle, qId, elSetId, name) result(itemId)
  !
  ! result
  integer :: itemId
  !
  ! arguments
  type(tECHandle)             :: ECHandle
  integer        , intent(in) :: qId
  integer        , intent(in) :: elSetId
  character(len=*), intent(in), optional :: name
  !
  ! locals
  !
  ! body
  if (present(name)) then
     itemId = addECItem_viaData(ECHandle%fields%ECData, qId, elSetId, name)
  else
     itemId = addECItem_viaData(ECHandle%fields%ECData, qId, elSetId)
  endif
end function addECItem_viaHandle
!
!
!==============================================================================
function addECItemField_ref_viaHandle(ECHandle, ECItemId, array) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)                    :: ECHandle
  integer               , intent(in) :: ECItemId
  real(fp), dimension(:), pointer    :: array
  !
  ! locals
  !
  ! body
  success = addECItemField(ECHandle%fields%ECData, ECItemId, array)
end function addECItemField_ref_viaHandle
!
!
!==============================================================================
function addECItemField_ref2d_viaHandle(ECHandle, ECItemId, array) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)                    :: ECHandle
  integer               , intent(in) :: ECItemId
  real(fp), dimension(:,:), pointer    :: array
  !
  ! locals
  !
  ! body
  success = addECItemField(ECHandle%fields%ECData, ECItemId, array)
end function addECItemField_ref2d_viaHandle
!
!
!==============================================================================
function getECValue(ECHandle, ECItemId, curtim) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)             :: ECHandle
  integer        , intent(in) :: ECItemId
  real(hp)       , intent(in) :: curtim
  !
  ! locals
  integer                      :: i
  logical                      :: retVal
  type(tECItem)      , pointer :: ecItem
  type(tECField)     , pointer :: field
  !
  ! body
  success = .true.
  ecItem => ECHandle%fields%ECData%ECItemPtrs(ECItemId)%ECItemPtr
  field  => ecItem%fields(ecItem%t0fieldId)
  if (field%time /= field%missingValue .and. comparereal(field%time,curtim) == 0) then
    !
    ! This item is up to date
    ! Probably because it has been updated at the same time as another item
    !
    return
  endif
  !
  ! Call ECUpdate for all providers involved
  !
  do i=1, size(ecItem%providerIds)
    retVal = ECUpdate(ECHandle%fields%providers(ecItem%providerIds(i)), curtim)
    if (.not. retVal) success = .false.
  enddo
  !
  ! Call ECConvert for all converters involved
  !
  do i=1, size(ecItem%converterIds)
    retVal = ECConvert(ECHandle%fields%converters(ecItem%converterIds(i)), curtim)
    if (.not. retVal) success = .false.
  enddo
end function getECValue
!
!
!==============================================================================
function getECValue_array(ECHandle, ECItemId, curtim, targetArray) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)                    :: ECHandle
  integer               , intent(in) :: ECItemId
  real(hp)              , intent(in) :: curtim
  real(fp), dimension(:), pointer    :: targetArray
  !
  ! locals
  integer                            :: istat
  type(tECItem), pointer             :: pECItem
  real(fp), dimension(:), pointer    :: arr1D
  !
  ! body
  !
  success = getECValue(ECHandle, ECItemId, curtim)
  !
  pECItem     => ECHandle%Fields%ECData%ECItemPtrs(ECItemId)%ECItemptr
  arr1D       => pECItem%fields(0)%Arr1D
  if (.not. associated(targetArray, arr1D)) then
     deallocate(targetArray, STAT = istat)
     targetArray => arr1D
  endif
end function getECValue_array
!
!==============================================================================
function getECValue_array_dims(ECHandle, ECItemId, curtim, targetArray, nlb, nub, mlb, mub) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)                     :: ECHandle
  integer               , intent(in)  :: ECItemId, nlb, nub, mlb, mub
  real(hp)              , intent(in)  :: curtim
  real(fp), dimension(nlb:nub, mlb:mub), intent(out) :: targetArray
  !
  ! locals
  integer                             :: i, j, nmax, mmax, dim1, dim2
  type(tECItem), pointer              :: pECItem
  real(fp), dimension(:,:), pointer   :: arr2D
  !
  ! body
  !
  success = getECValue(ECHandle, ECItemId, curtim)
  !
  pECItem     => ECHandle%Fields%ECData%ECItemPtrs(ECItemId)%ECItemptr
  arr2D       => pECItem%fields(0)%Arr2D
  mmax = size(arr2D,2)
  nmax = size(arr2D,1)
  dim1 = min(nmax, nub)
  dim2 = min(mmax, mub)
  do j = 1, dim2
     do i = 1, dim1
        targetArray(i,j) = arr2D(i,j)
     enddo
  enddo
end function getECValue_array_dims
!
!==============================================================================
function getECValue_2darray(ECHandle, ECItemId, curtim, targetArray) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)                    :: ECHandle
  integer               , intent(in) :: ECItemId
  real(hp)              , intent(in) :: curtim
  real(fp), dimension(:,:), pointer  :: targetArray
  !
  ! locals
  integer                            :: istat
  type(tECItem), pointer             :: pECItem
  real(fp), dimension(:,:), pointer  :: arr2D
  !
  ! body
  !
  success = getECValue(ECHandle, ECItemId, curtim)
 
  pECItem     => ECHandle%Fields%ECData%ECItemPtrs(ECItemId)%ECItemptr
  arr2D       => pECItem%fields(0)%Arr2D
  if (.not. associated(targetArray, arr2D)) then
     write (*,*) 'TargetArray has changed (unexpected)'
     deallocate(targetArray, STAT = istat)
     targetArray => arr2D
  endif
end function getECValue_2darray
!
!
!==============================================================================
function ecmoduleECItemSetType(ECHandle, itemId, aType) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)             :: ECHandle
  integer        , intent(in) :: itemId
  integer        , intent(in) :: aType
  !
  ! locals
  !
  ! body
  success = ECItemSetType(ECHandle%fields%ECData, itemId, aType)
end function ecmoduleECItemSetType
!
!
!==============================================================================
! converter subroutines
!==============================================================================
function ecmoduleIncreaseSize_converters(ECHandle) result (k)
  !
  ! result
  integer :: k
  !
  ! arguments
  type(tECHandle) :: ECHandle
  !
  ! locals
  !
  ! body
  k = increaseSize(ECHandle%fields%converters)
end function ecmoduleIncreaseSize_converters
!
!
!==============================================================================
! Provider subroutines
!==============================================================================
function ecAddProvider(ECHandle, provType) result(providerId)
   !
   ! result: unique provider id
   integer :: providerId
   !
   ! arguments
   type(tECHandle)  :: ECHandle
   integer          :: provType
   !
   ! locals
   logical          :: success
   !
   ! body
   providerId = increaseSize_providers(ECHandle)
   success    = new_provider(ECHandle%fields%providers(providerId), ECHandle%fields%ECData, &
                   & provType)
   if (.not. success) providerId=0
end function ecAddProvider
!
!
!==============================================================================
function ecAddProvider_withString(ECHandle, provType, iniString, provFileType) result(providerId)
   !
   ! result: unique provider id
   integer :: providerId
   !
   ! arguments
   type(tECHandle)             :: ECHandle
   integer        , intent(in) :: provType
   character(*)                :: iniString ! provType = file: iniString = filename
   integer        , intent(in) :: provFileType
   !
   ! locals
   logical :: success
   !
   ! body
   providerId = increaseSize_providers(ECHandle)
   success    = new_provider(ECHandle%fields%providers(providerId), ECHandle%fields%ECData, &
                   & provType, iniString, provFileType)
   if (.not. success) providerId=0
end function ecAddProvider_withString
!
!
!==============================================================================
function increaseSize_providers(ECHandle) result (k)
  !
  ! result
  integer :: i
  integer :: k
  !
  ! arguments
  type(tECHandle) :: ECHandle
  !
  ! locals
  integer                                  :: istat
  logical                                  :: success
  type(tECProvider), dimension(:), pointer :: newProviders
  !
  ! body
  k = size(ECHandle%fields%providers) + 1
  allocate(newProviders(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    k       = 0
    call setECMessage("ERROR: ec_module::increaseSize_providers: Unable to allocate additional memory")
    return
  endif
  do i=1, k
    success = new_provider(newProviders(i), i)
    if (.not. success) k=0
  enddo
  success = copyArray(ECHandle%fields%providers,newProviders)
  if (.not. success) k=0
  success = free(ECHandle%fields%providers)
  if (.not. success) k=0
  ECHandle%fields%providers => newProviders
end function increaseSize_providers
!
!
!==============================================================================
function copyArray_providers(sourceArr, targetArr) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider), dimension(:), pointer :: sourceArr
  type(tECProvider), dimension(:), pointer :: targetArr
  !
  ! locals
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
     retVal = copy(sourceArr(k), targetArr(k))
     if (.not. retVal) success=.false.
  enddo
  do k = arrLenSource+1, arrLenTarget
     retVal = new_provider(targetArr(k), k)
     if (.not. retVal) success=.false.
  enddo
end function copyArray_providers
!
!
!==============================================================================
function initProvider(ECHandle, provId, qName, qValue, gridECItemId) result (success)
  !
  ! result
  logical                     :: success
  !
  ! arguments
  type(tECHandle)             :: ECHandle
  integer        , intent(in) :: provId
  integer        , intent(in), optional :: gridECItemId
  character(*)   , intent(in), optional :: qName
  real(fp)       , intent(in), optional :: qValue
  !
  ! locals
  integer, dimension(:), pointer :: connectionIds
  integer                        :: l_gridECItemId
  real(fp)                       :: l_qValue
  !
  ! body
  !
  ! handle optional arguments
  !
  if (present(gridECItemId)) then
      l_gridECItemId = gridECItemId
  else
      l_gridECItemId = -1
  endif
  if (present(qValue)) then
      l_qValue = qValue
  else
      l_qValue = -1
  endif
  !
  if (present(qName)) then
      success = init_ec_provider(ECHandle%fields%providers(provId), ECHandle%fields%ECData, qName, l_qValue, l_gridECItemId)
  else
      success = init_ec_provider(ECHandle%fields%providers(provId), ECHandle%fields%ECData, ' '  , l_qValue, l_gridECItemId)
  endif
  !
  if (.not. success) return
  success = getConnectionsOfProvider(ECHandle%fields%providers(provId), &
          & ECHandle%fields%ECData, connectionIds)
  if (.not. success) return
  success = addConverters(ECHandle%fields%converters, ECHandle%fields%ECData, &
          & ECHandle%fields%providers(provId), connectionIds)
end function initProvider
!
!==============================================================================
function putValues1(ECHandle, provId, curtim, val) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)                 :: ECHandle
  integer        , intent(in)     :: provId
  real(hp)       , intent(in)     :: curtim
  real(hp), dimension(:), pointer :: val
  !
  ! locals
  type(tECProvider)             :: provider
  !
  ! body
  provider = ECHandle%fields%providers(provId)
  success  = ECUpdate(provider, curtim, val)
end function putValues1
!
!==============================================================================
function putValues2(ECHandle, provId, curtim, val) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)                 :: ECHandle
  integer        , intent(in)     :: provId
  real(hp)       , intent(in)     :: curtim
  real(sp), dimension(:), pointer :: val
  !
  ! locals
  type(tECProvider)               :: provider
  real(hp), dimension(:), pointer :: valcopy
  integer                         :: istat
  !
  ! body
  allocate(valcopy(size(val)), STAT = istat)
  if (istat == 0) then
     valcopy  = val
     provider = ECHandle%fields%providers(provId)
     success  = ECUpdate(provider, curtim, valcopy)
     deallocate(valcopy)
  else
     success = .false.
     call setECMessage("ERROR: ec_module::putvalues: Unable to allocate memory")
  endif
end function putValues2
!
!
!==============================================================================
function getECConnections(ECHandle, provId, connectionIds) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)                   :: ECHandle
  integer              , intent(in) :: provId
  integer, dimension(:), pointer    :: connectionIds
  !
  ! body
  success = getConnectionsOfProvider(ECHandle%fields%providers(provId), ECHandle%fields%ECData, connectionIds)
end function getECConnections
!
!
!==============================================================================
function addECConverters(ECHandle, providerId, connectionIds) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECHandle)                   :: ECHandle
  integer              , intent(in) :: providerId
  integer, dimension(:), pointer    :: connectionIds
  !
  ! body
  success = addConverters(ECHandle%fields%converters, ECHandle%fields%ECData, &
                        & ECHandle%fields%providers(providerId), connectionIds)
end function addECConverters
!
!==============================================================================
subroutine checkResult(ECHandle, success)
  implicit none
  !
  ! arguments
  type(tECHandle), intent(in)           :: ECHandle
  logical        , intent(in), optional :: success
  !
  ! locals
  character(maxMessageLen) :: message
  logical                  :: localSuccess
  !
  ! body
  if (present(success)) then
    localSuccess = success
  else
    !
    ! checkResult is called, knowing there is something wrong
    !
    localSuccess = .false.
  endif
  if (.not. localSuccess) then
    message = getECMessage()
    write(*,*) "ERROR in EC-module: ", trim(message)
    stop 1
  endif
end subroutine checkResult
!
!==============================================================================
!
end module ec_module
