module ec_converter
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
!  $Id: ec_converter.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_converter.f90 $
!!--description-----------------------------------------------------------------
!
! HTML DOC START
!
! <TABLE>
!   <TR>
!     <TD colSpan=2>
!            Module: ec_converter
!     </TD>
!     <TD colSpan=2>
!            Description:
!              Implementation for each type of converter.
!              Provider information is needed to set the type of the converter.
!            More information: http://wiki.deltares.nl/display/FLOW/EC-Module
!     </TD>
!   </TR>
!   <TR>
!     <TD>
!            Dependencies:
!              use ec_data
!              use ec_field
!              use ec_provider
!     </TD>
!     <TD>
!            Data:
!              type tECConverter
!                integer                                       :: id
!                integer                                       :: type
!                type(tECConnectionPtr), dimension(:), pointer :: connections
!                type(tIndexWeight)                  , pointer :: indexWeightId
!                integer                                       :: interpolationMethod
!                integer                                       :: operand
!                integer                                       :: method
!                integer                                       :: iweight
!              end type tECConverter
!     </TD>
!   </TR>
!   <TR>
!     <TD colSpan=2>
!            Typical calls:
!              function convert(converter, curtim) result(success)
!                select case (converter%type)
!                  case (convType_default)
!                    success = convert_default(converter)
!                  case (convType_unimagdir)
!                    success = convert_unimagdir(converter, curtim)
!                  case default
!                    call setECMessage('ERROR')
!                end select
!              end function convert
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
  use ec_data
  use ec_field
  use ec_provider
  !
  implicit none
  !
  ! private
!
! parameters
!
  ! enumeration for interpolation types
  integer, parameter :: interpolate_timeNever         = 0
  integer, parameter :: interpolate_timeAlways        = 1
  integer, parameter :: interpolate_spaceTimeAlways   = 2
  integer, parameter :: interpolate_saveWeightFactors = 3
  !
  ! enumeration for operand types
  integer, parameter :: operand_add        = 1
  integer, parameter :: operand_replace    = 2
  !
  ! enumeration for converter types
  integer, parameter :: convType_undefined = 0
  integer, parameter :: convType_default   = 1
  integer, parameter :: convType_unimagdir = 2
  integer, parameter :: convType_svwp      = 3
  integer, parameter :: convType_grib      = 4
  integer, parameter :: convType_ById      = 5
  !
  ! externals
  !integer, external :: compare_fp
!
!
! interfaces
!
  interface free
    module procedure free_converter
    module procedure free_converter_array
  end interface
  interface init
    module procedure init_converter
  end interface
  interface copy
    module procedure copy_converter
  end interface
  interface ECConvert
    module procedure convert
  end interface
  interface increaseSize
    module procedure increaseSize_converters
  end interface
  interface addConverters
    module procedure addConverters
  end interface
  interface dump
    module procedure dump_converter
  end interface
!
! public entities
!
  public :: addConverters
  public :: ECConvert
  public :: free_converter_array
  public :: tECConverter
  public :: dump
contains
!
!
!==============================================================================
! Converter subroutines
!==============================================================================
function free_converter(converter) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter) :: converter
  !
  ! locals
  integer :: istat
  integer :: k
  !
  ! body
  if (associated(converter%connections)  ) deallocate(converter%connections  , STAT = istat)
  if (associated(converter%indexWeightId)) deallocate(converter%indexWeightId, STAT = istat)
  success = .true.
end function free_converter
!
!
!==============================================================================
function free_converter_array(converter_array) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter), dimension(:), pointer :: converter_array
  !
  ! locals
  integer :: istat
  integer :: k
  !
  ! body
  do k = 1,size(converter_array)
    success = free(converter_array(k))
  enddo
  if (associated(converter_array)) deallocate(converter_array, STAT = istat)
  success = .true.
end function free_converter_array
!
!
!==============================================================================
function init_converter(converter) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter) :: converter
  !
  ! locals
  integer :: istat
  !
  ! body
  converter%id                  = 0
  converter%type                = convType_undefined
  converter%interpolationMethod = 0
  converter%operand             = 0
  converter%method              = 0
  converter%iweight             = 0
  allocate(converter%connections(0), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_converter::init_converter: Unable to allocate additional memory")
    return
  endif
  nullify(converter%indexWeightId)
end function init_converter
!
!
!==============================================================================
function increaseSize_converters(converters) result (k)
  !
  ! result
  integer :: i
  integer :: k
  !
  ! arguments
  type(tECConverter), dimension(:), pointer :: converters
  !
  ! locals
  integer                                   :: istat
  logical                                   :: success
  type(tECConverter), dimension(:), pointer :: newConverters
  !
  ! body
  k = size(converters) + 1
  allocate(newConverters(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_converter::increaseSize_converters: Unable to allocate additional memory")
    return
  endif
  do i=1,k
    success = init(newConverters(i))
    if (.not. success) k=0
  enddo
  success = copyArray_converters(converters, newConverters)
  if (.not. success) k=0
  success = free(converters)
  converters => newConverters
end function increaseSize_converters
!
!
!==============================================================================
function copyArray_converters(sourceArr, targetArr) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter), dimension(:), pointer :: sourceArr
  type(tECConverter), dimension(:), pointer :: targetArr
  !
  ! locals
  integer :: i
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
    retVal = copy_converter(sourceArr(k), targetArr(k))
    if (.not. retVal) success = retVal
  enddo
  do k = arrLenSource+1, arrLenTarget
    retVal = init_converter(targetArr(k))
    if (.not. retVal) success = retVal
  enddo
end function copyArray_converters
!
!
!==============================================================================
function copy_converter(sourceC,targetC) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter) :: sourceC
  type(tECConverter) :: targetC
  !
  ! locals
  integer :: i
  integer :: istat
  !
  ! body
  targetC%id   = sourceC%id
  targetC%type = sourceC%type
  if (associated(targetC%connections)) deallocate(targetC%connections, STAT = istat)
  allocate(targetC%connections(size(sourceC%connections)), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_converter::copy_converter: Unable to allocate additional memory (connections)")
    return
  endif
  do i=1,size(targetC%connections)
    targetC%connections(i)%ECConnectionPtr => sourceC%connections(i)%ECConnectionPtr
  enddo
  if (associated(targetC%indexWeightId)) deallocate(targetC%indexWeightId, STAT = istat)
  if (associated(sourceC%indexWeightId)) then
    allocate(targetC%indexWeightId, STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      call setECMessage("ERROR: ec_converter::copy_converter: Unable to allocate additional memory (indexWeightId)")
      return
    endif
    targetC%indexWeightId = sourceC%indexWeightId
  endif
  targetC%interpolationMethod = sourceC%interpolationMethod
  targetC%operand             = sourceC%operand
  targetC%method              = sourceC%method
  targetC%iweight             = sourceC%iweight
  success                     = .true.
end function copy_converter
!
!
!==============================================================================
function ECConverterIncreaseSize_connections(converter) result (k)
  !
  ! result
  integer :: k
  !
  ! arguments
  type(tECConverter) :: converter
  !
  ! locals
  integer                                       :: istat
  logical                                       :: success
  type(tECConnectionPtr), dimension(:), pointer :: newConnections
  !
  ! body
  k = size(converter%connections) + 1
  allocate(newConnections(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_converter::ECConverterIncreaseSize_connections: Unable to allocate additional memory")
    return
  endif
  success = ECConverterCopyArray_connections(converter, converter%connections, newConnections)
  if (.not. success) k=0
  deallocate (converter%connections, STAT = istat)
  converter%connections => newConnections
end function ECConverterIncreaseSize_connections
!
!
!==============================================================================
function ECConverterCopyArray_connections(converter, sourceArr, targetArr) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter)                            :: converter
  type(tECConnectionPtr), dimension(:), pointer :: sourceArr
  type(tECConnectionPtr), dimension(:), pointer :: targetArr
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
     targetArr(k)%ECConnectionPtr => sourceArr(k)%ECConnectionPtr
  enddo
  do k = arrLenSource+1, arrLenTarget
     nullify(targetArr(k)%ECConnectionPtr)
  enddo
  success = .true.
end function ECConverterCopyArray_connections
!
!
!==============================================================================
function addConverters(converters, ECData, provider, connectionIds) result(success)
  !
  ! Add a new converter (when needed)
  ! For each cId in connectionIds:
  !   Add the corresponding connectionPointer to the new converter
  !   Set the connection's converterId to the new converter's Id
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter), dimension(:), pointer :: converters
  type(tECData)                             :: ECData
  type(tECProvider)                         :: provider
  integer, dimension(:)           , pointer :: connectionIds
  !
  ! locals
  type(tECConnection), pointer :: connectionptr
  integer :: i
  integer :: j
  integer :: convId
  !
  ! body
  success = .false.
  ! TODO: is there already a unimagdir converter that can be used?
  convId = increaseSize_converters(converters)
  converters(convId)%id = convId
  select case (provider%type)
    case (provType_default)
      converters(convId)%type    = convType_default
    case (provType_file)
      select case (provider%filetype)
        case (provFile_unimagdir)
          converters(convId)%type    = convType_unimagdir
          converters(convId)%method  = interpolate_timeAlways
          converters(convId)%operand = operand_replace
        case (provFile_svwp)
          converters(convId)%type    = convType_svwp
          converters(convId)%method  = interpolate_timeAlways
          converters(convId)%operand = operand_replace         ! ejs todo: nodig?
        case (provFile_grib)
          converters(convId)%type    = convType_grib
          converters(convId)%method  = interpolate_timeAlways
          converters(convId)%operand = operand_replace
        case default
          call setECMessage("ERROR: Undefined provider filetype in ec_converter::addConverters")
          return
      end select
    case (provType_PutById)
      converters(convId)%type    = convType_ById
      converters(convId)%method  = interpolate_timeAlways
      converters(convId)%operand = operand_replace
    case default
      call setECMessage("ERROR: Undefined provider type in ec_converter::addConverters")
      return
  end select
  do i=1, size(connectionIds)
    j = ECConverterIncreaseSize_connections(converters(convId))
    connectionptr => ECData%connectionPtrs(connectionIds(i))%ECConnectionPtr
    converters(convId)%connections(j)%ECConnectionPtr => connectionptr
    connectionptr%converterId = convId
  enddo
  success = .true.
end function addConverters
!
!
!==============================================================================
subroutine dump_converter(converter)
  !
  ! arguments
  type(tECConverter) :: converter
  !
  ! locals
  integer :: i
  !
  ! body
  write (*,'(a,i0)') '  id                 : ', converter%id
  write (*,'(a,i0)') '  type               : ', converter%type
  write (*,'(a,i0)') '  interpolationMethod: ', converter%interpolationMethod
  write (*,'(a,i0)') '  operand            : ', converter%operand
  write (*,'(a,i0)') '  method             : ', converter%method
  write (*,'(a,i0)') '  iweight            : ', converter%iweight
  do i=1, size(converter%connections)
    write (*,'(a,i0)') '  connection id      : ', converter%connections(i)%ECConnectionPtr%id
  enddo
end subroutine dump_converter
!
!
!==============================================================================
function convert(converter, curtim) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter)             :: converter
  real(hp)          , intent(in) :: curtim
  !
  ! locals
  integer :: i
  integer :: j
  integer :: convId
  !
  ! body
  success = .false.
  select case (converter%type)
    case (convType_default)
      success = convert_default(converter)
    case (convType_unimagdir)
      success = convert_unimagdir(converter, curtim)
    case (convType_svwp)
      success = convert_svwp(converter, curtim)
    case (convType_grib)
      success = convert_grib(converter, curtim)
    case (convType_ById)
      success = convert_byId(converter, curtim)
    case default
      call setECMessage("ERROR: Undefined converter type in ec_converter::convert")
  end select
end function convert
!
!
!==============================================================================
function convert_default(converter) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter) :: converter
  !
  ! locals
  integer                      :: i
  integer                      :: j
  integer                      :: k
  type(tECConnection), pointer :: connection
  type(tECField)     , pointer :: field
  logical                      :: retVal
  !
  ! body
  success = .true.
  do i=1, size(converter%connections)
    connection => converter%connections(i)%ECConnectionPtr
    do j=1, size(connection%targetItems)
      do k=0, size(connection%targetItems(j)%ECItemPtr%fields) - 1
        field => connection%targetItems(j)%ECItemPtr%fields(k)
        retVal = setField(field, connection%sourceItems(1)%ECItemPtr%fields(0)%arr1d(1))
        if (.not. retVal) success = .false.
      enddo
    enddo
  enddo
end function convert_default
!
!
!==============================================================================
function convert_unimagdir(converter, curtim) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter)             :: converter
  real(hp)          , intent(in) :: curtim
  !
  ! locals
  integer                      :: i, method
  real(fp)                     :: a0
  real(fp)                     :: a1
  real(hp)                     :: t0
  real(hp)                     :: t1
  real(fp)                     :: u0
  real(fp)                     :: u1
  real(fp)                     :: wdir
  real(fp)                     :: wdir0
  real(fp)                     :: wdir1
  real(fp)                     :: wmag
  type(tECConnection), pointer :: connection
  type(tECItem)      , pointer :: magItem
  type(tECItem)      , pointer :: dirItem
  type(tECItem)      , pointer :: uItem
  type(tECItem)      , pointer :: vItem
  !
  ! body
  do i=1, size(converter%connections)
    connection => converter%connections(i)%ECConnectionPtr
    magItem    => connection%sourceItems(1)%ECItemPtr
    dirItem    => connection%sourceItems(2)%ECItemPtr
    uItem      => connection%targetItems(1)%ECItemPtr
    vItem      => connection%targetItems(2)%ECItemPtr
    !
    t0 = magItem%fields(magItem%t0FieldId)%time
    t1 = magItem%fields(magItem%t1FieldId)%time
    method = converter%method
    call time_weight_factor(a0, a1, curtim, t0, t1, method)
    !
    ! first interpolate in time
    !
    u0    = magItem%fields(magItem%t0FieldId)%arr1d(1)
    u1    = magItem%fields(magItem%t1FieldId)%arr1d(1)
    wmag  = a0*u0 + a1*u1
    wdir0 = dirItem%fields(dirItem%t0FieldId)%arr1d(1)
    wdir1 = dirItem%fields(dirItem%t1FieldId)%arr1d(1)
    !
    ! Now convert to u/v
    ! Be careful with interpolation of angles
    !
    if (comparereal( abs(wdir0-wdir1) , 180.0_fp ) == 1) then
      if (comparereal( wdir0 ,wdir1 ) == -1) then
         wdir0 = wdir0 + 360.0_fp
      else
         wdir1 = wdir1 + 360.0_fp
      endif
    endif
    wdir = a0*wdir0 + a1*wdir1
    if (comparereal( wdir , 360.0_fp ) == 1) then
      wdir = wdir - 360.0_fp
    endif
    !
    ! nautical convention
    !
    wdir = (270.0_fp - wdir)*d2r
    if (associated(uItem%fields(uItem%t0FieldId)%arr3d)) then
      uItem%fields(uItem%t0FieldId)%arr3d = wmag * cos(wdir)
      vItem%fields(vItem%t0FieldId)%arr3d = wmag * sin(wdir)
    endif
    if (associated(uItem%fields(uItem%t0FieldId)%arr2d)) then
      uItem%fields(uItem%t0FieldId)%arr2d = wmag * cos(wdir)
      vItem%fields(vItem%t0FieldId)%arr2d = wmag * sin(wdir)
    endif
    if (associated(uItem%fields(uItem%t0FieldId)%arr1d)) then
      uItem%fields(uItem%t0FieldId)%arr1d = wmag * cos(wdir)
      vItem%fields(vItem%t0FieldId)%arr1d = wmag * sin(wdir)
    endif
    uItem%fields(uItem%t0FieldId)%time = curtim
    vItem%fields(vItem%t0FieldId)%time = curtim
  enddo
  success = .true.
end function convert_unimagdir
!
!
!==============================================================================
function convert_svwp(converter, curtim) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter)             :: converter
  real(hp)          , intent(in) :: curtim
  !
  ! locals
  integer                      :: i, method
  real(fp)                     :: a0
  real(fp)                     :: a1
  real(hp)                     :: t0
  real(hp)                     :: t1
  type(tECConnection), pointer :: connection
  type(tECItem)      , pointer :: uItemS
  type(tECItem)      , pointer :: vItemS
  type(tECItem)      , pointer :: patmItemS
  type(tECItem)      , pointer :: uItemT
  type(tECItem)      , pointer :: vItemT
  type(tECItem)      , pointer :: patmItemT

  !
  ! body
  do i=1, size(converter%connections)
    connection => converter%connections(i)%ECConnectionPtr
    uItemS     => connection%sourceItems(1)%ECItemPtr
    vItemS     => connection%sourceItems(2)%ECItemPtr
    patmItemS  => connection%sourceItems(3)%ECItemPtr
    uItemT     => connection%targetItems(1)%ECItemPtr
    vItemT     => connection%targetItems(2)%ECItemPtr
    patmItemT  => connection%targetItems(3)%ECItemPtr
    !
    t0 = uItemS%fields(uItemS%t0FieldId)%time
    t1 = uItemS%fields(uItemS%t1FieldId)%time
    method = converter%method
    call time_weight_factor(a0, a1, curtim, t0, t1, method)
    !
    uItemT%fields(uItemT%t0FieldId)%arr2d = &
        a0 * uItemS%fields(uItemS%t0FieldId)%arr2d &
      + a1 * uItemS%fields(uItemS%t1FieldId)%arr2d
    vItemT%fields(vItemT%t0FieldId)%arr2d = &
        a0 * vItemS%fields(vItemS%t0FieldId)%arr2d &
      + a1 * vItemS%fields(vItemS%t1FieldId)%arr2d
    patmItemT%fields(patmItemT%t0FieldId)%arr2d = &
        a0 * patmItemS%fields(patmItemS%t0FieldId)%arr2d &
      + a1 * patmItemS%fields(patmItemS%t1FieldId)%arr2d
    !
    uItemT%fields(uItemT%t0FieldId)%time = curtim
    vItemT%fields(vItemT%t0FieldId)%time = curtim
    patmItemT%fields(patmItemT%t0FieldId)%time = curtim
  enddo
  success = .true.
end function convert_svwp
!
function convert_grib(converter, curtim) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConverter)             :: converter
  real(hp)          , intent(in) :: curtim
  !
  ! locals
  integer                      :: i, n, m, method
  integer                      :: nsu, nsd, msu, msd
  integer                      :: dim1source, dim2source, dim1target, dim2target
  logical, parameter           :: debug = .false.
  real(hp)                     :: ns, ms
  real(hp)                     :: dimratio1, dimratio2
  real(fp)                     :: a0
  real(fp)                     :: a1
  real(hp)                     :: t0
  real(hp)                     :: t1
  type(tECConnection), pointer :: connection
  type(tECItem)      , pointer :: uItemS
  type(tECItem)      , pointer :: vItemS
  type(tECItem)      , pointer :: patmItemS
  type(tECItem)      , pointer :: uItemT
  type(tECItem)      , pointer :: vItemT
  type(tECItem)      , pointer :: patmItemT

  !
  ! body
  do i=1, size(converter%connections)
    connection => converter%connections(i)%ECConnectionPtr
    uItemS     => connection%sourceItems(1)%ECItemPtr
    vItemS     => connection%sourceItems(2)%ECItemPtr
    patmItemS  => connection%sourceItems(3)%ECItemPtr
    uItemT     => connection%targetItems(1)%ECItemPtr
    vItemT     => connection%targetItems(2)%ECItemPtr
    patmItemT  => connection%targetItems(3)%ECItemPtr
    !
    t0 = uItemS%fields(uItemS%t0FieldId)%time
    t1 = uItemS%fields(uItemS%t1FieldId)%time
    method = converter%method
    call time_weight_factor(a0, a1, curtim, t0, t1, method)
    !
    dim1target = size(uItemT%fields(uItemT%t0FieldId)%arr2d, 1)
    dim2target = size(uItemT%fields(uItemT%t0FieldId)%arr2d, 2)
    dim1source = size(uItemS%fields(uItemS%t0FieldId)%arr2d, 1)
    dim2source = size(uItemS%fields(uItemS%t0FieldId)%arr2d, 2)
    if (debug) then
       write (*,'(a,i0,a,i0)') 'sizes target are: ', dim1target, ' ', dim2target
       write (*,'(a,i0,a,i0)') 'sizes source are: ', dim1source, ' ', dim2source
    endif
    !
    ! ejs, TODO: simple interpolation in space, to be extented later
    !
    dimratio1 = real(dim1source, hp)/real(dim1target, hp)
    dimratio2 = real(dim2source, hp)/real(dim2target, hp)
    do n = 1, dim1target
       do m = 1, dim2target
          ns = real(n, hp) * dimratio1
          ms = real(m, hp) * dimratio2
          nsd = int(ns)
          nsu = nsd + 1
          msd = int(ms)
          msu = msd + 1
          nsd = max(1, nsd)
          msd = max(1, msd)
          uItemT%fields(uItemT%t0FieldId)%arr2d(n,m) = &
              a0 * uItemS%fields(uItemS%t0FieldId)%arr2d(nsd,msd) &
            + a1 * uItemS%fields(uItemS%t1FieldId)%arr2d(nsd,msd)
          vItemT%fields(vItemT%t0FieldId)%arr2d(n,m) = &
              a0 * vItemS%fields(vItemS%t0FieldId)%arr2d(nsd,msd) &
            + a1 * vItemS%fields(vItemS%t1FieldId)%arr2d(nsd,msd)
          patmItemT%fields(patmItemT%t0FieldId)%arr2d(n,m) = &
              a0 * patmItemS%fields(patmItemS%t0FieldId)%arr2d(nsd,msd) &
            + a1 * patmItemS%fields(patmItemS%t1FieldId)%arr2d(nsd,msd)
       enddo
    enddo
    uItemT%fields(uItemT%t0FieldId)%time = curtim
    vItemT%fields(vItemT%t0FieldId)%time = curtim
    patmItemT%fields(patmItemT%t0FieldId)%time = curtim
  enddo
  success = .true.
end function convert_grib
!
!==============================================================================
function convert_byId(converter, curtim) result(success)
  !
  ! result
  logical                           :: success
  !
  ! arguments
  type(tECConverter)                :: converter
  real(hp)          , intent(in)    :: curtim
  !
  ! locals
  integer                           :: i, s, t, ierr
  real(fp)                          :: a0, a1
  real(hp)                          :: t0, t1
  real(fp), dimension(:), pointer   :: src_t0_arr, src_t1_arr, target_arr
  type(tECConnection), pointer      :: connection
  type(tECItem)      , pointer      :: sourceItem, targetItem
  type(ecElementSet)  , pointer     :: sourceElms, targetElms
  integer, dimension(:,:), pointer  :: indices
  integer                           :: sourceElmsDim, targetElmsDim, method
  !
  ! body
  do i=1, size(converter%connections)
    connection => converter%connections(i)%ECConnectionPtr
    sourceItem => connection%sourceItems(1)%ECItemPtr
    targetItem => connection%targetItems(1)%ECItemPtr
    sourceElms => sourceItem%Elementset
    targetElms => targetItem%Elementset
    sourceElmsDim = getElementSetDim(sourceItem%Elementset)
    targetElmsDim = getElementSetDim(targetItem%Elementset)
    !
    t0     = sourceItem%fields(sourceItem%t0FieldId)%time
    t1     = sourceItem%fields(sourceItem%t1FieldId)%time
    method = converter%method
    call time_weight_factor(a0, a1, curtim, t0, t1, method)
    !
    ! build index by searching for identical IDs in source and target
    !
    if (.not. associated(converter%IndexWeightID)) then
                       allocate(converter%IndexWeightID, STAT=ierr)
        if (ierr == 0) allocate(converter%IndexWeightID%Indices(1,targetElmsDim), STAT=ierr)
        if (ierr /= 0) then
           call setECMessage("ERROR: allocate error in convert_byId.")
           success = .false.
           return
        endif
        indices => converter%IndexWeightID%Indices
        do s = 1, sourceElmsDim
           inner: do t = 1, targetElmsDim
              if (CompareIds(sourceElms, s, targetElms, t)) then
                 indices(1, t) = s
                 exit inner
              endif
           enddo inner
       enddo
    else
       indices => converter%IndexWeightID%Indices
    endif
    !
    ! interpolate in time
    src_t0_arr => sourceItem%fields(sourceItem%t0FieldId)%arr1d
    src_t1_arr => sourceItem%fields(sourceItem%t1FieldId)%arr1d
    target_arr => targetItem%fields(targetItem%t0FieldId)%arr1d
    do t = 1, targetElmsDim
       s = indices(1, t)
       target_arr(t) = a0 * src_t0_arr(s) + a1 * src_t1_arr(s)
    enddo
    targetItem%fields(targetItem%t0FieldId)%time = curtim
  enddo
  success = .true.
end function convert_byId
!
!
!==============================================================================
subroutine time_weight_factor(a0, a1, curtim, t0, t1, method)
   ! arguments
   real(fp), intent(out) :: a0, a1
   real(hp), intent(in)  :: t1, t0, curtim
   integer , intent(in)  :: method
   !
   ! body
   !
   if (method == interpolate_timeNever) then
      a1 = 0.0_fp
   elseif (comparereal(t0,t1) == 0) then
      a1 = 1.0_fp
   else
      a1 = real((curtim - t0)/ (t1-t0), fp)
   endif
   a0 = 1.0_fp - a1
   !
end subroutine time_weight_factor
!
end module ec_converter
