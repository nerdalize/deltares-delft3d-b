module ec_provider
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
!  $Id: ec_provider.F90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_provider.F90 $
!!--description-----------------------------------------------------------------
!
! HTML DOC START
!
! <TABLE>
!   <TR>
!     <TD colSpan=2>
!            Module: ec_provider
!     </TD>
!     <TD colSpan=2>
!            Description:
!              Implementation for each type of provider.
!            More information: http://wiki.deltares.nl/display/FLOW/EC-Module
!     </TD>
!   </TR>
!   <TR>
!     <TD>
!            Dependencies:
!              use ec_data
!     </TD>
!     <TD>
!            Data:
!              type tECProvider
!                integer                                 :: id
!                integer                                 :: type
!                integer                                 :: fileType
!                integer                                 :: pHandle
!                character(maxNameLen)                   :: name
!                character(maxFileNameLen)               :: fileName
!                type(tECItemPtr), dimension(:), pointer :: items
!              end type tECProvider
!     </TD>
!   </TR>
!   <TR>
!     <TD colSpan=2>
!            Typical calls:
!              function ECUpdate_provider(provider, curtim) result (success)
!                select case (provider%type)
!                  case (provType_default)
!                    ! Nothing to be done
!                  case (provType_OpenMI)
!                  case (provType_file)
!                    do
!                      if uptodate exit
!                      select case (provider%filetype)
!                        case (provFile_unimagdir)
!                          success =  readseries(provider%pHandle, 2, mag, dir, tread)
!                        case default
!                          ! error
!                      end select
!                    enddo
!                  case default
!                    ! error
!                end select
!              end function ECUpdate_provider
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
  use ec_provider_read
  use handles
  use table_handles
  !
  implicit none
!
! parameters
!
  !
  ! enumeration for provider types
  integer, parameter :: provType_undefined  = 0
  integer, parameter :: provType_default    = 1
  integer, parameter :: provType_file       = 2
  integer, parameter :: provType_OpenMI     = 3
  integer, parameter :: provType_ThisModule = 4
  integer, parameter :: provType_PutById    = 5
  !
  ! enumeration for provider filetypes
  integer, parameter :: provFile_undefined                      =  0
  integer, parameter :: provFile_uniform                        =  1  ! kx values per timestep 1 dim arr       uni
  integer, parameter :: provFile_unimagdir                      =  2  ! kx values per timestep 1 dim arr, mag/dir transf op index 1,2 u,v
  integer, parameter :: provFile_svwp                           =  3  ! 3 velden per timestep 3 dim array      noint
  integer, parameter :: provFile_arcinfo                        =  4  ! 1 veld per timestep 2 dim array        bilin/direct
  integer, parameter :: provFile_spiderweb                      =  5  ! 3 veld per timestep 3 dim array        bilin/spw
  integer, parameter :: provFile_curvi                          =  6  ! 1 veld per timestep 2 dim array        bilin/findnm
  integer, parameter :: provFile_triangulation                  =  7  ! 1 veld per timestep                    triang
  integer, parameter :: provFile_triangulationmagdir            =  8  ! 2 velden u,v per timestep 3 dim array  triang, vectormax = 2
                                                                      ! op basis van windreeksen op stations mag/dir
  integer, parameter :: provFile_poly_tim                       =  9  ! for line oriented bnd conditions, refs to uniform, fourier or harmonic
  integer, parameter :: provFile_fourier                        = 10  ! period(hrs), ampl(m), phas(deg)
  integer, parameter :: provFile_grib                           = 11  ! grib files from KNMI
  integer, parameter :: provFile_table                          = 12  ! table with e.g. boundary conditions
  integer, parameter :: max_file_types                          = 12  ! max nr of supported types
!
!
! interfaces
!
  interface free
    module procedure free_provider
    module procedure free_provider_array
  end interface
  interface new_provider
    module procedure new_provider_id
    module procedure new_provider_type
    module procedure new_provider_typeString
  end interface
  interface copy
    module procedure copy_provider
  end interface
  interface ECUpdate
    module procedure ECUpdate_provider
    module procedure ECUpdate_put_by_id
  end interface
  interface getConnectionsOfProvider
    module procedure getConnections_provider
  end interface
  interface ECRewind
    module procedure ECRewind_provider
  end interface
  interface dump
    module procedure dump_provider
  end interface
!
! public entities
!
  public :: init_ec_provider
!
contains
!
!
!==============================================================================
! Provider subroutines
!==============================================================================
function free_provider(provider) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider) :: provider
  !
  ! locals
  integer :: istat
  integer :: k
  !
  ! body
  success = .true.
  deallocate(provider%items, STAT = istat)
end function free_provider
!
!
!==============================================================================
function free_provider_array(provider_array) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider), dimension(:), pointer :: provider_array
  !
  ! locals
  integer :: istat
  integer :: k
  !
  ! body
  success = .true.
  do k = 1,size(provider_array)
    success = free(provider_array(k))
  enddo
  !
  if (associated(provider_array)) deallocate(provider_array, STAT = istat)
end function free_provider_array
!
!
!==============================================================================
function new_provider_id(provider, id) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)             :: provider
  integer          , intent(in) :: id
  !
  ! locals
  integer :: istat
  !
  ! body
  provider%id       = id
  provider%type     = provType_undefined
  provider%fileType = provFile_undefined
  provider%pHandle  = 0
  provider%name     = ' '
  provider%fileName = ' '
  allocate(provider%items(0), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_provider::new_provider_id: Unable to allocate additional memory")
  endif
end function new_provider_id
!
!==============================================================================
function init_ec_provider(provider, ECData, qName, qValue, gridECItemId) result(success)
  !
  ! result
  logical                            :: success
  !
  ! arguments
  type(tECProvider)                  :: provider
  type(tECData)                      :: ECData
  real(fp)    , intent(in), optional :: qValue
  integer     , intent(in), optional :: gridECItemId
  character(*), intent(in), optional :: qName
  !
  ! locals
  !
  ! body
  !
  ! Do not call init(provider)!!!
  ! That will remove previously set provFile type
  !
  select case (provider%type)
    case (provType_file)
      select case (provider%fileType)
      case (provFile_unimagdir)
        success = init_provider_unimagdir(provider, ECData)
      case (provFile_svwp, provFile_grib)
        success = init_provider_svwp(provider, ECData)
      case (provFile_table)
        success = .true.
      case default
        call setECMessage("ERROR: ec_provider::init_provider_data: unknown provider fileType:", provider%fileType)
        success = .false.
      end select
    case (provType_default)
       success = init_provider_default(provider, ECData, qName, qValue)
    case (provType_PutById)
       success = init_provider_put_by_id(provider, ECData, qName, gridECItemId)
    case default
       call setECMessage("ERROR: ec_provider::init_provider_dataNameValue: unknown provider Type:", provider%fileType)
       success = .false.
  end select
end function init_ec_provider
!
!
!==============================================================================
function new_provider_type(provider, ECData, provType) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)            :: provider
  type(tECData)                :: ECData
  integer         , intent(in) :: provType
  !
  ! locals
  !
  ! body
  provider%type = provType
  success       = .true.
end function new_provider_type
!
!
!==============================================================================
function new_provider_typeString(provider, ECData, provType, iniString, provFileType) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)            :: provider
  type(tECData)                :: ECData
  integer         , intent(in) :: provType
  character(*)    , intent(in) :: iniString ! provType = file: iniString = filename
  integer         , intent(in) :: provFileType
  !
  ! locals
  !
  integer                      :: i
  integer                      :: wildcard  ! only implementing '?'
  character(maxFileNameLen)    :: filename
  ! body
  provider%type = provType
  select case (provType)
    case (provType_file)
      provider%filenr = -1
      provider%fileName = iniString
      wildcard = index(iniString, '?')
      if (wildcard == 0) then
         inquire(file=trim(iniString), exist=success)
      else
         success = .false.
         filename = iniString
         do i = 0, 9
            write(filename(wildcard:wildcard), '(i1)') i
            inquire(file=trim(filename), exist=success)
            if (success) then
               provider%filenr = i
               exit
            endif
         enddo
      endif
      !
      if (.not. success) then
         call setECMessage("ERROR: ec_provider: File does not exist:", trim(iniString))
      else
         success = set_prov_filetype(provider, provFileType)
      endif
    case default
      call setECMessage("ERROR: ec_provider::new_provider_typeString: unknown provider Type:", provider%fileType)
      success = .false.
  end select
  !
end function new_provider_typeString
!
!
!==============================================================================
function init_provider_default(provider, ECData, qName, qValue) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)             :: provider
  type(tECData)                 :: ECData
  real(fp)         , intent(in) :: qValue
  character(*)     , intent(in) :: qName
  !
  ! locals
  integer                :: elSetId
  integer                :: meteoECId
  integer                :: moduleECId
  integer                :: qId
  integer                :: ECConnectionId
  type(tECItem), pointer :: itemPtr
  !
  ! body
  elSetId = addElementSet(ECData, (/ 0.0_hp /), (/ 0.0_hp /), (/ 1 /), .false.)
  if (elSetId == 0) return
  !
  ! Add p ECItems defined on File elementSet, having provId as provider
  ! Add connection with quantity on FLOW grid
  !
  ECConnectionId = addConnection(ECData)
  !
  qId        = getQuantity(ECData, qName)
  meteoECId  = addECItem(ECData, qId, elSetId)
  success    = ECItemSetType(ECData, meteoECId, ECItemType_source)
  success    = ECItemAddFields(ECData, meteoECId, 1, 1, 1)
  success    = ECItemAddProvider(ECData, meteoECId, provider%id)
  success    = ECItemAddConnection(ECData, meteoECId, ECConnectionId)
  itemPtr    => ECData%ECItemPtrs(meteoECId)%ECItemPtr
  success    = ECProviderAddItem(provider,itemPtr)
  success    = ECConnectionAddSource(ECData, ECConnectionId, meteoECId)
  if (.not. success) return
  !
  moduleECId = getECItem(ECData, qId, getInternalGrid(ECData))
  success    = ECItemAddConnection(ECData, moduleECId, ECConnectionId)
  success    = ECConnectionAddTarget(ECData, ECConnectionId, moduleECId)
  success    = ECItemSetValue(ECData, meteoECId, qValue)
  if (.not. success) return
end function init_provider_default
!
!
!==============================================================================
function init_provider_unimagdir(provider, ECData) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider) :: provider
  type(tECData)     :: ECData
  !
  ! locals
  integer                 :: elSetId
  integer                 :: meteoECId
  integer                 :: moduleECId
  integer                 :: qIdSource
  integer                 :: qIdTarget
  integer                 :: ECConnectionId
  logical      , external :: openExistingFile
  type(tECItem), pointer  :: itemPtr
  !
  ! body
  success = openExistingFile(provider%pHandle,provider%fileName)
  if (.not. success) return
  !
  elSetId = addElementSet(ECData, (/ 0.0_hp /), (/ 0.0_hp /), (/ 1 /), .false.)
  if (elSetId == 0) return
  !
  ! Add u/v ECItems defined on File elementSet, having provId as provider
  ! Add connection with quantity on FLOW grid
  !
  ECConnectionId = addConnection(ECData)
  !
  qIdSource  = addQuantity(ECData, 'wind_magnitude')
  meteoECId  = addECItem(ECData, qIdSource, elSetId)
  success    = ECItemSetType(ECData, meteoECId, ECItemType_source)
  success    = ECItemAddFields(ECData, meteoECId, 2, 1, 1)
  success    = ECItemAddProvider(ECData, meteoECId, provider%id)
  success    = ECItemAddConnection(ECData, meteoECId, ECConnectionId)
  itemPtr    => ECData%ECItemPtrs(meteoECId)%ECItemPtr
  success    = ECProviderAddItem(provider,itemPtr)
  success    = ECConnectionAddSource(ECData, ECConnectionId, meteoECId)
  if (.not. success) return
  !
  qIdSource  = addQuantity(ECData, 'wind_direction')
  meteoECId  = addECItem(ECData, qIdSource, elSetId)
  success    = ECItemSetType(ECData, meteoECId, ECItemType_source)
  success    = ECItemAddFields(ECData, meteoECId, 2, 1, 1)
  success    = ECItemAddProvider(ECData, meteoECId, provider%id)
  success    = ECItemAddConnection(ECData, meteoECId, ECConnectionId)
  itemPtr => ECData%ECItemPtrs(meteoECId)%ECItemPtr
  success    = ECProviderAddItem(provider,itemPtr)
  success    = ECConnectionAddSource(ECData, ECConnectionId, meteoECId)
  if (.not. success) return
  !
  qIdTarget  = getQuantity(ECData, 'x_wind')
  moduleECId = getECItem(ECData, qIdTarget, getInternalGrid(ECData))
  success    = ECItemAddConnection(ECData, moduleECId, ECConnectionId)
  success    = ECConnectionAddTarget(ECData, ECConnectionId, moduleECId)
  if (.not. success) return
  !
  qIdTarget  = getQuantity(ECData, 'y_wind')
  moduleECId = getECItem(ECData, qIdTarget, getInternalGrid(ECData))
  success    = ECItemAddConnection(ECData, moduleECId, ECConnectionId)
  success    = ECConnectionAddTarget(ECData, ECConnectionId, moduleECId)
  if (.not. success) return
end function init_provider_unimagdir
!
!
!==============================================================================
function init_provider_put_by_id(provider, ECData, qName, elSetId) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider) :: provider
  type(tECData)     :: ECData
  character(*), intent(in) :: qName
  integer                 :: elSetId
  !
  ! locals
  integer                 :: ECId
  integer                 :: moduleECId
  integer                 :: qIdSource
  integer                 :: qIdTarget
  integer                 :: ECConnectionId
  integer                 :: dim
  type(tECItem), pointer  :: itemPtr
  !
  ! body
  !
  ! Add ECItems defined on File elementSet, having provId as provider
  ! Add connection with quantity on FLOW grid
  !
  ECConnectionId = addConnection(ECData)
  !
  qIdSource  = addQuantity(ECData, qName // '_S')
  ECId       = addECItem(ECData, qIdSource, elSetId)
  success    = ECItemSetType(ECData, ECId, ECItemType_source)
  dim        = getElementSetDim(ECData%ECItemPtrs(ECId)%ECItemPtr%Elementset)
  success    = ECItemAddFields(ECData, ECId, 2, 1, dim)
  success    = ECItemAddProvider(ECData, ECId, provider%id)
  success    = ECItemAddConnection(ECData, ECId, ECConnectionId)
  itemPtr    => ECData%ECItemPtrs(ECId)%ECItemPtr
  success    = ECProviderAddItem(provider,itemPtr)
  success    = ECConnectionAddSource(ECData, ECConnectionId, ECId)
  !
  qIdTarget  = getQuantity(ECData, qName)
  if (qIdTarget == 0) then
     success    = .false.
  else
     moduleECId = getECItem(ECData, qIdTarget) ! getInternalGrid(ECData))
     success    = ECItemAddConnection(ECData, moduleECId, ECConnectionId)
     success    = ECConnectionAddTarget(ECData, ECConnectionId, moduleECId)
  endif
  !
end function init_provider_put_by_id
!
!==============================================================================
function init_provider_svwp(provider, ECData) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider) :: provider
  type(tECData)     :: ECData
  !
  ! locals
  integer                 :: dim1
  integer                 :: dim2
  integer                 :: elSetId
  integer                 :: i
  integer                 :: ierr
  integer                 :: meteoECId
  integer                 :: moduleECId
  integer                 :: qIdSource
  integer                 :: qIdTarget
  integer                 :: ECConnectionId
  logical      , external :: openExistingFile
  type(tECItem), pointer  :: itemPtr
  real(fp), dimension(:,:), pointer :: arr2D
  real(hp)                :: tread
  !
  ! body
  if (provider%FILETYPE == provFile_grib) then
     success = ec_grib_open(provider%pHandle, provider%fileName, provider%filenr)
  else
     success = openExistingFile(provider%pHandle,provider%fileName)
  endif
  if (.not. success) return
  !
  elSetId = getInternalGrid(ECData)
  if (elSetId == 0) then
     success = .false.
     return
  endif
  !
  ! get dimensions
  !
  arr2D => ECData%ECItemPtrs(1)%ECItemptr%fields(0)%Arr2D
  if (provider%FILETYPE == provFile_grib) then
     success = read_grib(provider%pHandle, provider%filename, provider%filenr, arr2D, arr2D, arr2D, dim1, dim2, provider%grib_meta, tread, .true.)
     if (.not. success) return
  else
     dim1  =  size(arr2D,1)
     dim2  =  size(arr2D,2)
  endif
  !
  ! Add u/v ECItems defined on File elementSet, having provId as provider
  ! Add connection with quantity on FLOW grid
  !
  ECConnectionId = addConnection(ECData)
  !
  qIdSource  = addQuantity(ECData, 'x_wind_S')
  meteoECId  = addECItem(ECData, qIdSource, elSetId)
  success    = ECItemSetType(ECData, meteoECId, ECItemType_source)
  success    = ECItemAddFields(ECData, meteoECId, 2, 2, dim1, dim2)
  success    = ECItemAddProvider(ECData, meteoECId, provider%id)
  success    = ECItemAddConnection(ECData, meteoECId, ECConnectionId)
  itemPtr    => ECData%ECItemPtrs(meteoECId)%ECItemPtr
  success    = ECProviderAddItem(provider,itemPtr)
  success    = ECConnectionAddSource(ECData, ECConnectionId, meteoECId)
  !
  if (.not. success) return
  !
  qIdSource  = addQuantity(ECData, 'y_wind_S')
  meteoECId  = addECItem(ECData, qIdSource, elSetId)
  success    = ECItemSetType(ECData, meteoECId, ECItemType_source)
  success    = ECItemAddFields(ECData, meteoECId, 2, 2, dim1, dim2)
  success    = ECItemAddProvider(ECData, meteoECId, provider%id)
  success    = ECItemAddConnection(ECData, meteoECId, ECConnectionId)
  itemPtr    => ECData%ECItemPtrs(meteoECId)%ECItemPtr
  success    = ECProviderAddItem(provider,itemPtr)
  success    = ECConnectionAddSource(ECData, ECConnectionId, meteoECId)
  if (.not. success) return
  !
  qIdSource  = addQuantity(ECData, 'air_pressure_S')
  meteoECId  = addECItem(ECData, qIdSource, elSetId)
  success    = ECItemSetType(ECData, meteoECId, ECItemType_source)
  success    = ECItemAddFields(ECData, meteoECId, 2, 2, dim1, dim2)
  success    = ECItemAddProvider(ECData, meteoECId, provider%id)
  success    = ECItemAddConnection(ECData, meteoECId, ECConnectionId)
  itemPtr    => ECData%ECItemPtrs(meteoECId)%ECItemPtr
  success    = ECProviderAddItem(provider,itemPtr)
  success    = ECConnectionAddSource(ECData, ECConnectionId, meteoECId)
  if (.not. success) return
  !
  qIdTarget  = getQuantity(ECData, 'x_wind')
  moduleECId = getECItem(ECData, qIdTarget, getInternalGrid(ECData))
  success    = ECItemAddConnection(ECData, moduleECId, ECConnectionId)
  success    = ECConnectionAddTarget(ECData, ECConnectionId, moduleECId)
  if (.not. success) return
  !
  qIdTarget  = getQuantity(ECData, 'y_wind')
  moduleECId = getECItem(ECData, qIdTarget, getInternalGrid(ECData))
  success    = ECItemAddConnection(ECData, moduleECId, ECConnectionId)
  success    = ECConnectionAddTarget(ECData, ECConnectionId, moduleECId)
  if (.not. success) return
  !
  qIdTarget  = getQuantity(ECData, 'air_pressure')
  moduleECId = getECItem(ECData, qIdTarget, getInternalGrid(ECData))
  success    = ECItemAddConnection(ECData, moduleECId, ECConnectionId)
  success    = ECConnectionAddTarget(ECData, ECConnectionId, moduleECId)
!  
end function init_provider_svwp
!
!
!==============================================================================
function copy_provider(sourceP,targetP) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider) :: sourceP
  type(tECProvider) :: targetP
  !
  ! locals
  integer :: i
  integer :: istat
  !
  ! body
  targetP%id       = sourceP%id
  targetP%type     = sourceP%type
  targetP%fileType = sourceP%fileType
  targetP%pHandle  = sourceP%pHandle
  targetP%name     = sourceP%name
  targetP%fileName = sourceP%fileName
  if (associated(targetP%items)) deallocate(targetP%items, STAT = istat)
  allocate(targetP%items(size(sourceP%items)), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_provider::copy_provider: Unable to allocate additional memory")
    return
  endif
  do i=1,size(targetP%items)
     targetP%items(i) = sourceP%items(i)
  enddo
end function copy_provider
!
!
!==============================================================================
function set_prov_filetype(provider, provtype) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)             :: provider
  integer          , intent(in) :: provtype
  !
  ! local parameters
  integer                       :: unitnr, refjulday
  logical                       :: unitused
  character(len=256)            :: errorstring
  type(handletype)              :: handle
  ! body
  select case (provtype)
    case (provFile_undefined, provFile_uniform, provFile_unimagdir, provFile_svwp, provFile_grib, &
          provFile_arcinfo, provFile_spiderweb, provFile_curvi, provFile_triangulation, &
          provFile_triangulationmagdir, provFile_poly_tim, provFile_fourier)
      success           = .true.
      provider%filetype = provtype
    case (provFile_table)
       provider%filetype = provtype
       do unitnr = 1, 100
          inquire (unit = unitnr, opened = unitused) 
          if (.not. unitused) exit
       enddo
       refjulday = 245245
       call readtable(handle, unitnr, provider%fileName, refjulday, errorstring)
       success           = .true.
    case default
      success = .false.
      call setECMessage("ERROR: ec_provider::set_prov_filetype: Unknown provider type:", provtype)
  end select
end function set_prov_filetype
!
!
!==============================================================================
function ECProviderAddItem(provider, item) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)             :: provider
  type(tECItem)    , pointer    :: item
  !
  ! locals
  integer :: k
  !
  ! body
  k                           = ECProviderIncreaseSize_items(provider)
  provider%items(k)%ECItemPtr => item
  success                     = .true.
end function ECProviderAddItem
!
!
!==============================================================================
function ECProviderIncreaseSize_items(provider) result (k)
  !
  ! result
  integer :: k
  !
  ! arguments
  type(tECProvider) :: provider
  !
  ! locals
  integer                                 :: istat
  logical                                 :: success
  type(tECItemPtr), dimension(:), pointer :: newItems
  !
  ! body
  k = size(provider%items) + 1
  allocate(newItems(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_provider::ECProviderIncreaseSize_items: Unable to allocate additional memory")
    return
  endif
  success = ECProviderCopyArray_items(provider, provider%items, newItems)
  deallocate (provider%items, STAT = istat)
  provider%items => newItems
end function ECProviderIncreaseSize_items
!
!
!==============================================================================
function ECProviderCopyArray_items(provider, sourceArr, targetArr) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)                       :: provider
  type(tECItemPtr), dimension(:), pointer :: sourceArr
  type(tECItemPtr), dimension(:), pointer :: targetArr
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
     targetArr(k)%ECItemPtr => sourceArr(k)%ECItemPtr
  enddo
  do k = arrLenSource+1, arrLenTarget
     nullify(targetArr(k)%ECItemPtr)
  enddo
  success = .true.
end function ECProviderCopyArray_items
!
!
!==============================================================================
subroutine dump_provider(provider)
  !
  ! arguments
  type(tECProvider) :: provider
  !
  ! locals
  integer :: i
  !
  ! body
  write (*,'(a,i0)') '  id         : ', provider%id
  write (*,'(a,i0)') '  type       : ', provider%type
  write (*,'(a,i0)') '  fileType   : ', provider%fileType
  write (*,'(a,i0)') '  pHandle    : ', provider%pHandle
  do i=1, size(provider%items)
    write (*,'(a,i0,a,i0)') '  item(',i,') id : ', provider%items(i)%ECItemPtr%id
  enddo
  write (*,'(a,a)' ) '  name       : ', trim(provider%name)
  write (*,'(a,a)' ) '  fileName   : ', trim(provider%fileName)
end subroutine dump_provider
!
!
!==============================================================================
function ECUpdate_put_by_id(provider, curtim, val) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)               :: provider
  real(hp)         , intent(in)   :: curtim
  real(hp), dimension(:), pointer :: val
  !
  ! locals
  !
  type(tECItem)         , pointer :: firstItem
  real(fp), dimension(:), pointer :: fieldarr1dB
  !
  ! body
  !
  select case (provider%type)
  case (provType_PutById)
     firstItem      => provider%items(1)%ECItemPtr
     call swapECItem(firstItem)
     fieldarr1dB    => firstItem%fields(firstItem%t1FieldId)%arr1d
     fieldarr1dB    =  val
     firstItem%fields(firstItem%t1FieldId)%time  = curtim
     success = .true.
  case default
     !this function is only intended for provType_PutById.
     success = .false.
  end select
end function ECUpdate_put_by_id
!
!
!==============================================================================
function ECUpdate_provider(provider, curtim) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)             :: provider
  real(hp)         , intent(in) :: curtim
  !
  ! locals
  integer                         :: i
  integer                         :: ierr
  integer                         :: lenout
  integer                         :: nmax
  integer                         :: mmax
  type(tECItem)         , pointer :: firstItem
  real(hp)                        :: tread
  real(fp), dimension(:), pointer :: fieldarr1dA
  real(fp), dimension(:), pointer :: fieldarr1dB
  real(fp), dimension(:,:), pointer :: fieldarr2dA
  real(fp), dimension(:,:), pointer :: fieldarr2dB
  real(fp), dimension(:,:), pointer :: fieldarr2dC
  character(300)                  :: message
  !
  ! body
  select case (provider%type)
    case (provType_default, provType_PutById)
      !
      ! Nothing to be done
      !
      success = .true.
    case (provType_OpenMI)
      !
      ! Not Yet Implemented
      !
      success = .false.
      call setECMessage("ERROR ec_provider::ECUpdate_provider: provider type OpenMI is not yet implemented")
      return
    case (provType_file)
      do
        firstItem => provider%items(1)%ECItemPtr
        !
        ! stop reading data when (tim <= it1%time) and (it0 and it1 are not empty)
        ! it%time is hp, comparereal expects fp
        !
        if (  comparereal(curtim         , firstItem%fields(firstItem%t1FieldId)%time) /= 1 .and. &
            & comparereal(notime_default , firstItem%fields(firstItem%t0FieldId)%time) /= 0 .and. &
            & comparereal(notime_default , firstItem%fields(firstItem%t1FieldId)%time) /= 0        ) exit
        do i=1, size(provider%items)
          call swapECItem(provider%items(i)%ECItemPtr)
        enddo
        !it1          = meteoitem%it1
        !minp         = meteoitem%lun
        !mx           = meteoitem%numm
        !nx           = meteoitem%numn
        !kx           = meteoitem%numk
        !
        ! Read the time field in the meteo file
        ! For uniform wind nothing is done in readtime
        !
        !success = readtime(minp, meteoitem, flow_itdate, flow_tzone, tread)
        !if (.not. success) then
        !   return
        !endif
        !
        select case (provider%filetype)
          case (provFile_unimagdir)
            fieldarr1dA   => provider%items(1)%ECItemPtr%fields(provider%items(1)%ECItemPtr%t1FieldId)%arr1d
            fieldarr1dB   => provider%items(2)%ECItemPtr%fields(provider%items(2)%ECItemPtr%t1FieldId)%arr1d
            success =  readseries(provider%pHandle, 2, fieldarr1dA, fieldarr1dB, tread)
            if (.not. success) return
            provider%items(1)%ECItemPtr%fields(firstItem%t1FieldId)%time  = tread
            provider%items(2)%ECItemPtr%fields(firstItem%t1FieldId)%time  = tread
            !
            ! Conversion from angle/magnitude to u/v is done by the converter,
            ! after time interpolation
            !
          case (provFile_svwp, provFile_grib)
            fieldarr2dA   => provider%items(1)%ECItemPtr%fields(provider%items(1)%ECItemPtr%t1FieldId)%arr2d
            fieldarr2dB   => provider%items(2)%ECItemPtr%fields(provider%items(2)%ECItemPtr%t1FieldId)%arr2d
            fieldarr2dC   => provider%items(3)%ECItemPtr%fields(provider%items(3)%ECItemPtr%t1FieldId)%arr2d
            nmax = size(fieldarr2dB,1)
            mmax = size(fieldarr2dB,2)
            if (provider%filetype == provFile_svwp) then
                success = read_spv_block(provider%pHandle, 1d0, fieldarr2dA, fieldarr2dB, fieldarr2dC, nmax, mmax, 1, tread)
            else
                success = read_grib(provider%pHandle, provider%filename, provider%filenr, fieldarr2dA, fieldarr2dB, fieldarr2dC, nmax, mmax, provider%grib_meta, tread, .false.)
            endif
            if (.not. success) return
            provider%items(1)%ECItemPtr%fields(firstItem%t1FieldId)%time  = tread
            provider%items(2)%ECItemPtr%fields(firstItem%t1FieldId)%time  = tread
            provider%items(3)%ECItemPtr%fields(firstItem%t1FieldId)%time  = tread
          case default
            success = .false.
            call setECMessage("ERROR ec_provider::ECUpdate_provider: unknown provider filetype", provider%filetype)
            return
        end select
      enddo
      if ( comparereal(firstItem%fields(firstItem%t0FieldId)%time, curtim) == 1 ) then
         success = .false.
         write(message,'(3a,2(g16.8,a))') 'In file ',trim(provider%filename), &
              & ': Start time of data (', firstItem%fields(firstItem%t0FieldId)%time, &
              & ') is behind start time of simulation (',curtim,')'
         call setECMessage("ERROR", trim(message))
         return
      else
         success = .true.
      endif
    case default
      success = .false.
      call setECMessage("ERROR ec_provider::ECUpdate_provider: unknown provider type", provider%type)
      return
  end select
end function ECUpdate_provider
!
!
!==============================================================================
function getConnections_provider(provider, ECData, connectionIds) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider)              :: provider
  type(tECData)                  :: ECData
  integer, dimension(:), pointer :: connectionIds
  !
  ! locals
  integer :: itemId
  integer :: listElt
  !
  ! body
  do itemId=1,size(provider%items)
    do listElt=1,size(provider%items(itemId)%ECItemPtr%connectionIds)
      call addUniqueInt(connectionIds,provider%items(itemId)%ECItemPtr%connectionIds(listElt))
    enddo
  enddo
  success = .true.
end function getConnections_provider
!
!
!==============================================================================
function ECRewind_provider(provider) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECProvider) :: provider
  !
  ! locals
  integer           :: ierr
  !
  ! body
  select case (provider%type)
    case (provType_default, provType_PutById)
      !
      ! Nothing to be done
      !
      success = .true.
    case (provType_OpenMI)
      !
      ! Not Yet Implemented
      !
      success = .false.
      call setECMessage("ERROR: ec_provider::ECRewind_provider: provider type OpenMI is not yet implemented")
      return
    case (provType_file)
      select case (provider%filetype)
         case (provFile_unimagdir)
           rewind(provider%pHandle)
           success = .true.
         case (provFile_svwp)
           rewind(provider%pHandle)
           success = .true.
         case (provFile_grib)
           if (provider%pHandle >= 0) then
#             if defined GRIB
                 call pbclose(provider%pHandle, ierr)
                 if (ierr /= 0) write (*,*) 'problem with closing grib file; continuing'
#             else
                 success = .false.
                 call setECMessage("ERROR: grib lib not linked")
                 return
#             endif
           endif
           success = ec_grib_open(provider%pHandle, provider%fileName, provider%filenr)
         case default
           success = .false.
           call setECMessage("ERROR: ec_provider::ECRewind_provider: Undefined provider filetype", provider%filetype)
           return
      end select
    case default
      success = .false.
      call setECMessage("ERROR ec_provider::ECrewind_provider: unknown provider type", provider%type)
      return
   end select
end function ECrewind_provider



end module ec_provider
