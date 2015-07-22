module ec_connection
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
!  $Id: ec_connection.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_connection.f90 $
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
  use ec_ecitem
  use ec_typedefs
  !
  implicit none
!
! parameters
!
! interfaces
!
  interface init
     module procedure init_connection_noData
  end interface
  interface addConnection
     module procedure addConnection
     module procedure addConnection_ids
  end interface
  interface ECConnectionAddSource
     module procedure ECConnectionAddSource
  end interface
  interface ECConnectionAddTarget
     module procedure ECConnectionAddTarget
  end interface
  interface dump
    module procedure dump_connection
  end interface
!
! public entities
!
contains
!
!
!==============================================================================
function free_connection(connection) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConnection) :: connection
  !
  ! locals
  integer :: istat
  !
  ! body
  if (associated(connection%sourceItems)) deallocate(connection%sourceItems, STAT = istat)
  if (associated(connection%targetItems)) deallocate(connection%targetItems, STAT = istat)
  success = .true.
end function free_connection
!
!
!==============================================================================
function init_connection_noData(connection) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConnection) :: connection
  !
  ! locals
  integer :: istat
  !
  ! body
  connection%id = 0
                  allocate(connection%sourceItems(0), STAT = istat)
  if (istat == 0) allocate(connection%targetItems(0), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_connection::init_connection_noData: Unable to allocate additional memory")
  endif
end function init_connection_noData
!
!
!==============================================================================
function increaseSize_connectionPtrs(connectionPtrs) result (k)
  !
  ! result
  integer :: k
  !
  ! arguments
  type(tECConnectionPtr), dimension(:), pointer :: connectionPtrs
  !
  ! locals
  integer                                       :: istat
  logical                                       :: success
  type(tECConnectionPtr), dimension(:), pointer :: newConnectionPtrs
  !
  ! body
  k = size(connectionPtrs) + 1
  allocate(newConnectionPtrs(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_connection::increaseSize_connectionPtrs: Unable to allocate additional memory")
    return
  endif
  success = copyArray_connections(connectionPtrs,newConnectionPtrs)
  if (.not. success) k=0
  deallocate (connectionPtrs, STAT = istat)
  connectionPtrs => newConnectionPtrs
end function increaseSize_connectionPtrs
!
!
!==============================================================================
function copyArray_connections(sourceArr, targetArr) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConnectionPtr), dimension(:), pointer :: sourceArr
  type(tECConnectionPtr), dimension(:), pointer :: targetArr
  !
  ! locals
  integer :: istat
  integer :: k
  integer :: arrLenSource
  integer :: arrLenTarget
  !
  ! body
  success      = .true.
  arrLenSource = min(size(sourceArr), size(targetArr))
  arrLenTarget = size(targetArr)
  do k = 1,arrLenSource
    targetArr(k)%ECConnectionPtr => sourceArr(k)%ECConnectionPtr
  enddo
  do k = arrLenSource+1, arrLenTarget
    allocate(targetArr(k)%ECConnectionPtr, STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      call setECMessage("ERROR: ec_connection::copyArray_connections: Unable to allocate additional memory")
      return
    endif
    success = init(targetArr(k)%ECConnectionPtr)
  enddo
end function copyArray_connections
!
!
!==============================================================================
function increaseSize_sources(connection) result (k)
  !
  ! result
  integer :: k
  !
  ! arguments
  type(tECConnection) :: connection
  !
  ! locals
  integer                                 :: istat
  logical                                 :: success
  type(tECItemPtr), dimension(:), pointer :: newSources
  !
  ! body
  k = size(connection%sourceItems) + 1
  allocate(newSources(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_connection::increaseSize_sources: Unable to allocate additional memory")
    return
  endif
  success = copyArray_ECItemPtrs(connection, connection%sourceItems, newSources)
  if (.not. success) k=0
  deallocate (connection%sourceItems, STAT = istat)
  connection%sourceItems => newSources
end function increaseSize_sources
!
!
!==============================================================================
function increaseSize_targets(connection) result (k)
  !
  ! result
  integer :: k
  !
  ! arguments
  type(tECConnection) :: connection
  !
  ! locals
  integer                                 :: istat
  logical                                 :: success
  type(tECItemPtr), dimension(:), pointer :: newTargets
  !
  ! body
  k = size(connection%targetItems) + 1
  allocate(newTargets(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    call setECMessage("ERROR: ec_connection::increaseSize_targets: Unable to allocate additional memory")
    return
  endif
  success = copyArray_ECItemPtrs(connection, connection%targetItems, newTargets)
  if (.not. success) k=0
  deallocate (connection%targetItems, STAT = istat)
  connection%targetItems => newTargets
end function increaseSize_targets
!
!
!==============================================================================
function copyArray_ECItemPtrs(connection, sourceArr, targetArr) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConnection)                     :: connection
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
end function copyArray_ECItemPtrs
!
!
!==============================================================================
function getConnection(connectionPtrs, sourceItem, targetItem) result(id)
  !
  ! result: unique connection id or zero if it does not exist
  integer :: id
  !
  ! arguments
  type(tECConnectionPtr), dimension(:), pointer :: connectionPtrs
  type(tECItem)                                 :: sourceItem
  type(tECItem)                                 :: targetItem
  !
  ! locals
  integer            :: aSourceId
  integer            :: aTargetId
  integer            :: sourceItemId
  integer            :: targetItemId
  integer            :: i
  integer            :: j
  integer            :: k
  integer            :: retVal
  character(len=100) :: string
  !
  ! body
  sourceItemId = getECItemId(sourceItem)
  targetItemId = getECItemId(targetItem)

  retVal = 0
  do i=1, size(connectionPtrs)
    do j=1, size(connectionPtrs(i)%ECConnectionPtr%sourceItems)
      do k=1, size(connectionPtrs(i)%ECConnectionPtr%targetItems)
        aSourceId = getECItemId(connectionPtrs(i)%ECConnectionPtr%sourceItems(j)%ECItemPtr)
        aTargetId = getECItemId(connectionPtrs(i)%ECConnectionPtr%targetItems(k)%ECItemPtr)
        if (aSourceId==sourceItemId .and. aTargetId==targetItemId) then
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
            write(string,'(a,i0,a,i0,a)') 'sourceId "', sourceItemId, '" and targetId "', targetItemId, '"'
            call setECMessage("ERROR: More than one ECconnection with", trim(string))
            return
          endif
        endif
      enddo
    enddo
  enddo
  if (retVal == -1) then
    retVal = 0
  endif
  id = retVal
end function getConnection
!
!
!==============================================================================
function addConnection(connectionPtrs) result(id)
  !
  ! result: unique new connection id
  integer :: id
  !
  ! arguments
  type(tECConnectionPtr), dimension(:), pointer :: connectionPtrs
  !
  ! locals
  !
  ! body
  !
  ! allocate a new one
  id                 = increaseSize_connectionPtrs(connectionPtrs)
  connectionPtrs(id)%ECConnectionPtr%id = id
end function addConnection
!
!
!==============================================================================
function addConnection_ids(connectionPtrs, sourceItem, targetItem) result(id)
  !
  ! result: unique new connection id
  integer :: id
  !
  ! arguments
  type(tECConnectionPtr), dimension(:), pointer :: connectionPtrs
  type(tECItem)                       , pointer :: targetItem
  type(tECItem)                       , pointer :: sourceItem
  !
  ! locals
  logical :: success
  !
  ! body
  id = getConnection(connectionPtrs, sourceItem, targetItem)
  if (id == 0) then
    !
    ! allocate a new one
    !
    id      = addConnection(connectionPtrs)
    success = ECConnectionAddSource(connectionPtrs(id)%ECConnectionPtr, sourceItem)
    if (.not. success) id = 0
    success = ECConnectionAddTarget(connectionPtrs(id)%ECConnectionPtr, targetItem)
    if (.not. success) id = 0
  endif
end function addConnection_ids
!
!
!==============================================================================
function ECConnectionAddSource(connection, sourceItem) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConnection)          :: connection
  type(tECItem)      , pointer :: sourceItem
  !
  ! locals
  integer :: id
  !
  ! body
  id = increaseSize_sources(connection)
  connection%sourceItems(id)%ECItemPtr => sourceItem
  success = .true.
end function ECConnectionAddSource
!
!
!==============================================================================
function ECConnectionAddTarget(connection, targetItem) result(success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECConnection)          :: connection
  type(tECItem)      , pointer :: targetItem
  !
  ! locals
  integer :: id
  !
  ! body
  id = increaseSize_targets(connection)
  connection%targetItems(id)%ECItemPtr => targetItem
  success = .true.
end function ECConnectionAddTarget
!
!
!==============================================================================
subroutine dump_connection(connection)
  !
  ! arguments
  type(tECConnection) :: connection
  !
  ! locals
  integer :: i
  !
  ! body
  write (*,'(a,i0)') '    id         : ', connection%id
  write (*,'(a,i0)') '    converterId: ', connection%converterId
  do i=1, size(connection%sourceItems)
    write (*,'(a,i0,a,i0)') '    sourceItems(',i,')id: ', connection%sourceItems(i)%ECItemPtr%id
  enddo
  do i=1, size(connection%targetItems)
    write (*,'(a,i0,a,i0)') '    targetItems(',i,')id: ', connection%targetItems(i)%ECItemPtr%id
  enddo
end subroutine dump_connection



end module ec_connection
