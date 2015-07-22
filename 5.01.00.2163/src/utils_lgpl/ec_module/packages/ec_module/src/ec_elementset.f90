module ec_elementset
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
!  $Id: ec_elementset.f90 1865 2012-09-25 15:33:35Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_elementset.f90 $
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
!
! interfaces
!
  interface dump
    module procedure dump_ElementSet
  end interface
  interface addElementSet_elem
    module procedure addElementSet_components
    module procedure addElementSet_IDs
  end interface
!
! public entities
!
  public :: ecElementSet
  public :: free_Elementsets
  public :: increaseSize_Elementsets
  public :: addElementSet_elem
  public :: getElementset_elem
  public :: getElementsetId
  public :: getElementSetDim
  public :: CompareIds
  public :: dump
contains
!
!
!==============================================================================
function free_elementSets(elementSets) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(ecElementSet), dimension(:), pointer :: elementSets
  !
  ! locals
  integer :: istat
  integer :: k
  !
  ! body
  do k = 1,size(elementSets)
    if (associated(elementSets(k)%kcs)) deallocate (elementSets(k)%kcs, STAT = istat)
    if (associated(elementSets(k)%x  )) deallocate (elementSets(k)%x  , STAT = istat)
    if (associated(elementSets(k)%y  )) deallocate (elementSets(k)%y  , STAT = istat)
  enddo
  if (associated(elementSets)) deallocate (elementSets, STAT = istat)
  success = .true.
end function free_elementSets
!
!
!==============================================================================
function init_elementSet(elSet) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(ecElementSet) :: elSet
  !
  ! locals
  !
  ! body
  elSet%id         = -1
  elSet%dim        = 0
  nullify(elSet%kcs)
  nullify(elSet%x)
  nullify(elSet%y)
  elSet%spherical  = .false.
  success          = .true.
end function init_elementSet
!
!
!==============================================================================
! elementSet subroutines
!==============================================================================
function addElementSet_components(elementSets, x, y, kcs, spherical) result(id)
  !
  ! Return id when already present
  ! Create a new elementSet when not present and return id
  !
  ! result
  integer :: id
  !
  ! arguments
  type(ecElementSet), dimension(:), pointer    :: elementSets
  real(hp)          , dimension(:), intent(in) :: x
  real(hp)          , dimension(:), intent(in) :: y
  integer           , dimension(:), intent(in) :: kcs
  logical                         , intent(in) :: spherical
  !
  ! locals
  integer            :: i
  integer            :: istat
  integer            :: k
  integer            :: n       ! dimension of x,y,kcs
  logical            :: success ! ejs: success is only set
  type(ecElementSet) :: newSet
  !
  ! body
  id = getElementSet_elem(elementSets, x, y, kcs, spherical)
  if (id == 0) then
    !
    ! allocate a new one
    !
    id        = increaseSize_elementsets(elementSets)
    newSet%id = id
    n         = size(x)
                    allocate (newSet%x  (n), STAT = istat)
    if (istat == 0) allocate (newSet%y  (n), STAT = istat)
    if (istat == 0) allocate (newSet%kcs(n), STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      id      = 0
      call setECMessage("ERROR: ec_elementset::addElementSet_components: Unable to allocate additional memory")
      return
    endif
    !
    ! and fill with the given arrays
    !
    newSet%x         = x
    newSet%y         = y
    newSet%kcs       = kcs
    newSet%dim       = n
    newSet%spherical = spherical
    newSet%xy_based  = .true.
    !
    ! The assignment takes care of everything!
    !
    elementSets(id)  = newSet
  endif
end function addElementSet_components
!
!==============================================================================
function addElementSet_IDs(elementSets, IDs) result(id)
  !
  ! Return id when already present
  ! Create a new elementSet when not present and return id
  !
  ! result
  integer :: id
  !
  ! arguments
  type(ecElementSet), dimension(:), pointer    :: elementSets
  character(len=*)  , dimension(:), pointer    :: IDs
  !
  ! locals
  integer            :: i
  integer            :: istat
  integer            :: k
  integer            :: n       ! dimension of IDs
  logical            :: success ! ejs: success is only set
  type(ecElementSet) :: newSet
  !
  ! body
  id = getElementSet_IDs(elementSets, IDs)
  if (id == 0) then
    !
    ! allocate a new one
    !
    id        = increaseSize_elementsets(elementSets)
    newSet%id = id
    n         = size(IDs)
                    allocate (newSet%IDs(n), STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      id      = 0
      call setECMessage("ERROR: ec_elementset::addElementSet_IDs: Unable to allocate additional memory")
      return
    endif
    !
    ! and fill with the given arrays
    !
    newSet%IDs       = IDs
    newSet%dim       = n
    newSet%xy_based  = .false.
    !
    ! The assignment takes care of everything!
    !
    elementSets(id)  = newSet
  endif
end function addElementSet_IDs
!
!
!==============================================================================
function getElementSet_elem(elementSets, x, y, kcs, spherical) result(id)
  !
  ! Return id when already present
  !
  ! result
  integer :: id
  !
  ! arguments
  type(ecElementSet), dimension(:), pointer :: elementSets
  real(hp), dimension(:), intent(in)        :: x
  real(hp), dimension(:), intent(in)        :: y
  integer , dimension(:), intent(in)        :: kcs
  logical               , intent(in)        :: spherical
  !
  ! locals
  logical  :: found
  integer  :: i
  integer  :: k
  integer  :: kk
  integer  :: n       ! dimension of x,y,kcs
  integer  :: nn
  !
  ! body
  id = 0
  n  = size(x)
  kk = size(elementSets)
  !
  do k = 1,kk
    nn = size(elementSets(k)%x)
    if (nn == n) then
      found = .true.
      do i = 1,nn
        if (x(i)   /= elementSets(k)%x(i)   ) then
          found = .false.
          exit
        endif
        if (y(i)   /= elementSets(k)%y(i)   ) then
          found = .false.
          exit
        endif
        if (kcs(i) /= elementSets(k)%kcs(i) ) then
          found = .false.
          exit
        endif
      enddo
      if (found .and. (elementSets(k)%spherical .eqv. spherical)) then
        id = k
        exit
      endif
    endif
  enddo
end function getElementSet_elem
!
!==============================================================================
function getElementSet_IDs(elementSets, IDs) result(id)
  !
  ! Return id when already present
  !
  ! result
  integer :: id
  !
  ! arguments
  type(ecElementSet), dimension(:), pointer :: elementSets
  character(len=*)  , dimension(:), pointer :: IDs
  !
  ! locals
  logical  :: found
  integer  :: i
  integer  :: k
  integer  :: kk
  integer  :: n       ! dimension of x,y,kcs
  integer  :: nn
  !
  ! body
  id = 0
  n  = size(IDs)
  kk = size(elementSets)
  !
  do k = 1,kk
    nn = size(elementSets(k)%x)
    if (nn == n) then
      found = .true.
      do i = 1,nn
        if (IDs(i) /= elementSets(k)%IDs(i)   ) then
          found = .false.
          exit
        endif
      enddo
    endif
  enddo
end function getElementSet_IDs
!
!
!==============================================================================
function addElementSet_elementSet(elementSet) result(elementSetId)
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
  elementSetId = 0
end function addElementSet_elementSet
!
!
!==============================================================================
function increaseSize_elementSets(elementSets) result (k)
  !    Note that the allocated arrays are transferred from the old array of
  !    element sets to the new one. No need to free them
  !
  ! result
  integer :: k
  !
  ! arguments
  type(ecElementSet), dimension(:), pointer :: elementSets
  !
  ! locals
  integer                                   :: istat
  logical                                   :: success
  type(ecElementSet), dimension(:), pointer :: newElSets
  !
  ! body
  k = size(elementSets) + 1
  allocate(newElSets(k), STAT = istat)
  if (istat == 0) then
    success = .true.
  else
    success = .false.
    k       = 0
    call setECMessage("ERROR: ec_elementset::increaseSize_elementSets: Unable to allocate additional memory")
    return
  endif
  success = copyArray_elementSets(elementSets,newElSets)
  if (.not. success) k=0
  if (associated(elementSets)) deallocate(elementSets, STAT = istat)
  elementSets => newElSets
end function increaseSize_elementSets
!
!
!==============================================================================
function copyArray_elementSets(sourceArr, targetArr) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(ecElementSet), dimension(:), pointer :: sourceArr
  type(ecElementSet), dimension(:), pointer :: targetArr
  !
  ! locals
  integer :: k
  integer :: arrLenSource
  integer :: arrLenTarget
  logical :: retVal
  !
  ! body
  success = .true.
  arrLenSource = min(size(sourceArr), size(targetArr))
  arrLenTarget = size(targetArr)
  do k = 1,arrLenSource
     targetArr(k) = sourceArr(k)
  enddo
  do k = arrLenSource+1, arrLenTarget
     retVal = init_elementSet(targetArr(k))
     if(.not. retVal) success=.false.
  enddo
end function copyArray_elementSets
!
!
!==============================================================================
function getElementSetId(elementSet) result (id)
  !
  ! result
  integer :: id
  !
  ! arguments
  type(ecElementSet) :: elementSet
  !
  ! body
  id = elementSet%id
end function getElementSetId
!
!
!==============================================================================
function getElementSetDim(elementSet) result (dim)
  !
  ! result
  integer :: dim
  !
  ! arguments
  type(ecElementSet) :: elementSet
  !
  ! body
  dim = elementSet%dim
end function getElementSetDim
!
!
!==============================================================================
subroutine dump_ElementSet(elementSet)
  !
  ! arguments
  type(ecElementSet) :: elementSet
  !
  ! body
  write (*,'(a,i0)') '    id       : ', elementSet%id
  write (*,'(a,i0)') '    dim      : ', elementSet%dim
  if (elementSet%xy_based) then
     write (*,'(a,l1)') '    xy_based : ', .true.
     write (*,'(a,l1)') '    spherical: ', elementSet%spherical
  else
     write (*,'(a,l1)') '    ID_based : ', .true.
  endif
end subroutine dump_ElementSet
!
!
!==============================================================================
function CompareIds(set1, i, set2, j) result (compared)
  !
  ! result
  logical           :: compared
  !
  ! arguments
  type(ecElementSet) :: set1, set2
  integer            :: i, j
  !
  ! body
  compared = (set1%IDs(i) == set2%IDs(j))
end function CompareIds
!
end module ec_elementset
