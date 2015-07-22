module ec_field
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
!  $Id: ec_field.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_field.f90 $
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
!
! parameters
!
!
!
! interfaces
!
  interface init_field
    module procedure init_field_val
    module procedure init_field_1d
    module procedure init_field_2d
    module procedure init_field_3d
  end interface
  interface setField
    module procedure setField_val
    module procedure setField_timval
  end interface
!
! public entities
!
  public :: tECField
  public :: init_field
  public :: setField
  public :: emptyField
contains
!
!
!==============================================================================
function init_field_val(field, elSetId, numDim, dim1Len, dim2Len, dim3Len) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECField)       , pointer      :: field
  integer              , intent(in)   :: elSetId
  integer              , intent(in)   :: numDim
  integer              , intent(in)   :: dim1Len
  integer    ,optional , intent(in)   :: dim2Len
  integer    ,optional , intent(in)   :: dim3Len
  !
  ! locals
  integer :: istat
  !
  ! body
  field%missingValue = -999.0_hp
  field%elementSetId = elSetid
  select case (numDim)
  case (1)
    allocate(field%arr3d(1, 1, dim1Len), STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      call setECMessage("ERROR: ec_field::init_field_val: Unable to allocate additional memory (1d)")
      return
    endif
    field%arr1d => field%arr3d(1, 1, :)
    field%arr2d => null()
  case (2)
    allocate(field%arr3d(dim1Len, dim2Len, 1), STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      call setECMessage("ERROR: ec_field::init_field_val: Unable to allocate additional memory (2d)")
      return
    endif
    field%arr1d => null()
    field%arr2d => field%arr3d(:, :, 1)
  case (3)
    allocate(field%arr3d(dim1Len, dim2Len, dim3Len), STAT = istat)
    if (istat == 0) then
      success = .true.
    else
      success = .false.
      call setECMessage("ERROR: ec_field::init_field_val: Unable to allocate additional memory (3d)")
      return
    endif
    field%arr1d => null()
    field%arr2d => null()
  case default
    success = .false.
  end select
  success = setField(field, real(field%missingValue,fp))
end function init_field_val
!
!
!==============================================================================
function init_field_1d(field, time, array, elSetId) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECField)        , pointer     :: field
  real(hp)              , intent(in)  :: time
  real(fp), dimension(:), pointer     :: array
  integer               , intent(in)  :: elSetId
  !
  ! locals
  !
  ! body
  field%time         = time
  field%arr1d        => array
  field%arr2d        => null()
  field%arr3d        => null()
  field%elementSetId = elSetid
  success            = .true.
end function init_field_1d
!
!
!==============================================================================
function init_field_2d(field, time, array, elSetId) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECField)          , pointer    :: field
  real(hp)                , intent(in) :: time
  real(fp), dimension(:,:), pointer    :: array
  integer                 , intent(in) :: elSetId
  !
  ! locals
  !
  ! body
  field%time         = time
  field%arr1d        => null()
  field%arr2d        => array
  field%arr3d        => null()
  field%elementSetId = elSetid
  success            = .true.
end function init_field_2d
!
!
!==============================================================================
function init_field_3d(field, time, array, elSetId) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECField)            , pointer    :: field
  real(hp)                  , intent(in) :: time
  real(fp), dimension(:,:,:), pointer    :: array
  integer                   , intent(in) :: elSetId
  !
  ! locals
  !
  ! body
  field%time         = time
  field%arr1d        => null()
  field%arr2d        => null()
  field%arr3d        => array
  field%elementSetId = elSetid
  success            = .true.
end function init_field_3d
!
!
!==============================================================================
function setField_val(field, value) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECField)           :: field
  real(fp)    , intent(in) :: value
  !
  ! locals
  !
  ! body
  ! No time specified; set to missing value
  success = setField(field, field%missingValue, value)
end function setField_val
!
!
!==============================================================================
function setField_timval(field, time, value) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECField)           :: field
  real(hp)    , intent(in) :: time
  real(fp)    , intent(in) :: value
  !
  ! locals
  !
  ! body
  field%time = time
  if (associated(field%arr3d)) then
    field%arr3d = value
  elseif (associated(field%arr2d)) then
    field%arr2d = value
  else
    field%arr1d = value
  endif
  success = .true.
end function setField_timval
!
!
!==============================================================================
function emptyField(field) result (success)
  !
  ! result
  logical :: success
  !
  ! arguments
  type(tECField) :: field
  !
  ! locals
  !
  ! body
  success = setField(field, field%missingValue, real(field%missingValue,fp))
end function emptyField




end module ec_field
