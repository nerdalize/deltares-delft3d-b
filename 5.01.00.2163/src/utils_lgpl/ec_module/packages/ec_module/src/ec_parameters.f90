module ec_parameters
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
!  $Id: ec_parameters.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/ec_module/packages/ec_module/src/ec_parameters.f90 $
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
  use ec_message
  !
  implicit none
  public
!
! Module parameters
!
  real(hp), parameter :: notime_default = -999.0_hp
  real(fp), parameter :: nodata_default = -999.0_fp   ! Default missing value in meteo arrays
!
! Module variables
!
  real(fp) :: pi
  real(fp) :: d2r

contains
!
!
!==============================================================================
function initECParameters() result(success)
  !
  ! result
  logical :: success
  !
  ! body
  pi      = acos(-1.0_fp)
  d2r     = pi/180.0_fp
  success = .true.
end function initECParameters
!
!
!==============================================================================
subroutine addUniqueInt(intArr, anInt)
!
! intArr is array containing unique integers
! anInt must be added to intArr, only when it is not already in intArr
  !
  ! arguments
  integer, dimension(:), pointer :: intArr
  integer                        :: anInt
  !
  ! locals
  integer                        :: i
  integer                        :: istat
  integer                        :: lenArr
  integer, dimension(:), pointer :: newIntArr
  !
  ! body
  if (associated(intArr)) then
    lenArr = size(intArr)
    do i=1, lenArr
      if (intArr(i) == anInt) then
        ! This integer is already in intArr
        return
      endif
    enddo
  else
    lenArr = 0
  endif
  !
  ! This integer is not yet in intArr
  ! Add it
  !
  allocate(newIntArr(lenArr+1), STAT = istat)
  if (istat /= 0) then
    nullify(intArr)
    call setECMessage("ERROR: ec_parameters::addUniqueInt: Unable to allocate additional memory")
    return
  endif
  do i=1, lenArr
    newIntArr(i) = intArr(i)
  enddo
  newIntArr(lenArr+1) = anInt
  if (lenArr /= 0) then
    deallocate(intArr, STAT = istat)
  endif
  intArr => newIntArr
end subroutine addUniqueInt



end module ec_parameters
