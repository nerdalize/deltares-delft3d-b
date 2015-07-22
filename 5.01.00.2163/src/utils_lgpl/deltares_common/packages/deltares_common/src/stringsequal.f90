function stringsequalinsens(string1, string2) result(retval)
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
!  $Id: stringsequal.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/deltares_common/packages/deltares_common/src/stringsequal.f90 $
!!--description-----------------------------------------------------------------
!
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! return value
!
    logical                                     :: retVal  !  .true.  if strings are equal
                                                           !  .false. if strings are not equal or len1 /= len2
!
! arguments
!
    character(*)                  , intent(in)  :: string1 ! incoming string1
    character(*)                  , intent(in)  :: string2 ! incoming string2
!                                               
! locals                                        
!                                               
    integer                                     :: len1    ! length of string1, without trailing blanks
    integer                                     :: len2    ! length of string2, without trailing blanks
    integer                                     :: lenmin  ! minimum of the two lengths
    character(999) , dimension(:) , allocatable :: locstr1 ! copy of string1, to convert to lowercase
    character(999) , dimension(:) , allocatable :: locstr2 ! copy of string2, to convert to lowercase
!
!! executable statements -------------------------------------------------------
!
    retval = .false.
    len1   = len_trim(string1)
    len2   = len_trim(string2)
    lenmin = min(len1, len2)
    !
    ! Strings must be of the same length
    !
    if (len1 /= len2) then
       retval = .false.
       return
    endif
    !
    ! Local copy of the strings needed
    !
    allocate (locstr1(1))
    allocate (locstr2(1))
    locstr1 = ' '
    locstr2 = ' '
    !
    ! Strings will be compared upto lenmin
    !
    locstr1(1)(1:lenmin) = string1(1:lenmin)
    call small(locstr1(1), lenmin)
    !
    locstr2(1)(1:lenmin) = string2(1:lenmin)
    call small(locstr2(1), lenmin)
    !
    if (locstr1(1)(1:lenmin) == locstr2(1)(1:lenmin)) then
       !
       ! Strings are equal upto lenmin
       !
       retval = .true.
    else
       !
       ! Strings are not equal
       !
       retval = .false.
    endif
    deallocate (locstr1)
    deallocate (locstr2)
end function stringsequalinsens
