subroutine juldat(itdate, julday)
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
!  $Id: juldat.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/deltares_common/packages/deltares_common/src/juldat.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: returns julian day number of given date
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    implicit none
!
! Global variables
!
    integer , intent(in)  :: itdate !  Reference date
    integer , intent(out) :: julday !  Julian day number
!
!
! Local variables
!
    integer               :: id     ! Day part of ITDATE (dd) 
    integer               :: id1    ! Help var. to check calculation 
    integer               :: idate  ! Absolute date to avoid rounding errors 
    integer               :: imo    ! Month part of ITDATE (mm) 
    integer               :: iy     ! Year part of ITDATE (yyyy) 
    integer               :: j      ! Help var. to check calculation 
    integer               :: jdag   ! Calculation of julian day 
    integer               :: l      ! Help var. to check calculation 
    integer               :: m      ! Help var. to check calculation 
    integer               :: maand1 ! Help var. for calculation JDAG 
    integer               :: n      ! Help var. to check calculation 
!
!! executable statements -------------------------------------------------------
!
    !
    ! General initialisation
    !
    idate = abs(itdate)
    id     = mod(idate, 100)
    imo    = mod(idate, 10000)/100
    iy     = idate/10000
    iy     = sign(iy, itdate)
    !
    ! Calculate julian day assuming the given month is correct
    !
    maand1 = (imo - 14)/12
    jdag   = id - 32075 + 1461*(iy + 4800 + maand1)/4 + 367*(imo - 2 - maand1*12) &
           & /12 - 3*((iy + 4900 + maand1)/100)/4
    !
    ! Calculate backwards to test if this assumption is correct
    !
    l   = jdag + 68569
    n   = 4 * l / 146097
    l   = l - (146097*n + 3)/4
    j   = 4000 * (l + 1) / 1461001
    l   = l - 1461*j/4 + 31
    m   = 80 * l / 2447
    id1 = l - 2447*m/80
    l   = m / 11
    m   = m + 2 - 12*l
    j   = 100*(n-49) + j + l
    !
    ! Test if calculating is correct
    !
    if ((j /= iy) .or. (m /= imo) .or. (id1 /= id)) then
       julday = 0
    else
       julday = jdag
    endif
end subroutine juldat
