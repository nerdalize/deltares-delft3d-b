subroutine distance2(sferic    ,x1        ,y1        ,x2        ,y2        , &
                   & d12       )
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
!  $Id: distance2.f90 1817 2012-09-04 14:55:36Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/utils_lgpl/meteo/packages/meteo/src/distance2.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Calculates distance between two points on earth
! Method used: Circular distance when sferic is true,
!              Euclidic distance when sferic is false
!
!!--pseudo code and references--------------------------------------------------
!
! This subroutine is identical to subroutine distance in Delft3D-FLOW, except
! that this version is independent of GDP
!
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    logical, intent(in)   :: sferic !  true: spherical, false: cartesian coordinate system
    real(fp), intent(out) :: d12    !!  Calculated distance from 1 to 2
    real(fp), intent(in)  :: x1     !!  X coordinate of point 1 (deg or m)
    real(fp), intent(in)  :: x2     !!  X coordinate of point 2 (deg or m)
    real(fp), intent(in)  :: y1     !!  Y coordinate of point 1 (deg or m)
    real(fp), intent(in)  :: y2     !!  Y coordinate of point 2 (deg or m)
!
! Local variables
!
    real(hp) :: ddegrad
    real(hp) :: dearthrad
    real(hp) :: d128      ! Double precision d12 
    real(hp) :: phi       ! Angle 
    real(hp) :: x1rad     ! X1 in radials 
    real(hp) :: x2rad     ! X2 in radials 
    real(hp) :: y1rad     ! Y1 in radials 
    real(hp) :: y2rad     ! Y2 in radials 
!
!! executable statements -------------------------------------------------------
!
! TODO: CALL distance.f90 IN deltares_common INSTEAD OF DOUBLE CODE
!
    if (x1==x2 .and. y1==y2) then
       d12 = 0.0_fp
       return
    endif
    dearthrad = 6378137.0_hp
    ddegrad   = acos( - 1.0_hp)/180.0_hp
    if (sferic) then
       x1rad = real(x1, hp)*ddegrad
       x2rad = real(x2, hp)*ddegrad
       y1rad = real(y1, hp)*ddegrad
       y2rad = real(y2, hp)*ddegrad
       phi   = dcos(y1rad)*dcos(y2rad)*dcos(x1rad - x2rad) + dsin(y1rad)*dsin(y2rad)
       d128  = dearthrad*dacos(phi)
    else
       x1rad = real(x1, hp)
       x2rad = real(x2, hp)
       y1rad = real(y1, hp)
       y2rad = real(y2, hp)
       d128 = dsqrt((x2rad - x1rad)**2 + (y2rad - y1rad)**2)
    endif
    d12 = real(d128, fp)
end subroutine distance2
