subroutine stoktb(hrmsnm    ,tpu       ,h         ,ustokes   ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
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
!  $Id: stoktb.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/stoktb.f90 $
!!--description-----------------------------------------------------------------
!
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp), pointer :: ag
!
! Global variables
!
    real(fp)        :: h
    real(fp), intent(in)               :: hrmsnm
    real(fp)        :: tpu
    real(fp), intent(out)              :: ustokes
!
!
! Local variables
!
    real(fp)                       :: a
    real(fp)                       :: fact
    real(fp)                       :: kwav
    real(fp)                       :: omega
!
!
!! executable statements -------------------------------------------------------
!
    ag         => gdp%gdphysco%ag
    !
    !
    !-----Calculates Stokes velocities at the bottom
    !
    !
    !  declarations
    !
    !     GLOBAL DATA
    !
    !     global data structure definition and access functions
    !
    a = hrmsnm/2.
    if (tpu>0.1) then
       omega = 2.*pi/tpu
       !
       !-----Determine Wave number
       !
       call wavenr(h         ,tpu       ,kwav      ,ag        )
       !
       !-----Determine Second order Stokes drift at the bed (z=0)
       !
       fact = exp( - 2.*kwav*h)
       ustokes = omega*kwav*a**2*(fact*2/(1. - fact)**2)
    else
       ustokes = 0.
    endif
end subroutine stoktb
