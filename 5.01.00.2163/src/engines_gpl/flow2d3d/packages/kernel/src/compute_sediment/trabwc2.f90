subroutine trabwc2(utot      ,di        ,taub      ,par       ,sbot      , &
                 & ssus      ,dg        ,dgsd      ,chezy     )
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
!  $Id: trabwc2.f90 1983 2012-11-16 14:24:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/trabwc2.f90 $
!!--description-----------------------------------------------------------------
!
!  Computes sediment transport according to Wilcock and Crowe formula modified to use 
!  geometric standard deviation of sediment particle size distribution as a parameter
!  for scaling the dimensionless reference shear stress rather than the proportion of 
!  sand on the bed surface as in the original immplementation. This approach is
!  described in the 2009 paper by Gaueman et al "Predicting fractional bed load 
!  transport rates: Application of the Wilcock-Crowe equations to a regulated gravel
!  bed river" Water Resources Research. See equations 15, 16a and 16b.
!  
!  The original Wilcock-Crowe sediment transport formula is presented in Wilcock and 
!  Crowe "Surface based transport for mixed size sediment", Journal of Hydraulic 
!  Engineering, Feb 2003
! 
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    implicit none
!
! Global variables
!
    real(fp)               , intent(in)  :: utot   ! flow velocity
    real(fp)               , intent(in)  :: di     ! Grain size specified as d50
    real(fp)               , intent(in)  :: taub   ! bed shear stress [N/m2]
    real(fp)               , intent(out) :: sbot   ! bed load transport, magnitude [m3/m/s]
    real(fp)               , intent(out) :: ssus   ! suspended sediment transport
    real(fp)               , intent(in)  :: dg     ! geometric mean surface grain size [m]
    real(fp)               , intent(in)  :: dgsd   ! geometric standard deviation of particle size mix [m]
    real(fp)               , intent(in)  :: chezy  ! local Chézy value [m1/2/s]
    real(fp), dimension(30), intent(in)  :: par    ! sediment parameter list
!
! Local variables
!
    real(fp) :: ag
    real(fp) :: rhosol
    real(fp) :: rhow
    real(fp) :: delta  ! relative density of sediment particle
    real(fp) :: thco   ! user specified calibration coefficient Theta c0 - set equal to 0.021 for standard formulation
    real(fp) :: ao     ! user specified calibration coefficient Alpha 0 - set equal to 0.33 for standard formulation
    real(fp) :: ustar  ! shear velocity (m/s)
    real(hp) :: wistar ! 
    real(hp) :: phi    ! 
    real(hp) :: taurm  ! 
    real(hp) :: tauri  ! 
    real(hp) :: b      ! 
    real(hp) :: sag    ! 
!
!! executable statements -------------------------------------------------------
!
    sbot  = 0.0_fp
    ssus  = 0.0_fp
    if (chezy < 1.0e-6_fp) then
        return
    endif
    if (dg < 1.0e-6_fp) then
        return
    endif
    ag     = par(1)
    rhow   = par(2)
    rhosol = par(3)
    delta  = par(4)      ! delta = (rhosol - rhow) / rhow = (rhosol / rhow) - 1
    thco   = par(11)     ! user specified calibration coeficient
    ao     = par(12)     ! user specified calibration coeficient
    sag    = sqrt(ag)
    ustar  = sag * utot / chezy
    b       = (1.0_fp - ao) / (1.0_fp + exp(1.5_fp - (di / dg)))
    taurm   = (thco + 0.015_fp / (1.0_fp + exp(10.1_fp * dgsd - 14.14_fp))) * (rhosol - rhow) * ag * dg
    tauri   = taurm * (di / dg)**b
    phi     = taub / tauri
    if (phi < 1.35_fp) then
        wistar  = 0.002_fp * phi**7.5_fp
    else
        wistar  = 14.0_fp * (1.0_fp - (0.894_fp / sqrt(phi)))**4.5_fp
    endif
    ! bed load magnitude [m3/m/s]
    sbot    =  wistar * ustar**3 / (delta * ag)
    ! note: proportion of size fraction on surface (fi) is included elsewhere
end subroutine trabwc2
