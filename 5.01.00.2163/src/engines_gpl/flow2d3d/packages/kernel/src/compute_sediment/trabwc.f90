subroutine trabwc(utot      ,di        ,taub      ,par       ,sbot      , &
                & ssus      ,dg        ,fs        ,chezy     )
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
!  $Id: trabwc.f90 1983 2012-11-16 14:24:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/trabwc.f90 $
!!--description-----------------------------------------------------------------
!
!  Computes sediment transport according to the Wilcock and Crowe sediment
!  transport formula. See: Wilcock and Crowe "Surface based transport for mixed 
!  size sediment", Journal of Hydraulic Engineering, Feb 2003
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
    real(fp)               , intent(in)  :: dg     ! mean surface grain size [m]
    real(fp)               , intent(in)  :: fs     ! sand fraction on surface
    real(fp)               , intent(in)  :: chezy  ! local Chézy value [m1/2/s]
    real(fp), dimension(30), intent(in)  :: par    ! sediment parameter list
!
! Local variables
!
    real(fp) :: ag
    real(fp) :: rhosol
    real(fp) :: rhow
    real(fp) :: delta  ! relative density of sediment particle
    real(fp) :: a
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
    delta  = par(4)      ! delta = (rhosol - rhow) / rhow = rhosol / rhow - 1
    a      = par(11)     ! alpha: calibration coeficient specified by user
    sag    = sqrt(ag)
    ustar  = sag * utot / chezy
    !ustar   = sqrt(taub / rhow)
    b       = 0.67_fp / (1.0_fp + exp(1.5_fp - (di / dg)))
    taurm   = (0.021_fp + 0.015_fp * exp(-20.0_fp * fs)) * (rhosol - rhow) * ag * dg
    tauri   = taurm * (di / dg)**b
    phi     = taub / tauri
    if (phi < 1.35_fp) then
        wistar  = 0.002_fp * phi**7.5_fp
    else
        wistar  = 14.0_fp * (1.0_fp - (0.894_fp / sqrt(phi)))**4.5_fp
    endif
    ! bed load magnitude [m3/m/s]
    sbot    = a * wistar * ustar**3 / (delta * ag)
    ! note: proportion of size fraction on surface (fi) is included elsewhere
end subroutine trabwc
