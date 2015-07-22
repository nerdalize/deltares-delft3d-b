subroutine compbsskin (umean , vmean , depth , wave  , uorb  , tper  , &
                     & teta  , kssilt, kssand, thcmud, taumax, rhowat, &
                     & vicmol)
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
!  $Id: compbsskin.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/compbsskin.f90 $
!!--description-----------------------------------------------------------------
!
! Compute tau in case of muddy bed (skin fraction  only)
! Soulsby  (2004)
!
! Remarks: 1) uorb based upon Hrms, Tper = peak period wave spectrum
!          2) skin friction comuted and used everywhere in the model
!             (not yet a distinction based upon md layer thickness or
!              mudcontent)
!          4) umean and vmean based upon u0eul, v0eul (inclding wave mass fluxes)
!          5) ar = 0.26; as = 0.22 (original publication soulsby)
!          6) taumax or taum used in erosion/sedimentation computations ????????
!          7) Smooth bed formulations explode in case of
!             small orbital velocities (0.0001 m/s). Set orbital velocity to
!             at least 1 cm/s
!          8) Mutiple mud fractions, each fraction own kssilt and kssand?????
!
!          03/10/2005: instead of mudcnt thcmud is used
!                      thcmud  < 0.01 then use kssand!!!!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
!
! Local parameters
!
    real(fp), parameter :: localeps = 1.0e-12_fp
!
! Global variables
!
    real(fp), intent(in)  :: umean  ! depth averaged flow velocity in u-direction
    real(fp), intent(in)  :: vmean  ! depth averaged flow velocity in v-direction
    real(fp), intent(in)  :: depth  ! local water depth
    real(fp), intent(in)  :: uorb   ! orbital velocity based upon Hrms
    real(fp), intent(in)  :: tper   ! wave period
    real(fp), intent(in)  :: teta   ! angle between wave direction and local
                                    ! grid orientation
    real(fp), intent(in)  :: kssilt ! roughness height silt
    real(fp), intent(in)  :: kssand ! roughness height sand
                                    !(not yet used)
    real(fp), intent(in)  :: thcmud ! Total hickness of mud layers
                                    !(to be replaced by mudcnt in future)
    real(fp), intent(out) :: taumax ! resulting (maximum) bed shear stress muddy silt bed
    logical , intent(in)  :: wave   ! wave impacts included in flow comp. or not
    real(fp), intent(in)  :: rhowat ! water density
    real(fp), intent(in)  :: vicmol ! molecular viscosity
!
! Local variables
!
    real(fp) :: ar      ! constant rough  bed turbulent flow
    real(fp) :: as      ! constant smooth bed turbulent flow
    real(fp) :: z0silt  ! roughness height
    real(fp) :: umod    ! magnitude depth averaged flow velocity
    real(fp) :: uorbm   ! maximum uorb and 1 cm/s
    real(fp) :: phicur  ! angle beteen mean flow and local grid orientation
    real(fp) :: phiwr   ! angle beteen flow and wave direction
    real(fp) :: aorb    ! orbital displacement
    real(fp) :: rec     ! Reynolds number flow
    real(fp) :: rew     ! Reynolds number waves
    real(fp) :: cdr     ! Drag coefficient rough  turbulent flows
    real(fp) :: cdm     ! Mean drag coefficient (current)
    real(fp) :: cds     ! Drag coefficient smooth turbulent flows
    real(fp) :: cdmax   ! Drag coefficient (current + waves)
    real(fp) :: fws     ! Wave friction coeefficient smooth turbulent flows
    real(fp) :: fwr     ! Wave friction coeefficient rough  turbulent flows
    real(fp) :: reccr   ! Critcal Reynolds number current
    real(fp) :: rewcr   ! Critcal Reynolds number waves
    real(fp) :: taum    ! Mean shear stress
    real(fp) :: tauw    ! Shear stress (waves)
    real(fp) :: taumr   ! Mean shear stress (rough  bed)
    real(fp) :: taums   ! Mean shear stress (smooth bed)
    real(fp) :: taumaxr ! Maximum shear stress (rough  bed)
    real(fp) :: taumaxs ! Maximum shear stress (smooth bed)
    real(fp) :: t1      ! Help variable
    real(fp) :: t2      ! Help variable
    real(fp) :: t3      ! Help variable
    real(fp) :: a1      ! Help variable
    real(fp) :: a2      ! Help variable
!
!! executable statements -------------------------------------------------------
!
    !
    ! Set constants
    !
    ar    = 0.26
    as    = 0.22
    rewcr = 1.5e5
    !
    ! Compute basic parameters
    !
    umod   = max( sqrt(umean*umean + vmean*vmean) , localeps )
    !
    if (thcmud > 0.01) then
       z0silt = max( kssilt/30.0 , localeps )
    else
       z0silt = max( kssand/30.0 , localeps )
    endif
    !
    rec    = umod * depth / vicmol
    cds    = 1.615e-4 * exp(6.0 * rec**(-0.08))
    cdr    = ( 0.40 / (log(depth/z0silt)-1.0) )**2
    phicur = atan2(vmean,umean) / degrad
    if (phicur < 0.0) phicur = phicur + 360.0
    !
    if (wave) then
       phiwr  = (teta - phicur) * degrad
       uorbm  = max( uorb , 0.01_fp )
       aorb   = uorbm * tper / 2.0 / pi
       rew    = uorbm * aorb / vicmol
       fws    = 0.0521 * rew**(-0.187)
       fwr    = 1.39 * (aorb/z0silt)**(-0.52)
    endif
    !
    ! Determine flow regime
    !
    if (umod > 1.0e-6 .and. .not. wave) then
       !
       ! Flow only
       !
       if (rec <= 2000.) then
          taum   = 3.0 * rhowat * vicmol * umod / depth
          taumax = taum
       else
          if (cdr >= cds) then
             taum = rhowat * cdr * umod * umod
          else
             taum = rhowat * cds * umod * umod
          endif
          taumax = taum
       endif
    elseif (umod < 1.0e-6 .and. wave) then
       !
       ! Waves only
       !
       taum = 0.0
       if (rew <= rewcr) then
          taumax = rhowat * uorbm * uorbm / sqrt(rew)
       else
          if (fwr >= fws) then
             taumax = 0.5 * rhowat * fwr * uorbm * uorbm
          else
             taumax = 0.5 * rhowat * fws * uorbm * uorbm
          endif
       endif
    elseif (umod >= 1.0e-6 .and. wave) then
       !
       ! Combined flow and waves
       !
       reccr = 2000.0 + (5.92e5 * rew)**0.35
       if (rec <= reccr .and. rew <= rewcr) then
          !
          ! laminar flow
          !
          taum   = 3.0 * rhowat * vicmol * umod / depth
          tauw   = rhowat * uorbm * uorbm / sqrt(rew)
          taumax = sqrt((taum +  tauw*abs(cos(phiwr)))**2 &
                       &      + (tauw*abs(sin(phiwr)))**2 )
       else
          !
          ! turbulent flow
          !
          ! 1) compute shear stresses belonging with rough bed
          !
          t1      = max(12.0_fp , ar * sqrt(fwr/2.0) * (aorb/z0silt))
          t2      = depth / (t1 * z0silt)
          t3      = (cdr*cdr + (fwr/2.0)**2 * (uorbm/umod)**4)**0.25
          !
          a1      = t3 * (log(t2) - 1.0) / (2.0*log(t1))
          a2      = 0.40 * t3 / log(t1)
          cdm     = (sqrt(a1*a1 + a2) - a1)**2
          cdmax   = sqrt((cdm +  t3*(uorbm/umod)*sqrt(fwr/2.0)*abs(cos(phiwr)))**2 &
                         &    + (t3*(uorbm/umod)*sqrt(fwr/2.0)*abs(sin(phiwr)))**2 )
          taumr   = rhowat * cdm   * umod * umod
          taumaxr = rhowat * cdmax * umod * umod
          !
          ! 2) compute shear stresses belonging with smooth bed
          !
          t1      = 9.0 * as * rew * sqrt(fws/2.0) &
                  & * (cds*cds*(umod/uorbm)**4 + (fws/2.0)**2)**0.25
          t2      = ( (rec/rew)*(uorbm/umod)*sqrt(2.0/fws) ) / as
          t3      = (cds*cds + (fws/2.0)**2 * (uorbm/umod)**4)**0.25
          !
          a1      = t3 * (log(t2) - 1.0) / (2.0*log(t1))
          a2      = 0.40 * t3 / log(t1)
          cdm     = (sqrt(a1*a1 + a2) - a1)**2
          cdmax   = sqrt((cdm +  t3*(uorbm/umod)*sqrt(fws/2.0)*abs(cos(phiwr)))**2 &
                         &    + (t3*(uorbm/umod)*sqrt(fws/2.0)*abs(sin(phiwr)))**2)
          taums   = rhowat * cdm   * umod * umod
          taumaxs = rhowat * cdmax * umod * umod
          !
          ! 3) determine shear stresses
          !
          if (taumaxs > taumaxr) then
             taum   = taums
             taumax = taumaxs
          else
             taum   = taumr
             taumax = taumaxr
          endif
       endif
    else
       !
       ! No flow and no waves
       !
       taum   = 0.0
       taumax = 0.0
    endif
end subroutine compbsskin
