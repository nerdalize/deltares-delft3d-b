subroutine bedbc2004(tp        ,rhosol    ,rhowat    , &
                   & h1        ,umod      ,d10       ,zumod     ,d50       , &
                   & d90       ,z0cur     ,z0rou     ,drho      ,dstar     , &
                   & taucr0    ,u2dhim    ,aks       ,ra        ,usus      , &
                   & zusus     ,uwb       ,muc       ,tauwav    ,ustarc    , &
                   & tauc      ,taurat    ,ta        ,ce_nm     ,dss       , &
                   & uwc       ,uuu       ,vvv       ,rlabda    , &
                   & hrms      ,delw      ,uon       ,uoff      ,uwbih     , &
                   & delm      ,fc1       ,fw1       ,phicur    ,kscr      , &
                   & i2d3d     ,mudfrac   ,fsilt     ,taucr1    ,psi       , &
                   & dzduu     ,dzdvv     ,eps       ,camax     ,iopsus    , &
                   & ag        ,wave      ,tauadd    ,gamtcr    ) 
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
!  $Id: bedbc2004.f90 1294 2012-02-28 17:34:56Z ormondt $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/bedbc2004.f90 $
!!--description-----------------------------------------------------------------
!
! Compute bed roughness and shear stress parameters
! for based on TRANSPOR2004 formulations (Van Rijn and Walstra, 2004)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
    include "vanrijn.inc"
!
! Global variables
!
    integer, intent(in)   :: i2d3d
    real(fp)              :: aks    !  Description and declaration in esm_alloc_real.f90
    real(fp)              :: ce_nm
    real(fp)              :: d10
    real(fp)              :: d50
    real(fp)              :: d90
    real(fp)              :: delw
    real(fp), intent(out) :: drho
    real(fp), intent(out) :: dss    !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: dstar
    real(fp), intent(out) :: fc1
    real(fp), intent(out) :: fsilt
    real(fp), intent(in)  :: gamtcr
    real(fp)              :: h1
    real(fp)              :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: kscr
    real(fp)              :: muc
    real(fp), intent(in)  :: mudfrac
    real(fp), intent(out) :: phicur
    real(fp)              :: ra
    real(fp)              :: rc
    real(fp), intent(in)  :: rhosol !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp)              :: rlabda !  Description and declaration in esm_alloc_real.f90
    real(fp)              :: ta
    real(fp)              :: taubcw
    real(fp)              :: tauc
    real(fp), intent(in)  :: taucr0
    real(fp), intent(out) :: taucr1
    real(fp), intent(out) :: taurat
    real(fp)              :: tauwav
    real(fp)              :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: umod
    real(fp)              :: ustarc
    real(fp)              :: usus   !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: uuu
    real(fp)              :: uwb
    real(fp), intent(in)  :: vvv
    real(fp)              :: z0cur
    real(fp), intent(in)  :: z0rou
    real(fp), intent(in)  :: zumod
    real(fp)              :: zusus
    real(fp), intent(out) :: uon  
    real(fp), intent(out) :: uoff 
    real(fp), intent(out) :: uwbih
    real(fp), intent(out) :: psi
    real(fp), intent(in)  :: dzduu    !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: dzdvv    !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: eps
    real(fp), intent(in)  :: camax
    integer , intent(in)  :: iopsus
    real(fp), intent(in)  :: ag
    logical , intent(in)  :: wave
    real(fp), intent(in)  :: tauadd
!
! Local variables
!
    real(fp) :: a11  
    real(fp) :: alfacw
    real(fp) :: awb
    real(fp) :: cmax
    real(fp) :: cc
    real(fp) :: cmaxs
    real(fp) :: d50t
    real(fp) :: delm
    real(fp) :: fc
    real(fp) :: fch1
    real(fp) :: fclay
    real(fp) :: fpack
    real(fp) :: fw
    real(fp) :: fw1
    real(fp) :: kswr
    real(fp) :: muw
    real(fp) :: raih 
    real(fp) :: rmax 
    real(fp) :: rr
    real(fp) :: t1   
    real(fp) :: tt1
    real(fp) :: tt2
    real(fp) :: u1   
    real(fp) :: u2dhim
    real(fp) :: umax 
    real(fp) :: uwc
    real(fp) :: arg
    real(fp) :: hs
    real(fp) :: uhulp
    real(fp) :: rrr1
    real(fp) :: dzds
    real(fp) :: dzdn
    real(fp) :: fac_slp
    real(fp) :: phi
!
!! executable statements -------------------------------------------------------
!
    !
    ! VAN RIJN METHOD
    !
    ! Dimensionless density and grain size
    !
    drho  = (rhosol-rhowat) / rhowat
    uwb   = 0.0_fp
    usus  = umod
    zusus = zumod
    rc    = 30.0_fp * z0cur
    !
    ! calculate imaginary "depth-averaged current" which has a logarithmic
    ! velocity profile, and a velocity at the bottom zeta point equivalent
    ! to that calculated by the model for 3D current and waves.
    !
    if (i2d3d == 3) then
       u2dhim = (umod / h1 * ((h1+z0rou)*log(1.0_fp+h1/z0rou) - h1)) / log(1.0_fp+zumod/z0rou)
    else
       u2dhim = umod
    endif
    !
    ! calculate bed-shear stress due to currents
    !
    cc = 18.0_fp * log10(12.0_fp*h1/rc)
    ustarc = sqrt(ag) / cc * u2dhim
    !
    ! bed-shear stress current
    !
    fc   = 0.24_fp  * log10(12.0_fp*h1/rc)**(-2)
    tauc = 0.125_fp * rhowat * fc * u2dhim**2
    !
    if (tauadd>0.0_fp) then
       !
       ! extra stress
       !
       tauc   = sqrt(tauc**2 + tauadd**2)
       !
       ! update
       !
       ustarc = sqrt(tauc/rhowat)
    endif
    !
    phicur = atan2(vvv, uuu)
    if (phicur < 0.0_fp) then
       phicur = phicur + 2.0_fp*pi
    endif
    if (wave .and. tp>0.1_fp) then
       hs  = hrms*sqrt(2.0_fp)
       arg = 2.0_fp * pi * h1 / rlabda
       if (arg > 50.0_fp) then
          awb = 0.0_fp
          uwb = 0.0_fp
       else
          awb = hs / (2.0_fp * sinh(arg))
          uwb = 2.0_fp * pi / tp * awb
       endif
    else
       awb = 0.0_fp
       uwb = 0.0_fp
    endif
    awb = max(awb , 1.0e-6_fp)
    !
    ! wave parameters (if waves are present)
    !
    if (wave .and. tp>0.1_fp) then
       !
       ! kswr has same value as kscr (uncalibrated)
       !
       kswr = kscr
       !
       ! Calculate Van Rijn's reference height currents and waves (TR2004)
       !
       aks = max(0.5_fp*kscr , 0.5_fp*kswr , 0.01_fp)
       if (aks <= z0cur) then
          aks = z0cur + aks
       endif
       !
       ! calculate wave parameters
       !
       ! compute wave boundary laver thickness
       !
       delw = 0.36_fp * awb * (awb/kswr)**(-0.25_fp)
       !
       ! thickness of wave boundary mixing layer (acc. to Van Rijn & Walstra (2004))
       !
       delm = max(rc , min(0.2_fp , max(0.05_fp , 2.0*delw)))
       !
       ! method of Van Rijn not implemented because it is more consistent
       ! to use the apparent roughness calculated by TAUBOT dependant on the
       ! chosen wave-current interaction model.
       !
       ! But still limit according to Van Rijn
       ! ksc due to ripples, mega-ripples and dunes
       !
       ra = 30.0_fp * z0rou
       !
       ! convert velocity to velocity at top of wave mixing layer, based on
       ! ENHANCED bed roughness
       !
       ! Note that this means that Van Rijn's wave-current interaction factor
       ! alfacw is no longer required (G. Lesser)
       ! Van Rijn disagrees with above statement. In 2DH alfacw should be included!
       ! This is done some 20 lines below (Walstra)
       !
       ! set this as the reference velocity and height
       !
       usus  = umod * log(1.0_fp+delm/z0rou) / log(1.0_fp+zumod/z0rou)
       zusus = delm
       !
       ! calculate bed-shear stress due to waves
       !
       fw  = min(0.3_fp , exp(-6.0_fp + 5.2_fp*(awb/kswr)**(-0.19_fp)))
       !
       ! grain friction coefficient used in bedtr2004
       !
       fw1 = min(0.3_fp , exp(-6.0_fp + 5.2_fp*(awb/d90 )**(-0.19_fp)))
       !
       !   WAVE VELOCITY ASYMMETRY ACCORDING TO ISOBE-HORIKAWA
       !   (modified from tr2004 code)
       !
       rr     = -0.4_fp*hs/h1 + 1.0_fp
       umax   = rr * 2.0_fp * uwb
       t1     = tp * sqrt(ag/h1)
       u1     = umax / sqrt(ag*h1)
       a11    = -0.0049_fp*t1**2 - 0.069_fp*t1 + 0.2911_fp
       raih   = max(0.5_fp  , -5.25_fp-6.1_fp*tanh(a11*u1 - 1.76_fp))
       rmax   = max(0.62_fp , min(0.75_fp, -2.5_fp*h1/rlabda + 0.85_fp) )
       !
       uon    = umax * (0.5_fp+(rmax-0.5_fp)*tanh((raih-0.5_fp)/(rmax-0.5_fp)))
       uoff   = umax - uon
       uon    = max(1.0e-5_fp , uon)
       uoff   = max(1.0e-5_fp , uoff)
       !
       uwbih  = (0.5_fp*uon**3.0_fp + 0.5_fp*uoff**3.0_fp)**(1.0_fp/3.0_fp)
       tauwav = 0.25_fp * rhowat * fw * uwbih**2
       !
       ! Updated muw expression in TR2004
       !
       muw = max(0.14_fp, min(0.35_fp, 0.7_fp/dstar))
       !
       ! In 2DH alfacw should be included according to Van Rijn
       ! In 3D alfacw=1.0 (but then vrdelm should also be based on near bed velocities)
       !
       tt1    = (log(30.0_fp*delm/ra) / log(30.0_fp*delm/rc))**2
       tt2    = ((-1.0_fp + log(30.0_fp*h1/rc))/(-1.0_fp + log(30.0_fp*h1/ra)))**2 
       alfacw = max(0.0_fp , min(1.0_fp , tt1*tt2))
       uhulp  = uwbih
       uwc    = uhulp**2 + u2dhim**2
       uwc    = sqrt(max(0.0_fp , uwc))
    else
       !
       ! Calculate Van Rijn's reference height currents only (TR2004)
       !
       ! kscr uses calibration factor from trachytopes
       !
       aks = max(0.5_fp*kscr , 0.01_fp)
       if (aks <= z0cur) then
          aks = z0cur + aks
       endif
       tauwav = 0.0_fp
       muw    = 0.0_fp
       alfacw = 1.0_fp
       delm   = 0.05_fp
       uwc    = u2dhim
       fw1    = 0.0_fp
       uon    = 0.0_fp
       uoff   = 0.0_fp
       ra     = rc
    endif
    d50t = max(0.0001_fp , min(d50 , 0.0005_fp))
    psi  = uwc**2 / (drho * ag * d50t)
    !
    ! calculate efficiency factor currents
    !
    fc1  = 0.24_fp  * log10(12.0_fp*h1/d90)**(-2)
    muc  = fc1 / fc
    !
    ! effective bed-shear stress current + waves
    !
    ! calculate bed shear stress ratio for bed-load slope effects
    ! note this ignores bed-slope effects on initiation of motion
    !
    taubcw = alfacw*muc*tauc + muw*tauwav
    !
    ! critical bed-shear stress
    !
    fclay = 1.0_fp
    fpack = 1.0_fp
    fch1  = 1.0_fp
    if (d50 < dsand) then
       cmaxs = 0.65_fp
       fch1  = max((dsand/d50)**gamtcr, 1.0_fp)
       cmax  = min(max((d50/dsand)*cmaxs , 0.05_fp) , cmaxs)
       fpack = min(cmax/cmaxs , 1.0_fp)
    else
       fclay = min((1.0_fp+mudfrac)**3, 2.0_fp)
    endif
    taucr1 = fpack * fch1 * fclay * taucr0
    taurat  = taubcw / taucr1
    !
    ! Make assumptions for friction angle
    !
    phi = 30.0_fp * degrad
    !
    ! bed slope effects on critical shear stress
    ! using Dey (2001) as modified by Van Rijn (Z4056)
    ! (approximation where Schocklitsch and Leitner factors are combined)
    ! positive values refer to downsloping beds
    !
    dzds    = (dzduu*uuu + dzdvv*vvv)/max(umod,1.0e-4_fp)
    dzdn    = abs(dzduu*vvv + dzdvv*uuu)/max(umod,1.0e-4_fp)
    fac_slp = max((1.0_fp-(atan(dzds)/phi)) , 0.001_fp)**0.75_fp * max((1.0_fp-(atan(dzdn)/phi)),0.001_fp)**0.37_fp
    !
    ! calculate Van Rijn's Dimensionless bed-shear stress for reference
    ! concentration at z=a
    !
    rrr1    = max(min(0.8_fp+0.2_fp*((taubcw/(taucr1*fac_slp)-0.8_fp)/1.2_fp) , 1.0_fp) , 0.8_fp )
    ta = (taubcw-rrr1*taucr1*fac_slp) / (taucr1*fac_slp)
    !
    ! Equilibrium concentration at reference level aks
    ! following Van Rijn.
    !
    fsilt = max(1.0_fp, dsand/d50)
    !
    if (ta > eps) then
       !
       ! Upper limit camax set to 0.65 in stead of 0.05
       ! 0.05 is official TR2004, but results seem to be reasonable when using 0.65
       !
       ce_nm = min(camax , 0.015_fp*fsilt*d50*ta**1.5_fp/(aks*dstar**0.3_fp))
    else
       ce_nm = 0.0_fp
    endif
    !
    !  Determination of suspended sediment size dss
    !
    if (iopsus == 1) then
       if (psi < 550.0_fp) then
          dss = (1.0_fp+0.0006_fp*(d50/d10-1.0_fp)*(psi-550.0_fp)) * d50
       else
          dss = d50
       endif
       dss = max(d10, dss)
       if (d50 < dsilt) then
          dss = d50
       endif
       dss = max(dss , 0.5_fp*dsilt)
    endif
end subroutine bedbc2004
