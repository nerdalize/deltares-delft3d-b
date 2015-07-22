subroutine bedbc1993(tp        ,uorb      ,rhowat    ,h1        ,umod      , &
                   & zumod     ,d50       ,d90       ,z0cur     ,z0rou     , &
                   & dstar     ,taucr     ,aks       ,usus      ,zusus     , &
                   & uwb       ,delr      ,muc       ,tauwav    ,ustarc    , &
                   & tauc      ,taubcw    ,taurat    ,ta        ,ce_nm     , &
                   & dss       ,mudfrac   ,eps       ,aksfac    ,rwave     , &
                   & camax     ,rdc       ,rdw       ,iopkcw    ,iopsus    , &
                   & vonkar    ,wave      ,tauadd    )
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
!  $Id: bedbc1993.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/bedbc1993.f90 $
!!--description-----------------------------------------------------------------
!
! Compute bed roughness and shear stress parameters
! Van Rijn (1993,2000)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
!
! Global variables
!
    real(fp)              :: aks    !  Description and declaration in esm_alloc_real.f90
    real(fp)              :: ce_nm
    real(fp), intent(in)  :: d50
    real(fp), intent(in)  :: d90
    real(fp)              :: delr
    real(fp), intent(out) :: dss    !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: dstar
    real(fp), intent(in)  :: h1
    real(fp)              :: muc
    real(fp), intent(in)  :: mudfrac
    real(fp), intent(in)  :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp)              :: ta
    real(fp)              :: taubcw
    real(fp)              :: tauc
    real(fp), intent(in)  :: taucr
    real(fp), intent(out) :: taurat
    real(fp)              :: tauwav
    real(fp), intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)  :: umod
    real(fp), intent(in)  :: uorb   !  Description and declaration in esm_alloc_real.f90
    real(fp)              :: ustarc
    real(fp)              :: usus   !  Description and declaration in esm_alloc_real.f90
    real(fp)              :: uwb
    real(fp), intent(in)  :: z0cur
    real(fp), intent(in)  :: z0rou
    real(fp), intent(in)  :: zumod
    real(fp)              :: zusus
    real(fp), intent(in)  :: eps
    real(fp), intent(in)  :: aksfac
    real(fp), intent(in)  :: rwave
    real(fp), intent(in)  :: camax
    real(fp), intent(in)  :: rdc
    real(fp), intent(in)  :: rdw
    integer , intent(in)  :: iopkcw
    integer , intent(in)  :: iopsus
    real(fp), intent(in)  :: vonkar
    logical , intent(in)  :: wave
    real(fp), intent(in)  :: tauadd
!
! Local variables
!
    real(fp) :: awb
    real(fp) :: delm
    real(fp) :: delw
    real(fp) :: f1c
    real(fp) :: f1w
    real(fp) :: fc
    real(fp) :: fw
    real(fp) :: muw
    real(fp) :: muwa
    real(fp) :: ra
    real(fp) :: rc
    real(fp) :: rw
    real(fp) :: taucr1   ! critical shear stress corrected for mud fraction
!
!! executable statements -------------------------------------------------------
!
    delr = 0.0
    uwb  = 0.0
    !
    ! G. Lesser's implementation of Van Rijn's pick-up function for waves and currents.
    !
    ! Set Nikuradse roughness length using Z0CUR
    ! (current-only Z0 values transferred from TAUBOT).
    ! Expression limits aks to minimum of 0.01*h1 for accuracy
    !
    rc   = 30.*z0cur
    delr = 0.025
    !
    usus  = umod
    zusus = zumod
    !
    ! Inserted options for determining Rc and Rw
    !
    if (iopkcw==1) then
       if (wave) then
          !
          ! calculate wave-related roughness height from ripple height
          !
          ! note: rwave is a user-specified constant
          ! (in range 1-3 according to Van Rijn 1993)
          !
          rw = rwave*delr
          rw = min(rw, 0.1_fp)
          rw = max(rw, 0.01_fp)
       endif
    else
       rc = rdc
       if (wave) rw = rdw
    endif
    !
    ! Calculate Van Rijn's reference height
    !
    aks = max(aksfac*rc, 0.01*h1)
    !
    ! Adjust velocity to top of wave mixing layer and calculate other
    ! wave parameters (if waves are present)
    !
    tauwav = 0.0
    muwa   = 0.0
    muw    = 0.0
    !
    if (wave) then
       if (tp>0.0) then
          !
          ! Calculate apparent (enhanced) bed roughness ra
          !
          ! method of Van Rijn not implemented because it is more
          ! consistent to use the apparent roughness calculated by
          ! TAUBOT dependent on the chosen wave-current interaction
          ! model.
          !
          ra = 30.0*z0rou
          !
          ! still limit according to Van Rijn
          !
          ra = min(10.*rc, ra)
          !
          ! Calculate wave parameters
          !
          uwb = sqrt(2.0)*uorb
          awb = tp*uwb/(2.0*pi)
          !
          ! Note: need this check to avoid floating overflow errors in
          ! calculation of fw and f1w
          !
          awb = max(awb, eps)
          !
          ! Check aks height
          !
          aks = max(delr/2, aks)
          !
          ! Compute wave boundary laver thickness
          !
          delw = 0.072*awb*(awb/rw)**(-0.25)
          !
          ! Thickness of wave boundary mixing layer (Van Rijn (1993))
          !
          delm = 3.0*delw
          !
          ! Limit minimum delm thickness
          !
          delm = max(delm, ra/30)
          !
          ! Convert velocity to velocity at top of wave mixing layer,
          ! based on ENHANCED bed roughness
          ! Note that this means that Van Rijn's wave-current
          ! interaction factor alfacw is no longer required.
          ! Set this as the reference velocity and height
          !
          usus  = umod*log(1.0 + delm/z0rou)/log(1.0 + zumod/z0rou)
          zusus = delm
          !
          ! Calculate tauwav and muwa
          ! Calculate bed-shear stress due to waves
          !
          fw     = exp( - 6.0 + 5.2*(awb/rw)**( - 0.19))
          fw     = min(0.3_fp, fw)
          tauwav = 0.25*rhowat*fw*uwb**2
          !
          ! Calculate efficiency factor for waves
          ! (at reference level aks)
          !
          muwa = 0.6/dstar
          !
          ! And for bed-load slope effects
          !
          f1w = exp( - 6.0 + 5.2*(awb/(3*d90))**( - 0.19))
          muw = f1w/fw
       endif
    endif
    !
    ! Limit maximum aks to 20% of water depth
    ! (may be used when water depth becomes very small)
    !
    aks = min(aks, 0.2_fp * h1)
    !
    ! Calculate bed-shear stress due to currents
    ! Note: this expression uses the current-only roughness (z0 value)
    ! and is based on the velocity USUS at the height ZUSUS.
    ! Note that alfacw is not required in wave and current situations.
    !
    ustarc = usus*vonkar/log(1. + zusus/z0cur)
    tauc   = rhowat*ustarc**2
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
    ! Calculate efficiency factor currents
    !
    if (d90>0.0_fp) then
        f1c = 0.24*log10(12.0*h1/(3.0*d90))**( - 2)
    else
        f1c = 0.0_fp
    endif
    fc  = 0.24*log10(12.0*h1/rc)**( - 2)
    muc = f1c/fc
    !
    ! Calculate bed shear stress ratio for bed-load slope effects
    ! Note: this ignores bed-slope effects on initiation of motion
    !
    taubcw = muc*tauc + muw*tauwav
    taucr1 = taucr*(1.0 + mudfrac)**3
    taurat = taubcw/taucr1
    !
    ! Calculate Van Rijn's Dimensionless bed-shear stress for reference
    ! concentration at z=a
    !
    ta = max((muc*tauc + muwa*tauwav)/taucr1 - 1.0, 0.0_fp)
    !
    ! Equilibrium concentration at reference level aks
    ! following Van Rijn.
    !
    if (ta>eps) then
       ce_nm = 0.015*d50*ta**1.5/(aks*dstar**0.3)
       ce_nm = min(camax, ce_nm)
    else
       ce_nm = 0.0
    endif
    !
    ! Determination of suspended sediment size dss
    !
    if (iopsus==1) then
       if (ta<=1.0) then
          dss = d50*0.64
       elseif (ta>=25.0) then
          dss = d50
       else
          dss = d50*(1.0 + 0.015*(ta - 25.0))
       endif
    endif
end subroutine bedbc1993
