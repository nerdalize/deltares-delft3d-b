subroutine calseddf2004(ustarc    ,ws        ,tp        ,hrms      ,h1        , &
                      & seddif    ,kmax      ,sig       ,thick     ,dicww     , &
                      & tauwav    ,tauc      ,ltur      ,delw      ,rhowat    , &
                      & uwbih     ,aks       ,ce_nm     ,ce_nmtmp  ,deltas    , &
                      & akstmp    ,d50       ,salinity  ,ws0       ,fdamp     , &
                      & psi       ,epsbed    ,epsmax    ,epsmxc    ,epspar    , &
                      & eps       ,bed       ,vonkar    ,salmax    ,wave      )
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
!  $Id: calseddf2004.f90 1180 2012-01-13 17:05:48Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute_sediment/calseddf2004.f90 $
!!--description-----------------------------------------------------------------
!
! Compute sediment diffusion coefficient
! Van Rijn (2004)
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
    integer                    , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                    , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    real(fp)                   , intent(in)  :: psi
    real(fp)                                 :: fdamp
    real(fp)                                 :: aks
    real(fp)                                 :: akstmp
    real(fp)                                 :: ce_nm
    real(fp)                                 :: ce_nmtmp
    real(fp)                   , intent(in)  :: d50
    real(fp)                   , intent(in)  :: delw
    real(fp)                   , intent(in)  :: h1
    real(fp)                   , intent(in)  :: hrms   !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: salinity
    real(fp)                   , intent(in)  :: salmax
    real(fp)                   , intent(in)  :: tauc
    real(fp)                   , intent(in)  :: tauwav
    real(fp)                   , intent(in)  :: tp     !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: ustarc
    real(fp)                   , intent(in)  :: uwbih
    real(fp)                   , intent(in)  :: ws0
    real(fp), dimension(0:kmax), intent(in)  :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax), intent(out) :: seddif !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax), intent(in)  :: ws     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)  , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)  , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp)                   , intent(in)  :: eps
    real(fp)                   , intent(in)  :: bed
    real(fp)                   , intent(in)  :: vonkar
    logical                    , intent(in)  :: epspar
    logical                    , intent(in)  :: wave
!
! Local variables
!
    integer                     :: i
    integer                     :: k
    real(fp)                    :: beta
    real(fp)                    :: betaef
    real(fp)                    :: betaw
    real(fp)                    :: c
    real(fp)                    :: cckmax
    real(fp)                    :: cnew
    real(fp)                    :: cmax
    real(fp)                    :: cmaxs
    real(fp)                    :: deltas
    real(fp)                    :: efloc
    real(fp)                    :: epsbed
    real(fp)                    :: epsmax
    real(fp)                    :: epsmxc
    real(fp)                    :: es
    real(fp)                    :: esc
    real(fp)                    :: esw
    real(fp)                    :: fch2
    real(fp)                    :: ffloc
    real(fp)                    :: fhs
    real(fp)                    :: fhulp
    real(fp)                    :: gambr
    real(fp)                    :: fcc
    real(fp)                    :: ff
    real(fp)                    :: fi
    real(fp)                    :: hs
    real(fp)                    :: lci
    real(fp)                    :: ustw
    real(fp)                    :: z
    real(fp)                    :: zend
    real(fp), dimension(0:kmax) :: epscur
    real(fp), dimension(0:kmax) :: epswav
!
!! executable statements -------------------------------------------------------
!
    cmaxs = 0.65_fp
    cmax  = min(max((d50/dsand)*cmaxs , 0.05_fp) , cmaxs)
    ffloc = 1.0_fp
    fch2  = max(min(d50/(1.5_fp*dsand) , 1.0_fp) , 0.3_fp)
    !
    ! calculate vertical sediment diffusion coefficient
    !
    if (ustarc > eps) then
       !
       ! Beta factor assumed constant over the depth, using ws at
       ! water surface (approx. clear water). Beta limited to
       ! range 1 - 1.5 following Van Rijn (1993)
       !
       beta = 1.0_fp + 2.0_fp*(ws(1)/ustarc)**2
       beta = max(1.0_fp, beta)
       beta = min(1.5_fp, beta)
    else
       beta = 1.0_fp
    endif
    !
    epsmxc = vonkar * beta * ustarc
    if (tp>0.0_fp .and. wave) then
       hs = hrms * sqrt(2.0_fp)
       if (hs/h1 > 0.4_fp) then
          gambr = 1.0_fp + sqrt((hs/h1)-0.4_fp)
       else
          gambr = 1.0_fp
       endif
       deltas = min(0.2_fp , max(0.05_fp , 2.0_fp*gambr*delw))
       ustw   = sqrt(tauwav/rhowat)
       betaw  = min(1.5_fp , 1.0_fp+2.0_fp*(ws(1)/max(1.0e-5_fp , ustw))**2)
       !
       ! epsbed is now based on isobe-horikawa i.s.o. uwb
       ! including a damping factor as a function of the mobility parameter
       !
       fdamp  = max(0.1_fp , min(1.0_fp , sqrt(250.0_fp/psi)))
       epsbed = 0.018_fp * fdamp * betaw * gambr * deltas * uwbih
       epsmax = 0.035_fp * gambr * h1 * hs / tp
       epsmax = min(0.05_fp , max(epsbed , epsmax))
    else
       deltas = 0.05_fp
       epsbed = 0.0_fp
       epsmax = 0.0_fp
    endif
    !
    ! Determine height cell centre kmax
    !
    cckmax = (1.0_fp+sig(kmax)) * h1
    if (ltur==0 .or. ltur==1 .or. (epspar .and. wave)) then
       !
       ! if algebraic or K-L turbulence model
       ! if waves are active then calculate sediment mixing according to Van Rijn
       ! use Van Rijn's parabolic-linear mixing distribution for current-related
       ! mixing
       !
       ! if epspar=.true and waves=true then
       ! use Van Rijn's parabolic-linear mixing distribution for current-related
       ! mixing i.s.o. vertical sediment mixing values from K-epsilon turbulence model
       ! if waves are active then calculate sediment mixing according to Van Rijn
       ! use Van Rijn's parabolic-linear mixing distribution for current-related
       ! mixing
       !
       ! set vertical sediment mixing values for waves and currents at water surface
       !
       epswav(0) = epsmax
       epscur(0) = 0.25_fp * epsmxc * h1
       seddif(0) = sqrt(epscur(0)**2+epswav(0)**2)
       !
       ! loop through vertical
       !
       do k = 1, kmax
          !
          ! calculate level of lower cell interface
          !
          lci = (1.0_fp + sig(k) - thick(k)/2.0_fp) * h1
          if (lci >= 0.5_fp*h1) then
             epswav(k) = epsmax
             epscur(k) = 0.25_fp * epsmxc * h1
          elseif (lci > deltas) then
             epswav(k) = epsbed + (epsmax-epsbed) * (lci-deltas) / (0.5_fp*h1-deltas)
             epscur(k) = 0.25_fp * epsmxc * h1 * (1.0_fp-(1.0_fp-2.0_fp*lci/h1)**2)
          else
             epswav(k) = epsbed
             epscur(k) = 0.25_fp * epsmxc * h1 * (1.0_fp-(1.0_fp-2.0_fp*lci/h1)**2)
          endif
          !
          ! set vertical sediment mixing values for waves and currents
          !
          seddif(k) = sqrt(epscur(k)**2 + epswav(k)**2)
       enddo
    else
       !
       ! set vertical sediment mixing values from K-epsilon turbulence model
       ! note beta factor should only be applied to current-related mixing
       ! this is rather rough method to estimate the proportion of the mixing
       ! that is due to current.
       !
       if (tauwav+tauc > eps) then
          betaef = 1.0_fp + (beta-1.0_fp) * tauc / (tauwav+tauc)
       else
          betaef = beta
       endif
       do k = 0, kmax
          seddif(k) = dicww(k) * betaef
       enddo
    endif
    !
    if (cckmax>aks .and. ce_nm>1.0e-5_fp) then
       !
       ! If aks is lower than cell centre of kmax layer, compute new reference
       ! concentration in kmax centre by working upward from aks
       !
       c = ce_nm
       z = aks
       do i = 1, 20
          if (z < deltas) then
             esw = epsbed
          endif
          if (z>deltas .and. z<0.5_fp*h1) then
             esw = epsbed + (epsmax-epsbed) * ((z-deltas)/(0.5_fp*h1-deltas))
          endif
          esc = epsmxc * z * (1.0_fp-z/h1)
          es  = sqrt(esw**2 + esc**2)
          !
          ! fi : damping term
          !
          fi  = max(fch2*(1.0_fp+((c/cmaxs)**0.8_fp)-2.0_fp*((c/cmaxs)**0.4_fp)) , 0.01_fp)
          !
          ! fhs : hindered settling term
          !
          fhs = max(sqrt(1.0_fp-0.65_fp*c/cmax) , 0.01_fp)
          !
          ! ffloc : flocculation term
          !
          if (d50 < dsand .and. salinity>0.01_fp .and. salmax>0.0_fp) then
             fhulp = max(4.0_fp+log10(2.0_fp*c/cmax) , 1.0_fp)
             efloc = min(max(dsand/d50-1.0_fp , 0.0_fp) , 3.0_fp)
             ffloc = max(min(fhulp**efloc , 10.0_fp) , 1.0_fp)
             ffloc = (ffloc-1.0_fp) * min(1.0_fp , salinity/salmax) + 1.0_fp
          endif
          fcc  = -ws0 * c * fhs * ffloc / (es * fi)
          ff   = 1.0_fp / c * fcc
          zend = min(cckmax , aks*(cckmax/aks)**(real(i,fp)/20.0_fp))
          cnew = exp(log(c)+(zend-z)*ff)
          c    = max(cnew , 1.0e-6_fp)
          z    = zend
       enddo      
       akstmp   = cckmax
       ce_nmtmp = c
    endif
end subroutine calseddf2004
