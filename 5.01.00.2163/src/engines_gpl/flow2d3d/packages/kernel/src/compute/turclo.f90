subroutine turclo(j         ,nmmaxj    ,nmmax     ,kmax      ,ltur      , &
                & icx       ,icy       ,tkemod    , &
                & kcs       ,kfu       ,kfv       ,kfs       ,s0        , &
                & dps       ,hu        ,hv        ,u0        ,v0        , &
                & rtur0     ,thick     ,sig       ,rho       ,vicuv     , &
                & vicww     ,dicuv     ,dicww     ,windsu    ,windsv    , &
                & z0urou    ,z0vrou    ,bruvai    ,rich      ,dudz      , &
                & dvdz      ,ueul      ,veul      ,gdp       )
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
!  $Id: turclo.f90 2110 2013-01-17 15:32:14Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/turclo.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes eddy viscosity and eddy diffusivity.
!              dependent of closure model (ltur).
!              ltur=0    algebraic model
!              ltur=1    k-L model
!              ltur=2    k-epsilon model  (SANCTUM-model)
!              For ltur=1,2 transport equations are solved.
!              - For tkemod = 'Constant   ' user input is used
!
! Method used: Reference: R.E. Uittenbogaard, J.A.Th.M. van
!              Kester, G.S. Stelling, Implementation of three
!              turbulence models in 3D-FLOW for rectangular
!              grids, Deltares report Z81, april 1992)
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    real(fp)               , pointer :: z0
    real(fp)               , pointer :: z0v
    real(fp)               , pointer :: vonkar
    real(fp)               , pointer :: vicmol
    real(fp)               , pointer :: vicoww
    real(fp)               , pointer :: dicoww
    real(fp)               , pointer :: cmukl
    real(fp)               , pointer :: cmukep
    real(fp)               , pointer :: zwi
    real(fp)               , pointer :: ck
    integer                , pointer :: inpzw
!
! Global variables
!
    integer                                                   , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                   , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                                 :: j      !!  Begin pointer for arrays which have been transformed into 1D arrays. Due to the shift in the 2nd (M-) index, J = -2*NMAX + 1
    integer                                                   , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                   , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                                                 :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                 , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                 , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                 , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                 , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: hv     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: windsu !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: windsv !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)              , intent(in)  :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: bruvai !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: dicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: dudz   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: dvdz   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: rich   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)                    :: vicww  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur), intent(in)  :: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)                  :: dicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax + 2)                  :: vicuv  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: rho    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: ueul   !!  Eulerian velocity in X-direction (including Stokes drift)
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)        , intent(in)  :: veul   !!  Eulerian velocity in Y-direction (including Stokes drift)
    real(fp)  , dimension(kmax)                               , intent(in)  :: sig    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                               , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    character(12)                                             , intent(in)  :: tkemod !  Description and declaration in tricom.igs
!
! Local variables
!
    integer  :: kbg     ! denotes the k-index of vicuv/dicuv containing the background values
    integer  :: khtur   ! denotes the k-index of vicuv/dicuv containing the HLES values
    integer  :: k
    integer  :: kup
    integer  :: maskval
    integer  :: ndm
    integer  :: nm
    integer  :: nmd
    real(fp) :: aa
    real(fp) :: bb
    real(fp) :: difiwe
    real(fp) :: drhodz
    real(fp) :: epsd    ! Underbound denominator 
    real(fp) :: fl
    real(fp) :: fs
    real(fp) :: h0
    real(fp) :: h0i
    real(fp) :: rl
    real(fp) :: rz
    real(fp) :: shear
    real(fp) :: sigw
    real(fp) :: sqrtbv  ! Square root of Brunt Vaisly frequency BRUVAI(NM,K) 
    real(fp) :: tke
    real(fp) :: tkewin
    real(fp) :: tsg
    real(fp) :: ustbe
    real(fp) :: ustbot
    real(fp) :: ustwi
    real(fp) :: ustwin
    real(fp) :: ustwkw
    real(fp) :: uuu
    real(fp) :: zw
    real(fp) :: zwc
    integer  :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
    data epsd/1.E-20/
!
!! executable statements -------------------------------------------------------
!
    cmukl       => gdp%gdturcoe%cmukl
    cmukep      => gdp%gdturcoe%cmukep
    zwi         => gdp%gdturcoe%zwi
    ck          => gdp%gdturcoe%ck
    inpzw       => gdp%gdturcoe%inpzw
    rhow        => gdp%gdphysco%rhow
    ag          => gdp%gdphysco%ag
    z0          => gdp%gdphysco%z0
    z0v         => gdp%gdphysco%z0v
    vonkar      => gdp%gdphysco%vonkar
    vicmol      => gdp%gdphysco%vicmol
    vicoww      => gdp%gdphysco%vicoww
    dicoww      => gdp%gdphysco%dicoww
    !
    ! Initialization
    !
    kbg   = kmax + 1
    khtur = kmax + 2
    nm_pos= 1
    !
    ! THIS ROUTINE IS ONLY OF INTEREST IN CASE KMAX > 1 !!
    !
    if (kmax==1) then
       do nm = 1, nmmax
          vicuv(nm, 1) = vicuv(nm, kbg) + vicuv(nm, khtur)
          dicuv(nm, 1) = dicuv(nm, kbg) + dicuv(nm, khtur)
       enddo
       goto 2000
    endif
    !=======================================================================
    !
    ! VERTICAL EDDY VISCOSITIES/DIFFUSIVITIES AT LAYER INTERFACES
    !
    !     CONSTANT VALUES FOR EDDY VISCOSITY & DIFFUSIVITY
    !
    if (tkemod=='Constant   ') then
       do k = 0, kmax
          do nm = 1, nmmax
             vicww(nm, k) = vicoww
             dicww(nm, k) = dicoww
          enddo
       enddo
    else
       !=======================================================================
       !
       !     OTHER TURBULENCE MODELS
       !
       !=======================================================================
       !
       ! USE DICWW AS SCRATCH ARRAY TO COMPUTE 'Z0' IN
       !         WATER-LEVEL POINTS
       !
       nmd = -icx
       ndm = -icy
       do nm = 1, nmmax
          vicww(nm, kmax) = 0.0
          nmd = nmd + 1
          ndm = ndm + 1
          if (kfs(nm)==1) then
             dicww(nm, kmax) = (kfu(nm)*z0urou(nm) + kfu(nmd)*z0urou(nmd)       &
                             & + kfv(nm)*z0vrou(nm) + kfv(ndm)*z0vrou(ndm))     &
                             & /(kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
          endif
       enddo
       !
       !***production and buoyancy term (only vertical gradients)
       !
       do k = 1, kmax - 1
          kup = k + 1
          tsg = 0.5*(thick(k) + thick(kup))
          do nm = 1, nmmax
             dudz(nm, k) = 0.0
             if (kfu(nm)==1) then
                dudz(nm, k) = (ueul(nm, k) - ueul(nm, kup))/(hu(nm)*tsg)
             endif
          enddo
       enddo
       do k = 1, kmax - 1
          kup = k + 1
          tsg = 0.5*(thick(k) + thick(kup))
          do nm = 1, nmmax
             dvdz(nm, k) = 0.0
             if (kfv(nm)==1) then
                dvdz(nm, k) = (veul(nm, k) - veul(nm, kup))/(hv(nm)*tsg)
             endif
          enddo
       enddo
       !
       ! parallel case: exchange arrays in the overlapping cells
       !
       call dfexchg(dudz, 0, kmax, dfloat, nm_pos, gdp)
       call dfexchg(dvdz, 0, kmax, dfloat, nm_pos, gdp)
       do k = 1, kmax - 1
          kup = k + 1
          tsg = 0.5*(thick(k) + thick(kup))
          nmd = -icx
          ndm = -icy
          do nm = 1, nmmax
             nmd = nmd + 1
             ndm = ndm + 1
             if (kfs(nm)==1) then
                h0 = max(s0(nm) + real(dps(nm),fp), 0.01_fp)
                h0i = 1./h0
                drhodz = h0i*(rho(nm, k) - rho(nm, kup))/tsg
                if (kcs(nm)==3) then
                   maskval = kcs(nm) - 2
                else
                   maskval = kcs(nm)
                endif
                shear = maskval*0.5*(dudz(nm, k)**2 + dudz(nmd, k)              &
                      & **2 + dvdz(nm, k)**2 + dvdz(ndm, k)**2)
                if (shear<1E-8) shear = 1E-8
                bruvai(nm, k) = -ag*drhodz/rho(nm, k)
                rich(nm, k) = bruvai(nm, k)/shear
             endif
          enddo
       enddo
       !=======================================================================
       !
       !       ALGEBRAIC TURBULENCE MODEL (LTUR = 0)
       !
       !=======================================================================
       if (ltur==0) then
          do k = 1, kmax
             nmd = -icx
             ndm = -icy
             do nm = 1, nmmax
                nmd = nmd + 1
                ndm = ndm + 1
                if (kfs(nm)==1) then
                   h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
                   h0i = 1./h0
                   if (kcs(nm)==3) then
                      maskval = kcs(nm) - 2
                   else
                      maskval = kcs(nm)
                   endif
                   uuu = 0.5*maskval*sqrt(  (ueul(nm, k) + ueul(nmd, k))**2         &
                       &                  + (veul(nm, k) + veul(ndm, k))**2 )
                   !
                   ! bottom is assumed at Z0
                   !
                   rz = 1.0 + (1. + sig(k))*h0/dicww(nm, kmax)
                   vicww(nm, kmax) = vicww(nm, kmax) + uuu*vonkar/(log(rz)*kmax)
                endif
             enddo
          enddo
          !
          nmd = -icx
          ndm = -icy
          do nm = 1, nmmax
             nmd = nmd + 1
             ndm = ndm + 1
             if (kfs(nm)==1) then
                h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
                h0i = 1./h0
                if (kcs(nm)==3) then
                   maskval = kcs(nm) - 2
                else
                   maskval = kcs(nm)
                endif
                uuu = 0.5*maskval*sqrt(  (ueul(nm, kmax) + ueul(nmd, kmax))**2     &
                    &                  + (veul(nm, kmax) + veul(ndm, kmax))**2 )
                !
                ! bottom is assumed at Z0
                !
                rz = 1.0 + (1. + sig(kmax))*h0/dicww(nm, kmax)
                ustbot = uuu*vonkar/log(rz)
                vicww(nm, kmax) = max(vicww(nm, kmax), ustbot)
             endif
          enddo
          !
          !***KOLMOGOROV-PRANDTL MIXING LENGTH MODEL
          !
          sigw = 0.0
          do k = 1, kmax - 1
             sigw = sigw - thick(k)
             nmd = -icx
             ndm = -icy
             do nm = 1, nmmax
                nmd = nmd + 1
                ndm = ndm + 1
                if (kfs(nm)==1) then
                   h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
                   h0i = 1./h0
                   !
                   ! DAMPING FUNCTION MIXING LENGTH  (FL)
                   ! DAMPING FUNCTION DIFFUSIVITY    (FS)
                   !
                   if (rich(nm, k)>=0.0) then
                      fl = exp( - 2.3*min(rich(nm, k), 30.0_fp))
                      aa = (1. + 3.33*rich(nm, k))
                      aa = aa*sqrt(aa)
                      bb = (1. + 10.0*rich(nm, k))
                      bb = sqrt(bb)
                      fs = aa/bb
                   else
                      fl = (1. - 14.*rich(nm, k))**0.25
                      fs = 1.
                   endif
                   !
                   !***MIXING LENGTH
                   !
                   rl = vonkar*h0*(1. + sigw)*sqrt( - sigw)*fl
                   !
                   !***ALGEBRAIC EDDY VISCOSITY (UITTENBOGAARD '91 )
                   !
                   tkewin = sqrt(windsu(nm)**2 + windsv(nm)**2)
                   ustwi = sqrt(tkewin/rhow)
                   ustbe = vicww(nm, kmax)
                   tke = ustbe*ustbe*( - sigw)/sqrt(cmukep)                     &
                       & + ustwi*ustwi*(1. + sigw)/sqrt(cmukep)
                   vicww(nm, k) = cmukl*rl*sqrt(tke)
                   !
                   !***EDDY VISCOSITY IS MAXIMUM UITTENBOGAARD/KOLMOGOROV-PRANDTL
                   !
                   if (kcs(nm)==3) then
                      maskval = kcs(nm) - 2
                   else
                      maskval = kcs(nm)
                   endif
                   shear = 0.5*maskval*(dudz(nm, k)**2 + dudz(nmd, k)           &
                         & **2 + dvdz(nm, k)**2 + dvdz(ndm, k)**2)
                   shear = max(epsd, shear)
                   vicww(nm, k) = max(vicww(nm, k), rl*rl*sqrt(shear))
                   dicww(nm, k) = vicww(nm, k)/fs
                endif
             enddo
          enddo
       !=======================================================================
       !
       !    K-L MODEL (LTUR = 1)
       !
       !=======================================================================
       elseif (ltur==1) then
          sigw = 0.0
          do k = 1, kmax - 1
             sigw = sigw - thick(k)
             do nm = 1, nmmax
                if (kfs(nm)==1) then
                   h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
                   h0i = 1./h0
                   !
                   ! DAMPING FUNCTION MIXING LENGTH  (FL)
                   ! DAMPING FUNCTION DIFFUSIVITY    (FS)
                   !
                   if (rich(nm, k)>=0.0) then
                      fl = exp( - 2.3*min(rich(nm, k), 30.0_fp))
                   else
                      fl = (1. - 14.*rich(nm, k))**0.25
                   endif
                   fs = 1.
                   !
                   rl = vonkar*h0*(1. + sigw)*sqrt( - sigw)*fl
                   vicww(nm, k) = cmukl*rl*sqrt(rtur0(nm, k, 1))
                   dicww(nm, k) = cmukl*rl*sqrt(rtur0(nm, k, 1))/fs
                endif
             enddo
          enddo
       !=======================================================================
       !
       !   K-EPS MODEL (LTUR = 2)
       !
       !=======================================================================
       else
          do k = 1, kmax - 1
             do nm = 1, nmmax
                h0 = real(dps(nm),fp) + s0(nm)
                if (kfs(nm)==1 .and. h0>0.01_fp) then
                   vicww(nm, k) = cmukep*rtur0(nm, k, 1)                        &
                                & **2/max(rtur0(nm, k, 2), epsd)
                   dicww(nm, k) = vicww(nm, k)
                else
                   !
                   ! make vicww and dicww small for points
                   ! that are dry or almost dry
                   ! Actually, it would be more consistent to use
                   ! dicww(nm, k) = vicmol/sigmol
                   ! but sigmol depends on l and is not available here
                   ! So, just use
                   ! dicww(nm, k) = vicmol
                   !
                   vicww(nm, k) = vicmol
                   dicww(nm, k) = vicmol
                endif
             enddo
          enddo
       endif
       !
       ! bottom and free surface (for all turbulence models except constant)
       !
       nmd = -icx
       ndm = -icy
       do nm = 1, nmmax
          nmd = nmd + 1
          ndm = ndm + 1
          h0  = real(dps(nm),fp) + s0(nm)
          if (kfs(nm)==1 .and. h0>0.01_fp) then
             !
             zwc          = 0.5*thick(1)*h0
             zw           = inpzw*zwi + (1 - inpzw)*zwc
             ustwkw       = sqrt(windsu(nm)**2 + windsv(nm)**2)/rhow
             ustwin       = sqrt(ustwkw)
             vicww(nm, 0) = vonkar*zw*ustwin
             dicww(nm, 0) = vonkar*zw*ustwin
             !
             ! rtur can be used to determine viscosity at surface
             ! only for k-eps model
             !
             if (ltur==2) then
                vicww(nm, 0) = cmukep*rtur0(nm, 0, 1)                           &
                             & **2/max(rtur0(nm, 0, 2), epsd)
                dicww(nm, 0) = vicww(nm, 0)
             endif
             !
             ! bottom is assumed at Z0 (for all turbulence models execpt constant)
             !
             h0 = max(0.01_fp, real(dps(nm),fp) + s0(nm))
             if (kcs(nm)==3) then
                maskval = kcs(nm) - 2
             else
                maskval = kcs(nm)
             endif
             uuu = 0.5*maskval*sqrt(  (ueul(nm, kmax) + ueul(nmd, kmax))**2      &
                 &                  + (veul(nm, kmax) + veul(ndm, kmax))**2 )
             rz = 1.0 + (1. + sig(kmax))*h0/dicww(nm, kmax)
             ustbot = abs(uuu)*vonkar/log(rz)
             vicww(nm, kmax) = vonkar*ustbot*dicww(nm, kmax)
             dicww(nm, kmax) = vonkar*ustbot*dicww(nm, kmax)
          else
             !
             ! make vicww and dicww small for points
             ! that are dry or almost dry (for k=kmin and k=kmax)
             ! Actually, it would be more consistent to use
             ! dicww(nm, k) = vicmol/sigmol
             ! but sigmol depends on l and is not available here
             ! So, just use
             ! dicww(nm, k) = vicmol
             !
             vicww(nm, 0)    = vicmol
             dicww(nm, 0)    = vicmol
             vicww(nm, kmax) = vicmol
             dicww(nm, kmax) = vicmol
          endif
       enddo
       !
       ! The following vicww restriction is moved to UZD, CUCNP:
       ! vicww(nm,k) = max(vicww(nm,k), vicoww)
       ! The following dicww restriction is moved to DIFU, DIFUVL:
       ! dicww(nm,k) = max(dicww(nm,k), dicoww)
       !
       do k = 0, kmax
          do nm = 1, nmmax
             if (kcs(nm)/=0) then
                vicww(nm, k) = min(vicww(nm, k), 10.0_fp)
                dicww(nm, k) = min(dicww(nm, k), 10.0_fp)
             endif
          enddo
       enddo
    endif
    !
    ! parallel case: exchange arrays in the overlapping cells
    !
    call dfexchg(vicww, 0, kmax, dfloat, nm_pos, gdp)
    call dfexchg(dicww, 0, kmax, dfloat, nm_pos, gdp)
    !
    !***HORIZONTAL EDDY VISCOSITIES AND DIFFUSIVITIES
    !   IN DENSITY POINTS
    !
    do k = 1, kmax
       do nm = 1, nmmax
          if (kfs(nm)==1) then
             vicuv(nm, k) = 0.5 * (vicww(nm, k) + vicww(nm, k-1)) &
                          & + vicuv(nm, kbg) + vicuv(nm, khtur)
             dicuv(nm, k) = 0.5 * (dicww(nm, k) + dicww(nm, k-1)) &
                          & + dicuv(nm, kbg) + dicuv(nm, khtur)
          endif
       enddo
    enddo
    !
 2000 continue
end subroutine turclo
