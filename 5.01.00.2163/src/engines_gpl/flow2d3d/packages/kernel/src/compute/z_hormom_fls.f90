subroutine z_hormom_fls(nmmax     ,kmax      ,icx       , &
                      & icy       ,kcs       ,kcs45     ,kcscut    , &
                      & kfu       ,kfuz0     ,kfumin    ,kfumx0    ,kfv       , &
                      & kfvz0     ,kfsz0     ,kfsmin    ,kfsmx0    ,kspu      , &
                      & u0        ,v1        ,hu        ,kfvmin    ,kfvmx0    , &
                      & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
                      & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
                      & qxk       ,qyk       ,dzu0      ,dzv0      ,dzs0      , &
                      & ddk       ,umean     ,dps       ,s0        ,ua        , &
                      & ub        ,gdp       )
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
!  $Id: z_hormom_fls.f90 1044 2011-11-21 21:22:12Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20111115_13532_z-model_improvements_oss-merge/engines_gpl/flow2d3d/packages/kernel/src/compute/z_hormom_fls.f90 $
!!--description-----------------------------------------------------------------
!
! This subroutine solves the momentum equation with a
! conservative discretization.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    real(fp)     , pointer :: dgcuni
    real(fp)     , pointer :: dryflc
    logical      , pointer :: cstbnd
!
! Global variables
!
    integer                                                         :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                         :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: kmax   !  Description and declaration in iidim.f90
    integer                                                         :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kcs    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmx0 !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: kfsmin !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfu    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfv    !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumin !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfumx0 !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfvmin !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                      :: kfvmx0 !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)              :: kspu   !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kcs45
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: kcscut !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfsz0  !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfuz0  !  Description and declaration in iidim.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: kfvz0  !  Description and declaration in iidim.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: dps    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqiu  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gud    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guu    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guv    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: guz    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvd    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gvv    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvz    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: hu     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: s0     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean  !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk    !!  Internal work array, diagonal space at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzs0   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzu0   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dzv0   !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1     !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qxk    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qyk    !  Description and declaration in rjdim.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ua
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ub
!
! Local variables
!
    integer            :: idifc
    integer            :: idifd
    integer            :: idifu
    integer            :: k
    integer            :: kspu0k
    integer            :: kenm
    integer            :: ndm
    integer            :: ndmd
    integer            :: ndmu
    integer            :: nm
    integer            :: nmd
    integer            :: nmdd
    integer            :: nmu
    integer            :: num
    integer            :: numd
    integer            :: numu
    integer            :: nuum
    real(fp)           :: advecx
    real(fp)           :: advecy
    real(fp)           :: cun
    real(fp)           :: cup
    real(fp)           :: cuu
    real(fp)           :: cvn
    real(fp)           :: cvp
    real(fp)           :: cvv
    real(fp)           :: dgeta
    real(fp)           :: dgvnm
    real(fp)           :: geta
    real(fp)           :: gksi
    real(fp)           :: gvndm
    real(fp)           :: gvnm
    real(fp)           :: dpsu
    real(fp)           :: du1
    real(fp)           :: du2
    real(fp)           :: gsqi
    real(fp)           :: factor
    real(fp)           :: hl
    real(fp)           :: hr
    real(fp)           :: thick
    real(fp)           :: trsh
    real(fp)           :: uun
    real(fp)           :: uup
    real(fp)           :: uuu
    real(fp)           :: vvn
    real(fp)           :: vvp
    real(fp)           :: vvv
    real(fp)           :: svvv
    real(fp)           :: qxup
    real(fp)           :: qxdo
    real(fp)           :: qyup
    real(fp)           :: qydo
    real(fp), external :: ulim
    real(fp)           :: uvdgdy
    real(fp)           :: vvdgdx
!
!! executable statements -------------------------------------------------------
!
    dgcuni  => gdp%gdnumeco%dgcuni
    dryflc  => gdp%gdnumeco%dryflc
    cstbnd  => gdp%gdnumeco%cstbnd
    !
    trsh = dryflc
    !
    ! 45 degrees staircase boundary
    !
    do nm = 1, nmmax
       nmd = nm - icx
       ndm = nm - icy
       if (kcs(nm) > 0) then
          do k = kfsmin(nm), kfsmx0(nm)
             if (kcs45(nm, k) == 3) then
                v1(ndm, k) = -u0(nm, k) * guu(nm) / gvv(nm)
                u0(nmd, k) = -v1(nm, k) * gvv(nm) / guu(nm)
             elseif (kcs45(nm, k) == 6) then
                v1(ndm, k) = u0(nmd, k) * guu(nmd) / gvv(nm)
                u0(nm , k) = v1(nm , k) * gvv(nm ) / guu(nmd)
             elseif (kcs45(nm, k) == 9) then
                v1(nm , k) = u0(nm , k) * guu(nm ) / gvv(ndm)
                u0(nmd, k) = v1(ndm, k) * gvv(ndm) / guu(nm)
             elseif (kcs45(nm, k) == 12) then
                v1(nm, k) = -u0(nmd, k) * guu(nmd) / gvv(ndm)
                u0(nm, k) = -v1(ndm, k) * gvv(ndm) / guu(nmd)
             else
             endif
          enddo
       endif
    enddo
    !
    do nm = 1, nmmax
       if (kfu(nm) == 1) then
          nmd    = nm  - icx
          nmdd   = nmd - icx
          ndm    = nm  - icy
          ndmd   = nm  - icx - icy
          nmu    = nm  + icx
          num    = nm  + icy
          nuum   = num + icy
          numu   = nm  + icx + icy
          ndmu   = nm  + icx - icy
          numd   = nm  - icx + icy
          do k = kfumin(nm), kfumx0(nm)
             if (kfuz0(nm, k)==1 .and. kcs(nm)*kcs(nmu)>0) then
                !
                ! Compute UA (appr. of velocity in waterlevel points) at internal points
                ! At open boundary UA == U0 for inflow
                !
                du2         = (u0(nm,k) - u0(nmd,k)) * kfuz0(nm,k) * kfuz0(nmd,k)
                if (kspu(nm,0) > 0 .or. kspu(nmd,0) > 0) then
                   du2      = 0.0_fp
                endif
                if (qxk(nm,k) + qxk(nmd,k) > 0.0_fp) then
                   du1      = (u0(nmd,k)-u0(nmdd,k)) * kfuz0(nmd,k) * kfuz0(nmdd,k)
                   if (kspu(nmd,0) > 0 .or. kspu(nmdd,0) > 0 ) then
                      du1   = 0.0_fp
                   endif
                   ua(nm,k) =  u0(nmd,k) + ulim(du1,du2)*du1
                else
                   du1      = (u0(nmu,k)-u0(nm,k)) * kfuz0(nmu,k) * kfuz0(nm,k)
                   if (kspu(nm,0) > 0 .or. kspu(nmu,0) > 0 ) then
                      du1   = 0.0_fp
                   endif
                   ua(nm,k) =  u0(nm,k)  - ulim(du1,du2)*du1
                endif
                !
                ! Compute UB (appr. of velocity in depth points) at internal points
                ! At open boundary UB == U0 for inflow
                ! In case thin dams in the transverse direction let UB = 0
                !
                if (kfv(nm)*kfv(nmu) /= 0) then
                   if (qyk(nm,k) + qyk(nmu,k) > 0.0_fp) then
                      du1      = (u0(nm ,k)-u0(ndm,k)) * kfuz0(nm ,k) * kfuz0(ndm,k)
                      du2      = (u0(num,k)-u0(nm ,k)) * kfuz0(num,k) * kfuz0(nm ,k)
                      ub(nm,k) =  u0(nm ,k) + ulim(du1,du2)*du1
                   else
                      du1      = (u0(nuum,k)-u0(num,k)) * kfuz0(nuum,k) * kfuz0(num,k)
                      du2      = (u0(num ,k)-u0(nm ,k)) * kfuz0(num ,k) * kfuz0(nm ,k)
                      ub(nm,k) =  u0(num ,k) - ulim(du1,du2)*du1
                   endif
                endif
             endif
          enddo
       endif
    enddo
    !
    do nm = 1, nmmax
       !
       ! Check for domain decomposition points = kcs(nm)*kcs(nmu) > 0
       !
       nmu    = nm + icx
       if (kfu(nm) == 1 .and. kcs(nm)*kcs(nmu) > 0) then
          nmd    = nm - icx
          ndm    = nm - icy
          ndmd   = nm - icx - icy
          num    = nm + icy
          numu   = nm + icx + icy
          ndmu   = nm + icx - icy
          numd   = nm - icx + icy
          gksi   = gvu(nm)
          geta   = guu(nm)
          gvnm   = gvd(nm)
          gvndm  = gvd(ndm)
          dgeta  = guz(nmu) - guz(nm)
          dgvnm  = gvd(nm)  - gvd(ndm)
          gsqi   = gsqiu(nm)
          !
          hl     = max(real(dps(nm) ,fp) + s0(nm) , trsh)
          hr     = max(real(dps(nmu),fp) + s0(nmu), trsh)
          dpsu   = max(0.5_fp*(hl + hr), trsh)
          factor = 1.0_fp
          !
          ! Switch from momentum conservation to energy conservation
          ! is used to correct the advection fluxes at the end of the subroutine
          !
          if (       (umean(nm) > 0.0_fp .and. (hl > hr) .and. kfu(nmu) == 1) &
              & .or. (umean(nm) < 0.0_fp .and. (hr > hl) .and. kfu(nmd) == 1)  ) then
              if (       (real(dps(nm),fp)          > real(dps(nmu),fp)+ dgcuni) & 
                  & .or. (real(dps(nm),fp) + dgcuni < real(dps(nmu),fp)        )  ) then
                 factor = hr * hl / (dpsu*dpsu)
                 !
                 ! avoid factor becoming small (synchronised with SOBEK FLS)
                 !
                 factor = max (0.33_fp , factor)
              else
                 !
                 ! avoid dividing by zero
                 !
                 factor = 1.0_fp
              endif
          endif
          !
          do k = kfumin(nm), kfumx0(nm)
             kspu0k = kspu(nm, 0) * kspu(nm, k)
             if (kfuz0(nm, k)==1 .and. kspu0k /=4 .and. kspu0k /=10) then
                advecx = 0.0_fp
                advecy = 0.0_fp
                vvdgdx = 0.0_fp
                uvdgdy = 0.0_fp
                !
                ! Compute VVV
                !
                if (       (cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2)) &
                    & .or. (kcs(nm)==3 .or. kcs(nmu)==3               )  ) then
                   svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                   vvv = (v1(ndm, k)*kfv(ndm) + v1(ndmu, k)*kfv(ndmu) + v1(nm, k)  &
                       & *kfv(nm) + v1(nmu, k)*kfv(nmu))/svvv
                else
                   vvv = 0.25_fp*(v1(ndm, k) + v1(ndmu, k) + v1(nm, k) + v1(nmu, k))
                endif
                !
                ! ADVECTION IN U-DIRECTION; DU/DX AND CENTRIFUGAL ACCELERATION
                ! MOMENT - CONSERVATIVE LIMITER scheme
                ! No advection in the Y-direction along open boundary
                !
                qxup   = (qxk(nm ,k) + qxk(nmu ,k) )/ max(1, (kfuz0(nm ,k) + kfuz0(nmu ,k)) )
                qxdo   = (qxk(nmd,k) + qxk(nm  ,k) )/ max(1, (kfuz0(nmd,k) + kfuz0(nm  ,k)) )
                qyup   = (qyk(nm ,k) + qyk(nmu ,k) )/ max(1, (kfvz0(nm ,k) + kfvz0(nmu ,k)) )
                qydo   = (qyk(ndm,k) + qyk(ndmu,k) )/ max(1, (kfvz0(ndm,k) + kfvz0(ndmu,k)) )
                if (u0(nm,k) > 0.0_fp) then
                   thick  = max(dzs0(nm,k), trsh) / hl
                elseif (u0(nm,k) < 0.0_fp) then
                   thick  = max(dzs0(nmu,k), trsh) / hr
                else
                   thick  = max(dzs0(nm,k)/hl, dzs0(nmu,k)/hr)
                endif
                if (comparereal(ua(nm ,k),0.0_fp) /= 0 .and. &
                  & comparereal(ua(nmu,k),0.0_fp) /= 0        ) then
                   advecx = kfu(nmu)*kfu(nmd)*(qxup*ua(nmu,k) - qxdo*ua(nm,k) - u0(nm,k)*(qxup - qxdo)) * gsqi / (dpsu*thick)
                   !
                   ! CURVATURE TERM DUE TO CONVECTION IN U-DIRECTION
                   !
                   uvdgdy = kfu(nmu)*kfu(nmd)*0.5*vvv*gsqi*(gvnm - gvndm)
                endif
                if (comparereal(ub(ndm,k),0.0_fp) /= 0 .and. &
                  & comparereal(ub(nm ,k),0.0_fp) /= 0        ) then
                   advecy = kfu(num)*kfu(ndm)*(qyup*ub(nm,k) - qydo*ub(ndm,k) - u0(nm,k)*(qyup - qydo)) * gsqi / (dpsu*thick)
                   !
                   ! CURVATURE TERM DUE TO ADVECTION IN V-DIRECTION
                   !
                   vvdgdx = kfu(num)*kfu(ndm)*0.5*vvv*gsqi*(guu(nmu) - guu(nmd))
                endif
                advecy = advecy - vvv*vvdgdx
                advecx = advecx + u0(nm,k)*uvdgdy
                !
                ddk(nm, k) = ddk(nm, k) - advecx/factor - advecy/factor
             endif
          enddo
       endif
    enddo
end subroutine z_hormom_fls
