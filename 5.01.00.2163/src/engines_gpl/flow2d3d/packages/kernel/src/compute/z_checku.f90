subroutine z_checku(j         ,nmmaxj    ,nmmax     ,icx       ,kmax      , &
                  & flood     ,kfu       ,kcs       ,kcu       ,kspu      , &
                  & kfumin    ,kfumx0    ,hu        ,s0        ,dpu       , &
                  & dps       ,umean     ,kfuz0     ,kfsmin    ,kfsmx0    , &
                  & u0        ,dzu0      ,zk        ,gdp       )
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
!  $Id: z_checku.f90 2110 2013-01-17 15:32:14Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/z_checku.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: This routine checks the drying and flooding at ve-
!              locity points and sets the value of the mask
!              arrays to zero.
!              Always upwind-approach for wet cross section.
!
! Method used: Upwind-approach for wet cross section.
!              Fixed layer approach
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
     real(fp)     , pointer :: dryflc
     real(fp)     , pointer :: dzmin
     real(fp)     , pointer :: zbot
     logical      , pointer :: ztbml
     logical      , pointer :: zmodel
     logical      , pointer :: nonhyd
     integer      , pointer :: nh_level
!
! Global variables
!
    integer                                                         :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                              !!  then computation proceeds in the X-
                                                                              !!  dir. If icx=1 then computation pro-
                                                                              !!  ceeds in the Y-dir.
    integer                                                         :: j      !!  Begin pointer for arrays which have
                                                                              !!  been transformed into 1D arrays.
                                                                              !!  Due to the shift in the 2nd (M-)
                                                                              !!  index, J = -2*NMAX + 1
    integer                                                         :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nmmax  !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: kfuz0  !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: kspu   !  Description and declaration in esm_alloc_int.f90
    logical                                           , intent(in)  :: flood  !!  Flag for activating flooding part of
                                                                              !!  checku subroutine
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dpu    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: hu     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: s0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(0:kmax)                     , intent(in)  :: zk
!
! Local variables
!
    integer  :: k
    integer  :: kkmin
    integer  :: kkmax
    integer  :: nm
    integer  :: nmd
    integer  :: nmu
    real(fp) :: dzutot
    real(fp) :: hnm
    real(fp) :: htrsh
    real(fp) :: s1u
    real(fp) :: trsh
    logical  :: found
!
!! executable statements -------------------------------------------------------
!
    dzmin    => gdp%gdzmodel%dzmin
    zbot     => gdp%gdzmodel%zbot
    ztbml    => gdp%gdzmodel%ztbml
    dryflc   => gdp%gdnumeco%dryflc
    zmodel   => gdp%gdprocs%zmodel
    nonhyd   => gdp%gdprocs%nonhyd
    nh_level => gdp%gdnonhyd%nh_level
    !
    htrsh = 0.5_fp * dryflc
    trsh  = dryflc
    !
    kkmin = 0
    kkmax = 0
    umean = 0.0_fp
    !
    do nm = 1, nmmax
       if (kcu(nm) /= 0) then
          !
          ! First determine umean for the top layer(s) of cells NM and NMU
          ! to find the upwind direction
          !
          !nmu   = nm + icx
          !
          ! Determine layers participating in velocity point
          !
          !kkmin = min(kfsmx0(nm), kfsmx0(nmu))
          !kkmax = max(kfsmx0(nm), kfsmx0(nmu))
          !
          ! kkmin below the bottom in NM or NMU?
          !
          !kkmin = max( kkmin, max(kfsmin(nm), kfsmin(nmu)) )
          !kkmax = max( kkmax, kkmin)
          !
          if (kfu(nm) == 1) then
             hnm = 0.0_fp
             !do k = kkmin, kkmax
             do k = kfumin(nm), kfumx0(nm)
                umean(nm) = umean(nm) + u0(nm,k)*dzu0(nm,k)
                hnm       = hnm + dzu0(nm,k)
             enddo
             umean(nm) = umean(nm) / max(hnm, 0.01_fp)
          else
          endif
       endif
    enddo
    !
    !   For NH-model without ADI: UMEAN = u0(nm,kfumx0(nm)), Z_MOMCOR
    !
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
             & s0        ,dpu       ,umean     ,hu        ,gdp       )
    do nm = 1, nmmax
       nmd = nm - icx
       nmu = nm + icx
       if (kcu(nm) /= 0) then
          !
          ! check for drying
          !
          if (kfu(nm)*hu(nm) < htrsh) then
             kfu(nm) = 0
             do k = kfumin(nm), kmax
                kfuz0(nm, k) = 0
             enddo
          endif
          !
          ! check for flooding
          !
          if (flood) then
             !
             if (kfu(nm) == 0) then
                !
                if ( hu(nm) > trsh &
                   & .and. max(s0(nm),s0(nmu)) - max(-real(dps(nm),fp),-real(dps(nmu),fp)) >= trsh) then
                   !
                   ! set kfuz0 for the flooded points
                   !
                   kfu(nm) = 1
                   !
                   ! Set some parameters in the points which are flooded
                   ! s1u is used for setting kfumx0
                   !
                   s1u = max(s0(nm), s0(nmu))
                   if (umean(nm) >= 0.001_fp) then
                      s1u = s0(nm)
                   elseif (umean(nm) <= - 0.001_fp) then
                      s1u = s0(nmu)
                   else
                   endif
                   !
                   ! for couple points
                   !
                   if ((kcu(nmd)>=1) .and. (kcu(nm)==3)) then
                      s1u = s0(nm)
                   elseif ((kcu(nmu)>=1) .and. (kcu(nm)==3)) then
                      s1u = s0(nmu)
                   else
                   endif
                   !
                   ! Set kfumx0 in flooded points,using s1u
                   !
                   found = .false.
                   do k = kfumin(nm), kmax
                      if (.not. found .and. zk(k) + dzmin>=s1u .or. &
                          & (s1u>zk(kmax) .and. k==kmax  )) then
                         kfumx0(nm) = k
                         found = .true.
                      endif
                   enddo
                   !
                   ! determine layer thickness new wet point. We assume that only one or two layers can be active
                   !
                   dzutot = 0.0_fp
                   do k = kfumin(nm), kfumx0(nm)
                       kfuz0(nm,k) = 1
                       if (kfumin(nm) == kfumx0(nm)) then
                          dzu0(nm, k) = hu(nm)
                       elseif (k == kfumin(nm)) then
                          dzu0(nm, k) = zk(k) + dpu(nm)
                       elseif (k == kfumx0(nm)) then
                          if (nonhyd .and. nh_level==nh_full) then
                             dzu0(nm, k) = min(zk(k)-zk(k-1), hu(nm)-dzutot)
                          else
                             dzu0(nm, k) = hu(nm) - dzutot
                          endif
                       else
                          dzu0(nm, k) = zk(k) - zk(k - 1)
                       endif
                       dzutot = dzutot + dzu0(nm, k)
                    enddo
                    !
                    ! Reset dzu0 to zero otherwise
                    !
                    do k = kfumx0(nm) + 1, kmax
                       dzu0(nm, k) = 0.0_fp
                    enddo
                    !
                    ! If requested by keyword ZTBML 
                    ! (Z-model TauBottom Modified Layering)
                    ! --> modify the near-bed layering to obtain smoother bottom shear stress representation in z-layer models
                    !
                    if (ztbml) then
                       if (kfumx0(nm) > kfumin(nm)) then
                          k             = kfumin(nm)
                          dzu0(nm, k  ) = 0.5_fp*(dpu(nm)+min(zk(k+1),s1u))
                          dzu0(nm, k+1) = dzu0(nm,k)
                       endif
                    endif
                    !
                    ! A "trick" to ensure that "wet" cells that were dry
                    ! obtain a velocity
                    !
                    if (kfumx0(nm) > kfumin(nm)) then
                       do k = max(kfumx0(nm),kfumin(nm)), kfumx0(nm)
                          u0(nm, k) = u0(nm, kfumin(nm))
                       enddo
                    endif
                 endif
             endif
          endif
       endif
    enddo
end subroutine z_checku
