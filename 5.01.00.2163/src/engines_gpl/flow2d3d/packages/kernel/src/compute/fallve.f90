subroutine fallve(kmax      ,nmmax     ,lsal      ,ltem      ,lsed      , &
                & kcs       ,kfs       ,aak       ,u0        ,v0        , &
                & wphy      ,r0        ,rtur0     ,ltur      ,thick     , &
                & saleqs    ,temeqs    ,rhowat    ,ws        ,dss       , &
                & icx       ,icy       ,lundia    ,dps       ,s0        , &
                & umean     ,vmean     ,z0urou    ,z0vrou    ,kfu       , &
                & kfv       ,gdp       )
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
!  $Id: fallve.f90 1294 2012-02-28 17:34:56Z ormondt $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/fallve.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Relation between sediment concentration
!              and vertical fall velocity. Model for
!              hindered settling.
!              Fall velocity at layer interfaces.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    include "vanrijn.inc"
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                         , pointer :: ag
    real(fp)                         , pointer :: vonkar
    real(fp)                         , pointer :: vicmol
    real(fp)                         , pointer :: csoil
    real(fp)           , dimension(:), pointer :: rhosol
    real(fp)           , dimension(:), pointer :: ws0
    real(fp)           , dimension(:), pointer :: wsm
    real(fp)           , dimension(:), pointer :: salmax
    real(fp)           , dimension(:), pointer :: sedd50
    real(fp)           , dimension(:), pointer :: sedd50fld
    integer,             dimension(:), pointer :: iform
    integer            , dimension(:), pointer :: sedtyp
    real(fp)                         , pointer :: timsec
    real(fp)           , dimension(:), pointer :: gamflc
    !
    character(256)     , dimension(:), pointer :: dll_function
    integer(pntrsize)  , dimension(:), pointer :: dll_handle
    character(256)     , dimension(:), pointer :: dll_usrfil
    !
    integer                          , pointer :: max_integers
    integer                          , pointer :: max_reals
    integer                          , pointer :: max_strings
    integer            , dimension(:), pointer :: dll_integers
    real(hp)           , dimension(:), pointer :: dll_reals
    character(256)     , dimension(:), pointer :: dll_strings
    include 'sedparams.inc'
!
! Global variables
!
    integer                                                 , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX then computation proceeds in the X-dir. If icx=1 then computation proceeds in the Y-dir.
    integer                                                 , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                                 , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: lsal   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                                 , intent(in)  :: ltem   !  Description and declaration in dimens.igs
    integer                                                 , intent(in)  :: ltur   !  Description and declaration in dimens.igs
    integer                                                               :: lundia !  Description and declaration in inout.igs
    integer                                                 , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer   , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, *)               :: ws     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, 1:kmax)                  :: aak    !!  Internal work array
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(in)  :: rhowat !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(in)  :: u0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(in)  :: v0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)      , intent(in)  :: wphy   !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, kmax, *  ) , intent(in)  :: r0     !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub,0:kmax,ltur), intent(in)  :: rtur0  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub, lsed)      , intent(in)  :: dss    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: s0     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: umean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: vmean  !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: z0urou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(gdp%d%nmlb:gdp%d%nmub)            , intent(in)  :: z0vrou !  Description and declaration in esm_alloc_real.f90
    real(fp)  , dimension(kmax)                             , intent(in)  :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                , intent(in)  :: saleqs    
    real(fp)                                                , intent(in)  :: temeqs    
!
! Local variables
!
    integer(pntrsize)           :: error_ptr
    integer                     :: k
    integer                     :: kn
    integer                     :: ku
    integer                     :: l
    integer                     :: ll
    integer                     :: lst
    integer                     :: n
    integer                     :: m
    integer                     :: nm
    integer                     :: ndm
    integer                     :: nmd
    integer(pntrsize), external :: perf_function_fallve
    real(fp)                    :: a
    real(fp)                    :: b
    real(fp)                    :: chezy
    real(fp)                    :: coefw  
    real(fp)                    :: ee
    real(fp)                    :: h0  
    real(fp)                    :: hinset
    real(fp)                    :: rhoint
    real(fp)                    :: s
    real(fp)                    :: sag
    real(fp)                    :: salint
    real(fp)                    :: temint
    real(fp)                    :: ts
    real(fp)                    :: cgel
    real(fp)                    :: efloc
    real(fp)                    :: ffloc
    real(fp)                    :: ffloc0
    real(fp)                    :: fhulp
    real(fp)                    :: u
    real(fp)                    :: v
    real(fp)                    :: vcmol
    real(fp)                    :: w
    real(fp)                    :: um
    real(fp)                    :: vm
    real(fp)                    :: tur_k
    real(fp)                    :: tur_eps
    real(fp)                    :: z0rou
    real(hp)                    :: ws_dll
    character(256)              :: errmsg
    character(256)              :: message                 ! Contains message from shared library
!
!! executable statements -------------------------------------------------------
!
    ag                  => gdp%gdphysco%ag
    vonkar              => gdp%gdphysco%vonkar
    vicmol              => gdp%gdphysco%vicmol
    csoil               => gdp%gdsedpar%csoil
    rhosol              => gdp%gdsedpar%rhosol
    ws0                 => gdp%gdsedpar%ws0
    wsm                 => gdp%gdsedpar%wsm
    salmax              => gdp%gdsedpar%salmax
    sedtyp              => gdp%gdsedpar%sedtyp
    sedd50              => gdp%gdsedpar%sedd50
    sedd50fld           => gdp%gdsedpar%sedd50fld
    gamflc              => gdp%gdsedpar%gamflc
    iform               => gdp%gdeqtran%iform
    !
    timsec              => gdp%gdinttim%timsec
    !
    dll_function        => gdp%gdeqtran%dll_function_settle
    dll_handle          => gdp%gdeqtran%dll_handle_settle
    dll_usrfil          => gdp%gdeqtran%dll_usrfil_settle
    !
    max_integers        => gdp%gdeqtran%max_integers_settle
    max_reals           => gdp%gdeqtran%max_reals_settle
    max_strings         => gdp%gdeqtran%max_strings_settle
    dll_integers        => gdp%gdeqtran%dll_integers_settle
    dll_reals           => gdp%gdeqtran%dll_reals_settle
    dll_strings         => gdp%gdeqtran%dll_strings_settle
    !
    aak = 0.0_fp
    sag = sqrt(ag)
    ee  = exp(1.0_fp)
    !
    lst = max(lsal, ltem)
    do l = 1, lsed
       ll = lst + l
       !
       ! bulk mass concentration; UPWIND approach
       !
       do k = 1, kmax
          do nm = 1, nmmax
!            if (kfs(nm)==1 .and. kcs(nm)<=2) then
             if (kfs(nm)==1 .and. kcs(nm)>-1 .and. kcs(nm)<=2) then
                aak(nm, k) = aak(nm, k) + r0(nm, k, ll)
             endif
          enddo
       enddo
    enddo
    !
    do l = 1, lsed
       ll = lst + l
       !
       ! the settling velocity can be either be specified by the user, or it
       ! may dependent on the sediment type: 'sand' or 'mud'
       !
       if (dll_function(l) /= ' ') then
          !
          ! Settling velocity routine supplied by the user in a DLL
          !
          do k = 1, kmax
             ku = min(k + 1, kmax)
             ts = thick(k) + thick(ku)
             do nm = 1, nmmax
                if (kfs(nm)==0 .or. kcs(nm)>2) cycle
                nmd  = nm - icx
                ndm  = nm - icy
                !
                ! Input parameters are passed via dll_reals/integers/strings-arrays
                !
                if (lsal > 0) then
                   salint = (  thick(k) *r0(nm, ku, lsal) &
                          &  + thick(ku)*r0(nm, k , lsal)  ) / ts
                else
                   salint = saleqs
                endif
                !
                if (ltem > 0) then
                   temint = (  thick(k) *r0(nm, ku, ltem) &
                          &  + thick(ku)*r0(nm, k , ltem)  ) / ts
                else
                   temint = temeqs
                endif
                !
                rhoint = (thick(k)*rhowat(nm,ku) + thick(ku)*rhowat(nm,k)) / ts
                !
                u = (thick(k)*u0(nm ,ku) + thick(ku)*u0(nm ,k) + &
                   & thick(k)*u0(nmd,ku) + thick(ku)*u0(nmd,k)) / 2.0_fp / ts
                v = (thick(k)*v0(nm ,ku) + thick(ku)*v0(nm ,k) + &
                   & thick(k)*v0(ndm,ku) + thick(ku)*v0(ndm,k)) / 2.0_fp / ts
                w = (thick(k)*wphy(nm,ku) + thick(ku)*wphy(nm,k)) / ts
                !
                if (ltur>0) then
                   tur_k = rtur0(nm,k,1)
                else
                   tur_k = -999.0_fp
                endif
                if (ltur>1) then
                   tur_eps = rtur0(nm,k,2)
                else
                   tur_k = -999.0_fp
                endif
                !
                h0 = s0(nm) - real(dps(nm),fp)
                um = (umean(nm) + umean(nmd))/2.0_fp
                vm = (vmean(nm) + vmean(nmd))/2.0_fp
                !
                ! Calculate total (possibly wave enhanced) roughness
                !
                kn    = max(1, kfu(nm) + kfu(nmd) + kfv(nm) + kfv(ndm))
                z0rou = (  kfu(nmd)*z0urou(nmd) + kfu(nm)*z0urou(nm) &
                      &  + kfv(ndm)*z0vrou(ndm) + kfv(nm)*z0vrou(nm)  )/kn
                chezy = sag * log( 1.0_fp + h0/max(1.0e-8_fp,ee*z0rou) ) / vonkar
                !
                if (max_reals < 21) then
                   write(errmsg,'(a,a,a)') 'Insufficient space to pass real values to settling routine.'
                   call prterr (lundia,'U021', trim(errmsg))
                   call d3stop(1, gdp)
                endif
                dll_reals( 1) = real(timsec ,hp)
                dll_reals( 2) = real(u      ,hp)
                dll_reals( 3) = real(v      ,hp)
                dll_reals( 4) = real(w      ,hp)
                dll_reals( 5) = real(salint ,hp)
                dll_reals( 6) = real(temint ,hp)
                dll_reals( 7) = real(rhoint ,hp)
                dll_reals( 8) = real(r0(nm,ku,ll),hp)
                dll_reals( 9) = real(aak(nm,ku),hp)
                dll_reals(10) = real(tur_k  ,hp)
                dll_reals(11) = real(tur_eps,hp)
                if (sedd50(l)<0.0_fp) then
                   dll_reals(12) = real(sedd50fld(nm),hp)
                else
                   dll_reals(12) = real(sedd50(l),hp)
                endif
                dll_reals(13) = real(dss(nm,l) ,hp)
                dll_reals(14) = real(rhosol(l) ,hp)
                dll_reals(15) = real(csoil     ,hp)
                dll_reals(16) = real(ag        ,hp)
                dll_reals(17) = real(vicmol    ,hp)
                dll_reals(18) = real(h0        ,hp)
                dll_reals(19) = real(um        ,hp)
                dll_reals(20) = real(vm        ,hp)
                dll_reals(21) = real(chezy     ,hp)
                !
                if (max_integers < 5) then
                   write(errmsg,'(a,a,a)') 'Insufficient space to pass integer values to settling routine.'
                   call prterr (lundia,'U021', trim(errmsg))
                   call d3stop(1, gdp)
                endif
                call nm_to_n_and_m(nm, n, m, gdp)
                dll_integers( 1) = nm
                dll_integers( 2) = n
                dll_integers( 3) = m
                dll_integers( 4) = k
                dll_integers( 5) = l
                !
                if (max_strings < 2) then
                   write(errmsg,'(a,a,a)') 'Insufficient space to pass strings to settling routine.'
                   call prterr (lundia,'U021', trim(errmsg))
                   call d3stop(1, gdp)
                endif
                dll_strings( 1) = gdp%runid
                dll_strings( 2) = dll_usrfil(l)
                !
                ! Initialisation of output variables of user defined settling velocity routine
                !
                ws_dll    = 0.0_hp
                message     = ' '
                !
                ! psem/vsem is used to be sure this works fine in DD calculations
                !
                call psemlun
                error_ptr = 0
                error_ptr = perf_function_fallve(dll_handle(l)       , dll_function(l)       , &
                                                 dll_integers        , max_integers          , &
                                                 dll_reals           , max_reals             , &
                                                 dll_strings         , max_strings           , &
                                                 ws_dll              , message)
                call vsemlun
                if (error_ptr /= 0) then
                   write(errmsg,'(a,a,a)') 'Cannot find function "',trim(dll_function(l)),'" in dynamic library.'
                   call prterr (lundia,'U021', trim(errmsg))
                   call d3stop(1, gdp)
                endif
                if (message /= ' ') then
                   write (lundia,'(a,a,a)') '*** ERROR Message from user defined settling velocity routine ',trim(dll_function(l)),' :'
                   write (lundia,'(a,a  )') '          ', trim(message)
                   write (lundia,'(a    )') ' '
                   call d3stop(1, gdp)
                endif
                !
                ! Output parameters
                !
                ws(nm, k, l) = real(ws_dll,fp)
             enddo     ! nm
          enddo        ! k
       elseif (sedtyp(l) == SEDTYP_NONCOHESIVE_SUSPENDED) then
          !
          do k = 1, kmax
             ku = min(k + 1, kmax)
             ts = thick(k) + thick(ku)
             do nm = 1, nmmax
                if (kfs(nm)==1 .and. kcs(nm)<=2) then
                   rhoint = (thick(k)*rhowat(nm,ku) + thick(ku)*rhowat(nm,k)) / ts
                   s = rhosol(l) / rhoint
                   !
                   ! Molecular viscosity vcmol computed according to Van Rijn (2004) sediment tranport
                   ! This only needs to be done in case of temperature modelling
                   ! Otherwise vicmol is used as computed in iniphy.f90 (also according
                   ! to Van Rijn 2004)
                   !
                   if (ltem > 0) then
                      vcmol = 4.0e-5_fp / (20.0_fp + r0(nm, k, ltem))
                   else                     
                      vcmol = vicmol
                   endif
                   if (dss(nm, l) < 1.5_fp*dsand) then
                      ws(nm, k, l) = (s-1.0_fp) * ag * dss(nm,l)**2/(18.0_fp*vcmol)
                   elseif (dss(nm, l) < 0.5_fp*dgravel) then
                      if (dss(nm, l) < 2.0_fp*dsand) then
                         coefw = (-2.9912_fp/dsand) * dss(nm,l) + 15.9824_fp
                      else
                         coefw = 10.0_fp
                      endif
                      ws(nm, k, l) = coefw * vcmol / dss(nm,l)                   &
                                   & * (sqrt(1.0_fp + (s-1.0_fp)*ag*dss(nm,l)**3  &
                                   & / (100.0_fp*vcmol**2)) - 1.0_fp)
                   else
                      ws(nm, k, l) = 1.1_fp * sqrt((s-1.0_fp)*ag*dss(nm,l))
                   endif
                   !
                   ffloc = 1.0_fp
                   if (  iform(l) == -2                          &
                       & .and. sedd50(l) < dsand                 &
                       & .and. (lsal     > 0 .or. saleqs > 0.0_fp)  ) then
                      !
                      ! Hindered settling (Van Rijn, 2004)
                      !
                      cgel = 0.65_fp * rhosol(l) * min(sedd50(l)/dsand , 1.0_fp)
                      hinset = max(0.0_fp , (1.0 - max(0.0_fp, 0.65_fp*aak(nm,ku))/cgel))
                      !
                      ! Flocculation (Van Rijn, 2004)
                      !
                      if (lsal > 0) then
                        salint = max((thick(k)*r0(nm,ku,lsal)+thick(ku)*r0(nm,k,lsal))/ts , 0.0_fp)
                      else
                        salint = saleqs
                      endif
                      if (salint >= 0.01_fp .and. salmax(l)>0.0_fp) then
                         fhulp = max(4.0_fp+log10(2.0_fp*max(1.0e-6_fp,aak(nm,k))/cgel) , 1.0_fp)
                         efloc = min(max(dsand/sedd50(l)-1.0_fp , 1.0_fp) , 3.0_fp)
                         ffloc0 = max(min(fhulp**efloc , 10.0_fp) , 1.0_fp)
                         ffloc = (ffloc0-1.0_fp) * min(1.0_fp,salint/salmax(l)) + 1.0_fp
                         !
                         ! Calibration parameter for flocculation
                         !
                         ffloc = ffloc * gamflc(l)
                         !
                         ffloc = max(min(ffloc , 10.0_fp) , 1.0_fp)
                      endif
                   else
                      !
                      ! hindered settling Richardson and Zaki formula
                      ! Previous approach: Oliver's formula
                      !
                      hinset = max(0.0_fp , (1.0_fp - max(0.0_fp , aak(nm,ku))/csoil)) 
                   endif
                   ws(nm, k, l) = ffloc *ws(nm,k,l) * hinset**5
                endif  ! kfs=1, kcs<=2
             enddo     ! nm
          enddo        ! k
       elseif (sedtyp(l) == SEDTYP_COHESIVE) then
          do k = 1, kmax
             ku = min(k+1 , kmax)
             ts = thick(k) + thick(ku)
             do nm = 1, nmmax
                if (kfs(nm)==1 .and. kcs(nm)<=2) then
                   if (lsal > 0) then
                      salint = (  thick(k) *r0(nm, ku, lsal) &
                             &  + thick(ku)*r0(nm, k , lsal)  ) / ts
                      !
                      ! Originally the following line was:
                      !    if (salint.lt.salmax(l)) then
                      ! A crash occurs due to (small) negative values of
                      ! salint in combination with salmax(l)=0 (dividing by
                      ! zero). This is solved by adding the test:
                      !    salmax(l).gt.0.0
                      !
                      if (salint<salmax(l) .and. salmax(l)>0.0_fp) then
                         a = 1.0_fp + wsm(l)/ws0(l)
                         b = a - 2.0_fp
                         ws(nm, k, l) = 0.5_fp * ws0(l) * (a-b*cos(pi*salint/salmax(l)))
                      else
                         ws(nm, k, l) = wsm(l)
                      endif
                   else
                      ws(nm, k, l) = ws0(l)
                   endif
                   !
                   ! hindered settling Richardson and Zaki/Mehta
                   !
                   hinset = max(0.0_fp , (1.0_fp - max(0.0_fp , aak(nm,ku))/csoil))
                   ws(nm, k, l) = ws(nm,k,l) * hinset**5
                endif   ! kfs=1, kcs<=2
             enddo      ! nm
          enddo         ! k
       else
       endif            ! dll_function/sedtyp
    enddo               ! l
end subroutine fallve
