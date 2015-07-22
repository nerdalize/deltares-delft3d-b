subroutine cucnp(dischy    ,icreep    ,dpdksi    ,s0        ,u0        , &
               & v1        ,w1        ,hu        ,dps       ,dpu       , &
               & umean     ,guu       ,gvv       ,gvu       ,gsqs      , &
               & gvd       ,gud       ,gvz       ,gsqiu     ,qxk       , &
               & qyk       ,disch     ,umdis     ,mnksrc    ,dismmt    ,j         , &
               & nmmaxj    ,nmmax     ,kmax      ,icx       ,icy       , &
               & nsrc      ,lsecfl    ,lstsci    ,betac     ,aak       , &
               & bbk       ,cck       ,ddk       ,bbka      ,bbkc      , &
               & thick     ,sig       ,rho       ,sumrho    ,vicuv     , &
               & vnu2d     ,vicww     ,wsu       ,fxw       ,wsbodyu   , &
               & rxx       ,rxy       ,kcs       ,kcu       ,kfu       ,kfv       , &
               & kfs       ,kspu      ,kadu      ,kadv      ,dfu       ,deltau    , &
               & tp        ,rlabda    ,cfurou    ,cfvrou    ,rttfu     , &
               & r0        ,diapl     ,rnpl      ,taubpu    ,taubsu    , &
               & windsu    ,patm      ,fcorio    ,ubrlsu    ,uwtypu    , &
               & hkru      ,pship     ,tgfsep    ,dteu      ,ua        , &
               & ub        ,ustokes   ,gdp       )
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
!  $Id: cucnp.f90 2159 2013-01-31 12:59:15Z goede $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/cucnp.f90 $
!!--description-----------------------------------------------------------------
!
! The coefficient for the momentum equations are computed and the stored in the
! arrays AAK, BBK, CCK, and DDK (velocity points). For the depth averaged
! equations the coefficients are stored in AA, BB, CC, DD (velocity points) and
! A, B, C, D (water level points). A double sweep is used to eliminate the
! coupling in the vertical.
!
! Reference : On the approximation of horizontal gradients in sigma co-ordinates
! for bathymetry with steep bottom slopes (G.S. Stelling and J.van Kester -
! International Journal for Methods in Fluids, Vol. 18 1994)
!
! - Horizontal Advection in U-direction : depending on the flag MOMSOL. Options:
!    explicit, central scheme (WAQUA)
!    explicit, central scheme (Cyclic; Ref.: Stelling & Leendertse "Approximation
!                              of Convective Processes by Cyclic AOI methods",
!                              Proceeding 2nd ASCE Conference on Est. and Coastal
!                              Modelling, Tampa, 1991)
!    explicit, conservative scheme (Flooding Scheme-FLS; Ref.: Stelling&Duijnmeijer
!                             "A Staggered conservative scheme for every Froude
!                              number in rapidly varied shallow water flows",
!                              Num. Method in Fluids)
!
! - Horizontal Diffusion: explicit, along Z-planes (3D), implicit (2DH)
! - Vertical Advection:   implicit, central scheme
! - Vertical Diffusion:   implicit
! - roughness (partial slip) of rigid walls
! - blockage flow for structures
! - 2D turbulence model at depth points
! - Special approximation pressure term, based on limiter to avoid artificial flow.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use timers
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                , pointer :: gammax
    real(fp)                , pointer :: hdt
    integer                 , pointer :: ibaroc
    logical                 , pointer :: cstbnd
    character(8)            , pointer :: dpsopt
    character(6)            , pointer :: momsol
    logical                 , pointer :: slplim
    real(fp)                , pointer :: rhow
    real(fp)                , pointer :: rhofrac
    real(fp)                , pointer :: ag
    real(fp)                , pointer :: vicmol
    integer                 , pointer :: iro
    integer                 , pointer :: irov
    logical                 , pointer :: wind
    logical                 , pointer :: wave
    logical                 , pointer :: roller
    logical                 , pointer :: xbeach
    logical                 , pointer :: dpmveg
!
! Global variables
!
    integer                                           , intent(in)  :: icreep  !  Description and declaration in tricom.igs
    integer                                                         :: icx     !!  Increment in the X-dir., if ICX= NMAX
                                                                               !!  then computation proceeds in the X-
                                                                               !!  dir. If icx=1 then computation pro-
                                                                               !!  ceeds in the Y-dir.
    integer                                                         :: icy     !!  Increment in the Y-dir. (see ICX)
    integer                                                         :: j       !!  Begin pointer for arrays which have
                                                                               !!  been transformed into 1D arrays.
                                                                               !!  Due to the shift in the 2nd (M-)
                                                                               !!  index, J = -2*NMAX + 1
    integer                                                         :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: lsecfl  !  Description and declaration in dimens.igs
    integer                                                         :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                         :: nmmax   !  Description and declaration in dimens.igs
    integer                                                         :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                           , intent(in)  :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nsrc)                       , intent(in)  :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub)                       :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)               :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: kadu    !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub, kmax)   , intent(in)  :: kadv    !  Description and declaration in esm_alloc_int.f90
    real(fp)                                                        :: betac   !  Description and declaration in tricom.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: dteu    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: gvz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hkru    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: tgfsep  !!  Water elev. induced by tide gen.force
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: uwtypu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)        , intent(in)  :: windsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                      :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax), intent(in)  :: w1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)                   :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)                   :: cfvrou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: aak     !!  Internal work array, coupling of layer velocity in (N,M,K)
                                                                               !!  with water level point left (down)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbk     !!  Internal work array, coefficient layer velocity in (N,M,K) implicit part
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbka    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: bbkc    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: cck     !!  Internal work array, coupling layer velocity in (N,M,K)
                                                                               !!  with water level point right (upper)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ddk     !!  Internal work array, diagonal space at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: dpdksi  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: rho     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: rttfu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)  , intent(in)  :: sumrho  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ua
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ub
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: ustokes !  Description and declaration in trisol.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)                :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)              :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax, lstsci)        :: r0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                       :: sig     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                       :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                         , intent(in)  :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                         , intent(in)  :: umdis   !  Description and declaration in esm_alloc_real.f90
    character(1), dimension(nsrc)                     , intent(in)  :: dismmt  !  Description and declaration in esm_alloc_char.f90
    character(8)                                      , intent(in)  :: dischy  !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                    :: ddb
    integer                    :: iada
    integer                    :: iadc
    integer                    :: icxy    ! MAX value of ICX and ICY
    integer                    :: isrc
    integer                    :: k
    integer                    :: kdo
    integer                    :: kfw
    integer                    :: kk
    integer                    :: kup
    integer                    :: kspu0k
    integer                    :: maskval
    integer                    :: nbaroc  ! No barocline pressure on open boundary points if IBAROC = 0
    integer                    :: ndm
    integer                    :: ndmd
    integer                    :: ndmu
    integer                    :: nm
    integer                    :: nmd
    integer                    :: nmdis
    integer                    :: nmu
    integer                    :: num
    integer                    :: numu
    real(fp)                   :: adza
    real(fp)                   :: adzb
    real(fp)                   :: adzc
    real(fp)                   :: ap
    real(fp)                   :: ap1
    real(fp)                   :: ap2
    real(fp)                   :: apd
    real(fp)                   :: apu
    real(fp)                   :: bdmwrp
    real(fp)                   :: bdmwrs
    real(fp)                   :: bi
    real(fp)                   :: cbot
    real(fp)                   :: cnurh
    real(fp)                   :: ddza
    real(fp)                   :: ddzb
    real(fp)                   :: ddzc
    real(fp)                   :: dia
    real(fp), dimension(1,1,1) :: dummy
    real(fp)                   :: dpsmax
    real(fp)                   :: facmax
    real(fp)                   :: ff
    real(fp)                   :: gksi
    real(fp)                   :: h0i
    real(fp)                   :: hl
    real(fp)                   :: hr
    real(fp)                   :: hugsqs  ! HU(NM/NMD) * GSQS(NM) Depending on UMDIS the HU of point NM or NMD will be used
    real(fp)                   :: qwind
    real(fp),external          :: redvic
    real(fp)                   :: rn
    real(fp)                   :: rou
    real(fp)                   :: svvv
    real(fp)                   :: tsg1
    real(fp)                   :: tsg2
    real(fp)                   :: umod
    real(fp)                   :: uuu
    real(fp)                   :: uweir
    real(fp)                   :: viz1
    real(fp)                   :: viz2
    real(fp)                   :: vvv
    real(fp)                   :: wsumax
    real(fp)                   :: www
!
!! executable statements -------------------------------------------------------
!
    gammax     => gdp%gdnumeco%gammax
    hdt        => gdp%gdnumeco%hdt
    ibaroc     => gdp%gdnumeco%ibaroc
    cstbnd     => gdp%gdnumeco%cstbnd
    dpsopt     => gdp%gdnumeco%dpsopt
    momsol     => gdp%gdnumeco%momsol
    slplim     => gdp%gdnumeco%slplim
    rhow       => gdp%gdphysco%rhow
    rhofrac    => gdp%gdphysco%rhofrac
    ag         => gdp%gdphysco%ag
    vicmol     => gdp%gdphysco%vicmol
    iro        => gdp%gdphysco%iro
    irov       => gdp%gdphysco%irov
    wind       => gdp%gdprocs%wind
    wave       => gdp%gdprocs%wave
    roller     => gdp%gdprocs%roller
    xbeach     => gdp%gdprocs%xbeach
    dpmveg     => gdp%gdprocs%dpmveg
    !
    ! INITIALISATION
    !
    call timer_start(timer_cucnp_ini, gdp)
    ddb = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ! factor in maximum wave force 1/4 alpha rho g gammax**2 h**2 / tp /(sqrt(g h)
    facmax = 0.25*sqrt(ag)*rhow*gammax**2
    !
    if (icx==1) then
       ff = -1.0
    else
       ff = 1.0
    endif
    !
    ! Initialise arrays aak, bbk and cck for all (nm,k)
    !
    aak = 0.0
    cck = 0.0
    !
    do k = 1, kmax
        do nm = 1, nmmax
            bbk(nm, k) = 1.0/hdt
            if (kfu(nm) == 0) then
                ddk(nm,k) = 0.0_fp
            else
                !
                ! For a closed gate layer
                !
                kspu0k = kspu(nm, 0)*kspu(nm, k)
                if (kspu0k==4 .or. kspu0k==10) then
                    ddk(nm, k) = 0.0
                else
                    !
                    !  For 2D weir use UUU value derived from flux QXK (value at old time step)
                    !
                    if (abs(kspu(nm, 0))==9) then
                        uuu = qxk(nm, k)/(guu(nm)*hu(nm))
                    else
                        uuu = u0(nm, k)
                    endif
                    ddk(nm, k) = uuu/hdt - icreep*dpdksi(nm, k)
                endif
            endif
        enddo
    enddo
    call timer_stop(timer_cucnp_ini, gdp)
    !
    call timer_start(timer_cucnp_momsol, gdp)
    if (momsol == 'cyclic' .or. momsol == 'waqua ') then
       call mom_cw &
            &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
            & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
            & dps       ,s0        ,u0        ,v1        ,qxk       ,qyk       , &
            & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
            & umean     ,bbk       ,ddk       ,dummy     ,dummy     ,dummy     , &
            & dummy     ,dummy     ,dummy     ,dummy     ,dummy     ,gdp)
    elseif (momsol == 'flood ') then
        call mom_fls &
             &(icx       ,icy       ,nmmax     ,kmax      ,kcu       ,kcs       , &
             & kfu       ,kfv       ,kspu      ,kadu      ,kadv      ,            &
             & dps       ,s0        ,u0        ,v1        ,qxk       ,qyk       , &
             & hu        ,guu       ,gvv       ,gvd       ,gvu       ,gsqiu     , &
             & umean     ,bbk       ,ddk       ,dummy     ,dummy     ,dummy     , &
             & dummy     ,dummy     ,dummy     ,ua        ,ub        ,thick     , &
             & gdp)
    endif
    call timer_stop(timer_cucnp_momsol, gdp)
    call timer_start(timer_cucnp_rhs, gdp)
    do k = 1, kmax
       nmd  = -icx
       ndm  = -icy
       ndmd = -icx - icy
       nmu  = icx
       num  = icy
       numu = icx + icy
       ndmu = icx - icy
       do nm = 1, nmmax
          nmd  = nmd + 1
          ndm  = ndm + 1
          ndmd = ndmd + 1
          nmu  = nmu + 1
          num  = num + 1
          numu = numu + 1
          ndmu = ndmu + 1
          !
          ! For an active point and not a gate and plate
          !
          kspu0k = kspu(nm, 0)*kspu(nm, k)
          if (kfu(nm)==1 .and. kspu0k /=4 .and. kspu0k /=10) then
             gksi = gvu(nm)
             if (       (cstbnd .and. (kcs(nm)==2 .or. kcs(nmu)==2) ) &
                 & .or. (kcs(nm)==3 .or. kcs(nmu)==3                )  ) then
                svvv = max(kfv(ndm) + kfv(ndmu) + kfv(nm) + kfv(nmu), 1)
                vvv  = (  v1(ndm, k)*kfv(ndm) + v1(ndmu, k)*kfv(ndmu) &
                     &  + v1(nm , k)*kfv(nm ) + v1(nmu , k)*kfv(nmu )  ) / svvv
             else
                vvv = .25*(v1(ndm, k) + v1(ndmu, k) + v1(nm, k) + v1(nmu, k))
             endif
             uuu  = u0(nm, k)
             umod = sqrt(uuu*uuu + vvv*vvv)
             !
             hl  = real(dps(nm),fp) + s0(nm)
             hr  = real(dps(nmu),fp) + s0(nmu)
             rou = .5*(rho(nm, k) + rho(nmu, k))
             !
             ! FLAG BAROCLINE PRESSURE ON OPEN BOUNDARY (DEFAULT YES IBAROC = 1)
             !
             nbaroc = 1
             if (kcs(nm)*kcs(nmu)==2) nbaroc = ibaroc
             !
             ! SUBSTITUTION IN COEFFICIENTS,  including in DDK
             !    CORIOLIS, GRAVITY PRESSURE TERM and TIDE GENERATING FORCES
             !     remember this is not a closed gate layer
             !     KSPU(NM,0)*KSPU(NM,K)<>4, so initialisation
             !     test like in loop 100 of UZD is not necessary
             !
             aak(nm, k) = -ag*rhofrac/gksi
             cck(nm, k) =  ag*rhofrac/gksi
             bbk(nm, k) = bbk(nm, k) + 0.5*rttfu(nm, k)*umod
             ddk(nm, k) = ddk(nm, k) +                                          &
                        & ff*fcorio(nm)*vvv - ag*(1. - icreep)/(gksi*rhow)      &
                        & *nbaroc*(sig(k)*rou*(hr - hl)                         &
                        & + (sumrho(nmu, k)*hr - sumrho(nm, k)*hl))             &
                        & - (patm(nmu) - patm(nm))/(gksi*rhow)                  &
                        & - (pship(nmu) - pship(nm))/(gksi*rhow)                &
                        & + ag*(tgfsep(nmu) - tgfsep(nm))/gksi
          endif
       enddo
    enddo
    call timer_stop(timer_cucnp_rhs, gdp)
    !
    ! energy loss (local weir loss/rigid sheets)
    !
    call timer_start(timer_cucnp_eloss, gdp)
    if (kmax == 1) then
       call usrbrl2d(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
                   & kspu      ,guu       ,gvu       ,qxk       ,bbk       , &
                   & ddk       ,ubrlsu    ,dps       ,hkru      ,s0        , &
                   & hu        ,umean     ,thick     ,dteu      ,taubpu    , &
                   & gdp       )
    endif
    call usrbrl(icx       ,icy       ,nmmax     ,kmax      ,kfu       , &
              & kspu      ,gvu       ,u0        ,v1        ,bbk       , &
              & ubrlsu    ,diapl     ,rnpl      ,gdp       )
    call timer_stop(timer_cucnp_eloss, gdp)
    !
    ndm = -icy
    kdo = max(1, kmax - 1)
    kup = min(kmax, 2)
    !
    call timer_start(timer_cucnp_stress, gdp)
    do nm = 1, nmmax
       ndm = ndm + 1
       if (kfu(nm)==1) then
          h0i = 1./hu(nm)
          !
          ! WIND AND BOTTOM FRICTION
          !
          qwind  = 0.0
          bdmwrp = 0.0
          bdmwrs = 0.0
          !
          ! BOTTOM STRESS DUE TO FLOW AND WAVES
          !
          ! Special measures for inundation
          !
          ! Estimate velocity on the basis of local equilibrium by solving u(nm)
          ! with an explicit procedure. So that high velocities can be avoided
          ! during flooding (kfu=1 and u=0.) in regions with steep topography.
          ! Velocity is estimated assuming critical flow over a short-crested
          ! weir. 
          ! Gates are excluded
          !
          if (dpsopt == 'DP  ' .or. slplim) then
             if (kfu(nm) == 1 .and. abs(u0(nm,kmax)) <= 1.0e-15 .and. kspu(nm, 0) /= 4 .and. kspu(nm, 0) /= 10) then
                !
                ! cfurou(nm,1) contains u/u*
                !
                uweir      = sqrt(2.0 / 3.0 * ag * hu(nm))
                taubpu(nm) = uweir / (cfurou(nm,1)**2)
             endif
          endif
          !
          ! Slope correction for steep slopes
          !
          if (slplim) then
             nmu    = nm + icx
             dpsmax = max(-dps(nm),-dps(nmu))
             if (s0(nm) < dpsmax) then
                do k = 1, kmax
                   ddk(nm,k) = ddk(nm,k) - ag*(s0(nm)-dpsmax)/gvu(nm)
                enddo
             elseif (s0(nmu) < dpsmax) then
                do k = 1, kmax
                   ddk(nm,k) = ddk(nm,k) + ag*(s0(nmu)-dpsmax)/gvu(nm)
                enddo
             endif
          endif
          !
          ! End of special measures for smooth inundation
          !
          ! Bottom and wind shear stress
          !
          cbot         = taubpu(nm)
          qwind        = h0i*windsu(nm)/thick(1)
          bdmwrp       = h0i*cbot/thick(kmax)
          bdmwrs       = h0i*taubsu(nm)/thick(kmax)
          bbk(nm,kmax) = bbk(nm, kmax) + bdmwrp
          ddk(nm,1)    = ddk(nm, 1) - qwind/rhow
          ddk(nm,kmax) = ddk(nm, kmax) + bdmwrs
          !
          ! WAVE STRESS AT SURFACE
          ! Physical limit to WSU to prevent unrealistically large stress in shallow water
          !
          if (wave) then
             wsumax = facmax*hu(nm)**(1.5)/max(0.1_fp, tp(nm))
             wsu(nm) = sign(min(abs(wsu(nm)), wsumax), wsu(nm))
             !
             ddk(nm, 1) = ddk(nm, 1) + wsu(nm)*h0i/(rhow*thick(1))
             !
             ! WAVE INDUCED BODY FORCE
             !
             if (roller .or. xbeach) then
                fxw(nm) = sign(min(abs(fxw(nm)), wsumax), fxw(nm))
                do k = 1, kmax
                   ddk(nm, k) = ddk(nm, k) + fxw(nm)*h0i/rhow
                enddo
             else
                wsbodyu(nm) = sign(min(abs(wsbodyu(nm)), wsumax), wsbodyu(nm))
                do k = 1, kmax
                   ddk(nm, k) = ddk(nm, k) + wsbodyu(nm)*h0i/rhow
                enddo
             endif
          endif
       endif
    enddo
    call timer_stop(timer_cucnp_stress, gdp)
    !
    ! In case of 3D waves:
    ! Added shear stress in wave boundary layer due to streaming
    !
    call timer_start(timer_cucnp_shrwav, gdp)
    if (wave .and. kmax>1) then
       call shrwav(nmmax     ,kmax      ,icx       ,dfu       ,deltau    , &
                 & tp        ,rlabda    ,hu        ,kfu       , &
                 & ddk       ,thick     ,gdp       )
    endif
    call timer_stop(timer_cucnp_shrwav, gdp)
    !
    ! DISCHARGE ADDITION OF MOMENTUM
    !
    call timer_start(timer_cucnp_dismmt, gdp)
    do isrc = 1, nsrc
       nm = (mnksrc(5, isrc) + ddb) + ((mnksrc(4, isrc) - 1)*icxy + ddb)
       nmd = nm - icx
       if (dismmt(isrc)=='Y' .and. disch(isrc)>0.0) then
          if (umdis(isrc)>=0.0) then
             nmdis = nm
             hugsqs = hu(nm)*gsqs(nm)
          else
             nmdis = nmd
             hugsqs = hu(nmd)*gsqs(nm)
          endif
          kk = mnksrc(6, isrc)
          if (kfu(nmdis)==1) then
             if (kk==0) then
                do k = 1, kmax
                   bbk(nmdis, k) = bbk(nmdis, k) + disch(isrc)/hugsqs
                   ddk(nmdis, k) = ddk(nmdis, k) + umdis(isrc)*disch(isrc)      &
                                 & /hugsqs
                enddo
             else
                bbk(nmdis, kk) = bbk(nmdis, kk) + disch(isrc)/(thick(kk)*hugsqs)
                ddk(nmdis, kk) = ddk(nmdis, kk) + umdis(isrc)*disch(isrc)       &
                               & /(thick(kk)*hugsqs)
             endif
          endif
       endif
    enddo
    call timer_stop(timer_cucnp_dismmt, gdp)
    !
    ! VERTICAL ADVECTION AND VISCOSITY, IMPLICIT
    !
    call timer_start(timer_cucnp_advdiffv, gdp)
    if (kmax>1) then
       do k = 1, kmax
          kfw = 0
          kdo = k - 1
          kup = k + 1
          if (k==1) then
             kfw = -1
             kdo = 1
          endif
          if (k==kmax) then
             kfw = 1
             kup = kmax
          endif
          tsg1 = thick(kdo) + thick(k)
          tsg2 = thick(k) + thick(kup)
          ndm = -icy
          nmu = icx
          do nm = 1, nmmax
             ndm = ndm + 1
             nmu = nmu + 1
             if (kfu(nm)==1) then
                !
                ! Free slip between open and closed layers of a gate
                !
                iada = 1
                iadc = 1
                if (kspu(nm, 0)==4 .or. kspu(nm, 0)==10) then
                   iada = max(1 - (kspu(nm, kdo) + kspu(nm, k)), 0)
                   iadc = max(1 - (kspu(nm, k) + kspu(nm, kup)), 0)
                endif
                h0i = 1.0/hu(nm)
                if (.not. dpmveg) then
                   ap1 = 1.0
                   ap2 = 1.0
                else
                   !
                   ! Directional Point Model of Vegetation
                   !
                   dia  = 0.5 * ( diapl(nm,kdo) + diapl(nmu,kdo) )
                   rn   = 0.5 * (  rnpl(nm,kdo) +  rnpl(nmu,kdo) )
                   apd  = 1.0 - dia*dia*rn*pi*0.25
                   dia  = 0.5 * ( diapl(nm,k  ) + diapl(nmu,k  ) )
                   rn   = 0.5 * (  rnpl(nm,k  ) +  rnpl(nmu,k  ) )
                   ap   = 1.0 - dia*dia*rn*pi*0.25
                   dia  = 0.5 * ( diapl(nm,kup) + diapl(nmu,kup) )
                   rn   = 0.5 * (  rnpl(nm,kup) +  rnpl(nmu,kup) )
                   apu  = 1.0 - dia*dia*rn*pi*0.25
                   ap1  = (ap+apd)/2.0
                   ap2  = (ap+apu)/2.0
                endif
                !
                ! vertical viscosity
                !
                cnurh = h0i * h0i
                !
                ! viz1 calculation 
                ! restriction is moved from TURCLO to here
                !
                viz1  = 0.25 * (2 + kfw*(1 - kfw)) * ap1             &
                      & * (2.0*vicmol + redvic(vicww(nm , kdo), gdp) &
                      &               + redvic(vicww(nmu, kdo), gdp))
                !
                ! viz1 calculation 
                ! restriction is moved from TURCLO to here
                !
                viz2 = 0.25 * (2 - kfw*(1 + kfw)) * ap2            &
                     & * (2.0*vicmol + redvic(vicww(nm , k), gdp)  &
                     &               + redvic(vicww(nmu, k), gdp))
                ddza = 2.0 * cnurh * viz1 / (tsg1*thick(k))
                ddzc = 2.0 * cnurh * viz2 / (tsg2*thick(k))
                !
                ddza = iada * ddza
                ddzc = iadc * ddzc
                ddzb = -ddza - ddzc
                !
                ! substitution in coefficients
                !
                bbka(nm, k) = -ddza
                bbk (nm, k) = bbk(nm, k) - ddzb
                bbkc(nm, k) = -ddzc
                !
                ! Effect of waves, due to Stokes drift
                !
                ddk(nm, k) = ddk(nm,k) - ( ddza*(ustokes(nm,kdo)-ustokes(nm,k  )) -   &
                                        &  ddzc*(ustokes(nm,k  )-ustokes(nm,kup))   )
                !
                ! advection in vertical direction; w*du/dz
                !
                maskval = min(kcs(nm), 2)*min(kcs(nmu), 2)
                www = .25*abs(maskval)*(w1(nm, k - 1) + w1(nm, k) + w1(nmu, k - 1)   &
                    & + w1(nmu, k))
                if (www<0.0) then
                   adza = 2.0*www*h0i*tsg2/(tsg1*(tsg1 + tsg2))*(1 - abs(kfw))  &
                        & + kfw*(1 + kfw)*www*h0i/tsg1
                   adzc = -2.0*www*h0i*tsg1/(tsg2*(tsg1 + tsg2))*(1 - abs(kfw))
                else
                   adza = 2.0*www*h0i*tsg2/(tsg1*(tsg1 + tsg2))*(1 - abs(kfw))
                   adzc = -2.0*www*h0i*tsg1/(tsg2*(tsg1 + tsg2))*(1 - abs(kfw)) &
                        & + abs(kfw)*( - 1 + kfw)*www*h0i/tsg2
                endif
                !
                adza = iada*adza
                adzc = iadc*adzc
                adzb = -adza - adzc
                !
                ! substitution in coefficients
                !
                bbka(nm, k) = bbka(nm, k) + adza
                bbk (nm, k) = bbk (nm, k) + adzb
                bbkc(nm, k) = bbkc(nm, k) + adzc
             else
                !
                ! no implicit vertical part
                !
                bbka(nm, k) = 0.0
                bbkc(nm, k) = 0.0
             endif
          enddo
       enddo
    !
    endif
    call timer_stop(timer_cucnp_advdiffv, gdp)
    !
    ! HORIZONTAL VISCOSTY
    !
    call timer_start(timer_cucnp_vih, gdp)
    if (irov>0) then
       !
       ! Stresses due to rigid walls
       !     implemented fully explicit
       !
       call vihrov(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                 & icy       ,kcs       ,kfu       ,kfv       ,kfs       , &
                 & u0        ,v1        ,vicuv     ,vnu2d     ,guu       , &
                 & gvv       ,gvu       ,ddk       ,rxx       ,rxy       , &
                 & gdp       )
    endif
    call timer_stop(timer_cucnp_vih, gdp)
    if (kmax>1) then
       !
       !***SCALE ROWS OF MATRIX/RIGHT HAND SIDE VECTOR
       !
       call timer_start(timer_cucnp_rowsc, gdp)
       do k = 1, kmax
          do nm = 1, nmmax
             if (kfu(nm)==1) then
                bi          = 1.0_fp / bbk(nm, k)
                aak (nm, k) = aak (nm, k) * bi
                bbk (nm, k) = 1.0_fp
                bbka(nm, k) = bbka(nm, k) * bi
                bbkc(nm, k) = bbkc(nm, k) * bi
                cck (nm, k) = cck (nm, k) * bi
                ddk (nm, k) = ddk (nm, k) * bi
             endif
          enddo
       enddo
       call timer_stop(timer_cucnp_rowsc, gdp)
       !
       ! SOLUTION PROCEDURE SYSTEM OF EQUATIONS IN VERTICAL DIRECTION
       !
       !
       ! Division by the pivot for k=1 is not needed anymore
       ! because of row scaling
       !
       call timer_start(timer_cucnp_lhs, gdp)
       do k = 2, kmax
          do nm = 1, nmmax
             if (kfu(nm)==1) then
                bi = 1./(bbk(nm, k) - bbka(nm, k)*bbkc(nm, k - 1))
                bbk(nm, k) = 1.0
                bbkc(nm, k) = bbkc(nm, k)*bi
                aak(nm, k) = (aak(nm, k) - bbka(nm, k)*aak(nm, k - 1))*bi
                cck(nm, k) = (cck(nm, k) - bbka(nm, k)*cck(nm, k - 1))*bi
                ddk(nm, k) = (ddk(nm, k) - bbka(nm, k)*ddk(nm, k - 1))*bi
             endif
          enddo
       enddo
       !
       ! back sweep
       !
       do k = kmax - 1, 1, -1
          do nm = 1, nmmax
             if (kfu(nm)==1) then
                aak(nm, k) = aak(nm, k) - bbkc(nm, k)*aak(nm, k + 1)
                cck(nm, k) = cck(nm, k) - bbkc(nm, k)*cck(nm, k + 1)
                ddk(nm, k) = ddk(nm, k) - bbkc(nm, k)*ddk(nm, k + 1)
             endif
          enddo
       enddo
       call timer_stop(timer_cucnp_lhs, gdp)
       !
    elseif (lsecfl/=0) then
       !
       ! STRESSES DUE TO SECONDARY FLOW (SPIRAL MOTION INTENSITY)
       !     IMPLEMENTED FULLY EXPLICIT, ACCORDING 3D CASE
       !
       call timer_start(timer_cucnp_vihsec, gdp)
       call vihsec(u0        ,v1        ,guu       ,gvu       ,gvv       , &
                 & r0        ,icx       ,icy       ,j         ,nmmaxj    , &
                 & nmmax     ,kmax      ,lsecfl    ,lstsci    ,betac     , &
                 & kfu       ,kfv       ,ddk       ,cfurou    ,cfvrou    , &
                 & rxx       ,rxy       ,gdp       )
       call timer_stop(timer_cucnp_vihsec, gdp)
    else
    endif
end subroutine cucnp
