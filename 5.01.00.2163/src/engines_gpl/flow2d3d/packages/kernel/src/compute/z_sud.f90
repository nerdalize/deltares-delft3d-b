subroutine z_sud(j         ,nmmaxj    ,nmmax     ,kmax      ,mmax      , &
               & nmax      ,nsrc      ,nst       ,icx       ,icy       , &
               & flood     ,norow     ,irocol    ,mnksrc    ,kfsmx0    , &
               & kfu       ,kfv       ,kfs       ,kcs       ,kcu       , &
               & kfuz0     ,kfvz0     ,kfsz0     ,kspu      ,kcs45     , &
               & kcscut    ,kfumin    ,kfsmin    ,kfumx0    ,kfvmin    , &
               & kfvmx0    ,thick     ,circ2d    ,circ3d    ,s0        , &
               & s1        ,u0        ,u1        ,v1        ,w1        , &
               & qxk       ,qyk       ,qzk       ,guu       ,gvv       , &
               & guv       ,gvu       ,gud       ,gvd       ,guz       , &
               & gvz       ,gsqiu     ,gsqs      ,disch     ,umdis     , &
               & dismmt    ,umean     ,evap      ,hu        ,dps       , &
               & dpu       ,dzs0      ,dzu0      ,dzv0      ,a         , &
               & b         ,c         ,d         ,aa        ,bb        , &
               & cc        ,dd        ,tetau     ,aak       ,bbk       , &
               & cck       ,ddk       ,d0        ,d0k       ,bbka      , &
               & bbkc      ,wsu       ,taubpu    ,taubsu    ,vicuv     , &
               & vnu2d     ,vicww     ,rxx       ,rxy       ,windu     , &
               & tp        ,rlabda    ,dfu       ,deltau    ,fxw       , wsbodyu  , &
               & patm      ,fcorio    ,tgfsep    ,drhodx    ,zk        , &
               & p0        ,crbc      ,idry      ,porosu    ,ubrlsu    , &
               & pship     ,diapl     ,rnpl      ,cfurou    ,precip    , &
               & ustokes   ,gdp       ) 
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
!  $Id: z_sud.f90 2121 2013-01-18 15:48:07Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/kernel/src/compute/z_sud.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Z_SUD evaluates/solves the implicitly coupled
!              momentum and continuity equation at each
!              half time step.
!              Special approximation for pressure term,
!              based on limiter to avoid artificial flow.
!              Switch which makes it possible to use
!              upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
! Method used: A.D.I.-scheme is used.
!              Upwind-approach for wet cross section in shallow
!              areas or if the model area contains structures.
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
    !
    integer                , pointer :: lundia
    real(fp)               , pointer :: hdt
    real(fp)               , pointer :: rhow
    real(fp)               , pointer :: ag
    integer                , pointer :: iro
    integer                , pointer :: maseva
    logical                , pointer :: wind
    logical                , pointer :: culvert
    logical                , pointer :: zmodel
    logical                , pointer :: wavcmp
    logical                , pointer :: bubble
    include 'flow_steps_f.inc'
!
! Global variables
!
    integer                                                       :: icx     !!  Increment in the X-dir., if ICX= NMAX
                                                                             !!  then computation proceeds in the X-
                                                                             !!  dir. If icx=1 then computation pro-
                                                                             !!  ceeds in the Y-dir.
    integer                                                       :: icy     !!  Increment in the Y-dir. (see ICX)
    integer                                                       :: idry
    integer                                                       :: j       !!  Begin pointer for arrays which have
                                                                             !!  been transformed into 1D arrays.
                                                                             !!  Due to the shift in the 2nd (M-)
                                                                             !!  index, J = -2*NMAX + 1
    integer                                                       :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: nmmax   !  Description and declaration in dimens.igs
    integer                                                       :: nmmaxj  !  Description and declaration in dimens.igs
    integer                                                       :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in)  :: nst     !!  Time step number
    integer    , dimension(5, norow)                              :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(7, nsrc)                               :: mnksrc  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kcs45
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kcscut  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kfs     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)   , intent(in)  :: kfsmin  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kfsmx0  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kfu     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kfumin  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kfumx0  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kfvmin  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kfvmx0  !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub)                 :: kfv     !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)         :: kspu    !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)           :: kfsz0   !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)           :: kfuz0   !  Description and declaration in esm_alloc_int.f90
    integer    , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)           :: kfvz0   !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(12, norow)                                :: crbc    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(4, norow)                                 :: circ2d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: a       !!  Internal work array, tridiagonal
                                                                             !!  matrix water levels lower diagonal
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: aa      !!  Internal work array, coupling mean
                                                                             !!  velocity with water level point in
                                                                             !!  (N,M,K) left (down)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: b       !!  Internal work array, tridiagonal ma-
                                                                             !!  trix water levels main diagonal
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: bb      !!  Internal work array, coefficient mean
                                                                             !!  velocity
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: c       !!  Internal work array, tridiagonal
                                                                             !!  matrix water levels upper diagonal
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: cc      !!  Internal work array, coupling mean
                                                                             !!  velocity with water level point
                                                                             !!  right (upper)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 3)                 :: cfurou  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: d       !!  Internal work array, Right Hand side
                                                                             !!  of the Continuity equation
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: d0      !!  Internal work array, Explicit part
                                                                             !!  of the Right Hand side
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dd      !!  Internal work array, Right hand side
                                                                             !!  of the momentum eq. at (N,M)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: deltau  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dfu     !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nmlb:gdp%d%nmub)    , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: evap    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: fcorio  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: fxw     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gsqiu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gud     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: guu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: guv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: guz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvd     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: gvz     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: patm    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)      , intent(in)  :: precip  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: pship   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: rlabda  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: s0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: s1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: taubpu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: taubsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: tetau   !!  Factor for upwind approach S0
                                                                             !!  can be 0.0, 0.5 or 1.0 depending
                                                                             !!  on value of HU, DCO, KSPU and UMEAN
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: tgfsep  !!  Water elev. induced by tide gen.force
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: tp      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: umean   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: vnu2d   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: windu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: wsu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)                    :: wsbodyu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: qzk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: vicww   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)            :: w1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak     !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbk     !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbka    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbkc    !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck     !!  Internal work array (in CUCNP & UZD)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: d0k     !!  Internal work array
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: ddk     !!  Internal work array, diagonal space
                                                                             !!  at (N,M,K)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: diapl   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzs0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzu0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: dzv0    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: p0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: porosu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: qxk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: qyk     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: rnpl    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: rxx     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: rxy     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: drhodx  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: u0      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: ubrlsu  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: ustokes !  Description and declaration in trisol.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax+2)            :: vicuv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                                     :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                                   :: zk
    real(fp), dimension(kmax, 2, norow)                           :: circ3d  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                     :: disch   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                     :: umdis   !  Description and declaration in esm_alloc_real.f90
    logical                                                       :: flood   ! Flag for activating flooding part of checku subroutine 
    character(1), dimension(nsrc)                                 :: dismmt  !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer       :: ddb
    integer       :: i
    integer       :: icxy
    integer       :: intdir
    integer       :: itr
    integer       :: k
    integer       :: kenm
    integer       :: kk
    integer       :: kkmin
    integer       :: kkmax
    integer       :: m
    integer       :: mmaxddb
    integer       :: n
    integer       :: nhystp
    integer       :: nm
    integer       :: nmaxddb
    integer       :: nmd
    integer       :: nmf
    integer       :: nmlu
    integer       :: nmu
    real(hp)      :: bi
    real(fp)      :: dxid
    real(fp)      :: dxiu
    real(fp)      :: eps
    real(fp)      :: fac
    real(fp)      :: hdti
    real(fp)      :: hnm
    real(fp)      :: hnmd
    real(fp)      :: pr
    real(fp)      :: s1u
    character(26) :: errtxt
!
!! executable statements -------------------------------------------------------
!
    wind       => gdp%gdprocs%wind
    culvert    => gdp%gdprocs%culvert
    zmodel     => gdp%gdprocs%zmodel
    wavcmp     => gdp%gdprocs%wavcmp
    bubble     => gdp%gdprocs%bubble
    maseva     => gdp%gdheat%maseva
    rhow       => gdp%gdphysco%rhow
    ag         => gdp%gdphysco%ag
    iro        => gdp%gdphysco%iro
    hdt        => gdp%gdnumeco%hdt
    lundia     => gdp%gdinout%lundia
    !
    call timer_start(timer_sud_rest, gdp)
    ddb     = gdp%d%ddbound
    nmaxddb = nmax + 2*gdp%d%ddbound
    mmaxddb = mmax + 2*gdp%d%ddbound
    hdti    = 1.0_fp / hdt
    icxy    = max(icx, icy)
    itr     = 0
    flood   = .false.
    !
    ! Compute depth averaged velocities	UMEAN and total depths in velocity points HU
    !
    ! This is necessary because umean and hu are computed based on the velocities near the free surface only
    ! in Z_CHECKU (for flooding purposes) 
    ! and because Z_SUD can be repeated in case of drying in Z_DRYCHK (DRYFLP <> NO)
    !
    umean = 0.0_fp
    do nm = 1, nmmax
       if (kfu(nm)==1) then
          hnm = 0.0_fp
          do k = kfumin(nm), kfumx0(nm)
             umean(nm) = umean(nm) + u0(nm, k)*dzu0(nm, k)
             hnm       = hnm + dzu0(nm, k)
          enddo
          hnm       = max(hnm, 0.01_fp)
          umean(nm) = umean(nm)/hnm
       endif
    enddo
    !
    call upwhu(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
             & zmodel    ,kcs       ,kcu       ,kspu      ,dps       , &
             & s0        ,dpu       ,umean     ,hu        ,gdp       )
    !
    nmu = +icx
    do nm = 1, nmmax
       hu(nm) = max(hu(nm), 0.01_fp)
       nmu       = nmu + 1
       tetau(nm) = 0.5_fp
       if (kfu(nm) == 1) then
          if (umean(nm) >= 0.001_fp) then
             tetau(nm) = 1.0_fp
          elseif (umean(nm) <= -0.001_fp) then
             tetau(nm) = 0.0_fp
          else
             tetau(nm) = 1.0_fp
             if (s0(nmu) > s0(nm)) then
                tetau(nm) = 0.0_fp
             endif
          endif
       endif
    enddo
    call timer_stop(timer_sud_rest, gdp)
    !
    call timer_start(timer_sud_cucnp, gdp)
    call z_cucnp(j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
               & icy       ,nsrc      ,kcs       ,kcs45     ,kcscut    , &
               & kfu       ,kfuz0     ,kfumin    ,kfumx0    ,kfv       , &
               & kfvz0     ,kfvmin    ,kfvmx0    ,dzv0      ,dzs0      , &
               & kfs       ,kfsz0     ,kfsmin    ,kfsmx0    ,kcu       , &
               & u0        ,v1        ,w1        ,hu        ,dzu0      , &
               & guu       ,gvv       ,gvu       ,guv       ,gsqs      , &
               & gud       ,gvd       ,guz       ,gvz       ,gsqiu     , &
               & disch     ,umdis     ,kspu      ,mnksrc    ,dismmt    , &
               & aak       ,bbk       ,cck       ,ddk       ,bbka      , &
               & bbkc      ,vicuv     ,vnu2d     ,vicww     ,tgfsep    , &
               & drhodx    ,wsu       ,wsbodyu   ,taubpu    ,taubsu    ,rxx       , &
               & rxy       ,windu     ,patm      ,fcorio    ,p0        , &
               & tp        ,rlabda    ,dfu       ,deltau    ,fxw       , &
               & ubrlsu    ,pship     ,diapl     ,rnpl      ,cfurou    , &
               & qxk       ,qyk       ,umean     ,dps       ,s0        , &
               & ustokes   ,gdp       )
    call timer_stop(timer_sud_cucnp, gdp)
    !
    ! When neighbor cells ly higher, mass in/outflow is added to current column
    !
    call timer_start(timer_sud_rest, gdp)
    do nm = 1, nmmax
       if (kcs(nm) /= 0) then
          do k = 1, kmax
             d0k(nm, k) = ( - qyk(nm, k) + qyk(nm - icy, k)) / gsqs(nm)
          enddo
       endif
    enddo
    !
    ! IN LAYER 1 DUE TO PERCIPITATION/EVAPORATION
    !     FOR TIME DEPENDENT INPUT OR HEAT MODEL WITH SPECIAL REQUEST
    !
    ! KFS is not yet corrected for flooding in Z_CHECKU
    !
    if (maseva > 0) then
       do nm = 1, nmmax
          kenm = min(1, kfu(nm) + kfu(nm - icx) + kfv(nm) + kfv(nm - icy))
          if (kcs(nm)==1) then
             k = kfsmx0(nm)
             d0k(nm, k) = d0k(nm, k) + precip(nm)
             if (kenm > 0) then
                d0k(nm, k) = d0k(nm, k)- evap(nm)/rhow
             endif
          endif
       enddo
    endif
    !
    ! ADDITION OF DISCHARGES (suction only permitted if the point isn't dry)
    !
    ! k=0 : discharge divided over total column
    ! k<>0: discharge in cell k
    !
    do i = 1, nsrc
       nm   = (mnksrc(5, i) + ddb) + ((mnksrc(4, i) - 1) + ddb) * icxy
       k    = mnksrc(6, i)
       kenm = min(1, kfu(nm) + kfu(nm - icx) + kfv(nm) + kfv(nm - icy))
       if (kenm/=0 .or. disch(i)>=0.0_fp) then
          if (k/=0) then
             !
             ! The order is important at dry points
             !
             if (k>kfsmx0(nm)) k = kfsmx0(nm)
             if (k<kfsmin(nm)) k = kfsmin(nm)
             d0k(nm, k) = d0k(nm, k) + disch(i)/gsqs(nm)
          else
             do kk = kfsmin(nm), max(kfsmx0(nm),kfsmin(nm))
                !
                ! In case of only one layer, add discharge in that layer
                !
                if (kfsmin(nm) == kfsmx0(nm)) then
                   d0k(nm, kk) = d0k(nm, kk) + disch(i)/gsqs(nm)
                else
                   d0k(nm, kk) = d0k(nm, kk) + disch(i)*dzs0(nm, kk)               &
                               & /( max((real(dps(nm),fp) + s0(nm)), 0.01_fp)*gsqs(nm) )
                endif
             enddo
          endif
       else
          if (bubble) then
             !
             !  in this version switched of because of bubble screens
             !
          else
             write (errtxt, '(i0,i3)') nst, i
             call prterr(lundia    ,'S208'    ,trim(errtxt))
          endif 
       endif
       !
       ! in case of an intake for an intake/outfall combination:
       !
       if (mnksrc(7, i) >= 2) then
          nm   = (mnksrc(2, i) + ddb) + ((mnksrc(1, i) - 1) + ddb)*icxy
          k    = mnksrc(3, i)
          kenm = min(1, kfu(nm) + kfu(nm - icx) + kfv(nm) + kfv(nm - icy))
          if (kenm/=0 .or. -disch(i)>=0.0_fp) then
             if (k /= 0) then
                !
                ! The order is important at dry points
                !
                if (k>kfsmx0(nm)) k = kfsmx0(nm)
                if (k<kfsmin(nm)) k = kfsmin(nm)
                d0k(nm, k) = d0k(nm, k) - disch(i)/gsqs(nm)
             else
                do kk = kfsmin(nm), max(kfsmx0(nm),kfsmin(nm))
                   !
                   ! In case of only one layer, add discharge in that layer
                   !
                   if (kfsmin(nm) == kfsmx0(nm)) then
                      d0k(nm, kk) = d0k(nm, kk) - disch(i)/gsqs(nm)
                   else
                      d0k(nm, kk) = d0k(nm, kk) - disch(i)*dzs0(nm, kk)               &
                                  & /( max((real(dps(nm),fp) + s0(nm)), 0.01_fp)*gsqs(nm) )
                   endif
                enddo
             endif
          elseif (mnksrc(7, i) /= 3) then
             !
             ! in case of a culvert no warning generated
             !
             write (errtxt, '(i0,i3)') nst, i
             call prterr(lundia    ,'S208'    ,trim(errtxt))
          else
          endif
       endif
    enddo
    !
    ! Initialise arrays a - dd for all (nm)
    !
    a  = 0.0_fp
    b  = 1.0_fp
    c  = 0.0_fp
    d0 = 0.0_fp
    aa = 0.0_fp
    bb = 1.0_fp
    cc = 0.0_fp
    dd = 0.0_fp
    !
    do nm = 1, nmmax
       d (nm) = s0(nm)
    enddo
    !
    ! Compare for u mean [m/s] ---> divided by bbk [=1/s] ==> [m]
    !
    do nm = 1, nmmax
       if (kfu(nm) == 1) then
          hnm = 0.0_fp
          do k = kfumin(nm), kfumx0(nm)
             hnm = hnm + dzu0(nm,k)
          enddo
          do k = kfumin(nm), kfumx0(nm)
             if (kfuz0(nm,k) == 1) then
                fac    = dzu0(nm, k) * porosu(nm, k) / (bbk(nm, k)*hnm)
                aa(nm) = aa(nm) + aak(nm, k)*fac
                cc(nm) = cc(nm) + cck(nm, k)*fac
                dd(nm) = dd(nm) + ddk(nm, k)*fac
             endif
          enddo
       endif
    enddo
    call timer_stop(timer_sud_rest, gdp)
    !
    ! BOUNDARY CONDITIONS
    !
    call timer_start(timer_sud_cucbp, gdp)
    call cucbp(kmax      ,norow     ,icx       , &
             & icy       ,zmodel    ,irocol    ,kcs       ,kfu       , &
             & kfumin    ,kfumx0    ,s0        ,u0        ,dpu       , &
             & hu        ,umean     ,tetau     ,guu       ,gvu       , &
             & dzu0      ,thick     ,circ2d    ,circ3d    ,a         , &
             & b         ,c         ,d         ,aa        ,bb        , &
             & cc        ,dd        ,aak       ,bbk       ,cck       , &
             & ddk       ,crbc      ,wavcmp    ,gdp       )
    call timer_stop(timer_sud_cucbp, gdp)
    !
    ! Sum d0k in d0
    !
    call timer_start(timer_sud_rest, gdp)
    do nm = 1, nmmax
       if (kcs(nm) /= 0) then
          do k = 1, kmax
             d0(nm) = d0(nm) + d0k(nm, k)
          enddo
          d0(nm) = d0(nm) + s0(nm)*hdti
       endif
    enddo
    call timer_stop(timer_sud_rest, gdp)
    !
 9999 continue
    !
    call timer_start(timer_sud_rest, gdp)
    itr = 0
    !
    ! SET UP SYSTEM OF EQUATIONS FOR INTERIOR POINTS
    !
    do nm = 1, nmmax
       nmd = nm - icx
       if (kcs(nm) == 1) then
          hnm = 0.0_fp
          do k = kfumin(nm), kfumx0(nm)
             hnm = hnm + dzu0(nm,k)
          enddo
          hnmd = 0.0_fp
          do k = kfumin(nmd), kfumx0(nmd)
             hnmd = hnmd + dzu0(nmd,k)
          enddo
          dxid  = hnmd * guu(nmd) / gsqs(nm)
          dxiu  = hnm  * guu(nm)  / gsqs(nm)
          a(nm) = aa(nmd) * dxid
          b(nm) = hdti + dxid*cc(nmd) - aa(nm)*dxiu
          c(nm) = -cc(nm) * dxiu
          d(nm) = d0(nm) - dd(nm)*dxiu + dd(nmd)*dxid
       endif
    enddo
    call timer_stop(timer_sud_rest, gdp)
    !
    !        Give Mapper chance to build the coupling equations
    !        Note that this is a two-stage process. First, coupling equations
    !        are built for the coupling points start+1;end-1
    !        Secondly, coupling equations are built for the `end' coupling points,
    !        start and end.
    !
    nhystp = nxtstp(d3dflow_build_adi_zeta, gdp)
    !
    ! SOLUTION TRIDIAGONAL SYSTEM FOR THE WATERLEVELS
    !
    if (nhystp==noneighbors) then
       !
       ! Single domain case without domain decomposition
       !
       ! DOUBLE SWEEP RECURSION
       !
       call timer_start(timer_sud_rest, gdp)
       nmf = icx - icxy
       do n = 1, nmaxddb
          nmf = nmf + icy
          if (kcs(nmf) /= 0) then
             bi     = 1.0_hp / real(b(nmf),hp)
             c(nmf) = real(c(nmf),hp) * bi
             d(nmf) = real(d(nmf),hp) * bi
          endif
       enddo
       do m = 2, mmaxddb
          nm = m*icx - icxy
          do n = 1, nmaxddb
             nm = nm + icy
             if (kcs(nm) /= 0) then
                bi    = 1.0_hp / (real(b(nm),hp) - real(a(nm),hp)*real(c(nm-icx),hp))
                c(nm) = real(c(nm),hp) * bi
                d(nm) = (real(d(nm),hp) - real(a(nm),hp)*real(d(nm-icx),hp)) * bi
             endif
          enddo
       enddo
       !
       ! BACK SWEEP
       !
       nmlu = mmaxddb*icx - icxy
       do n = 1, nmaxddb
          nmlu = nmlu + icy
          if (kcs(nmlu) /= 0) then
             s1(nmlu) = d(nmlu)
          endif
       enddo
       do m = mmaxddb - 1, 1, -1
          nm = m*icx - icxy
          do n = 1, nmaxddb
             nm = nm + icy
             if (kcs(nm) /= 0) then
                d (nm) = d(nm) - c(nm)*d(nm + icx)
                s1(nm) = d(nm)
             endif
          enddo
       enddo
       !
       call timer_stop(timer_sud_rest, gdp)
    else
       !
       ! Domain decomposition:
       !
       !       METHOD OF WANG
       !
       !       First part of Wang's algoritm: pre-elimination:
       !
       call timer_start(timer_sud_wangpre, gdp)
       call wangp1(s1        ,kcs       ,irocol    ,norow     ,icx       , &
                 & icy       ,j         ,nmmaxj    ,a         ,b         , &
                 & c         ,d         ,gdp       )
       call timer_stop(timer_sud_wangpre, gdp)
       !
       !       Now second part of Wang's algoritm: elimination of reduced
       !       system. This is carried out by a global mapper process.
       !       At end of global mapper process:
       !       at coupling point at the left side and
       !       at last computational point at the right side we have that
       !       a=c=0, b=1 and d=zeta=new water elevation
       !
       !       Now Mapper builds and solves the reduced system of equation
       !
       if (icx == 1) then
          intdir = 0
       else
          !
          !          intdir = 1 corresponds to left_to_Right direction
          !          (see GAWS routines)
          !
          intdir = 1
       endif
       call timer_start(timer_sud_gwsslv, gdp)
       call gwsslv(intdir    )
       call timer_stop(timer_sud_gwsslv, gdp)
       !
       !       Now third part of Wang's algoritm: back substitution
       !
       call timer_start(timer_sud_wangback, gdp)
       call wangp3(s1        ,kcs       ,irocol    ,norow     ,icx       , &
                 & icy       ,j         ,nmmaxj    ,a         ,b         , &
                 & c         ,d         ,gdp       )
       call timer_stop(timer_sud_wangback, gdp)
       !
       !       in case of Hydra (DD method, Wang approach) array d does
       !       not contain the solution, but the right-hand side evaluation.
       !       Since array d (and also s1) is used in the remainder of SUD
       !       for the new water elevation, we have to copy s1 to d.
       !
       do nm = 1, nmmax
          d(nm) = s1(nm)
       enddo
    endif
    !
    call timer_start(timer_sud_rest, gdp)
    !
    ! TOTAL WATERDEPTH DRYING IN SUD
    !
    do nm = 1, nmmax
       nmu = nm + icx
       if (kfu(nm) == 1) then
          hnm = tetau(nm)*d(nm) + (1.0_fp - tetau(nm))*d(nmu) + dpu(nm)
          !
          ! CHECK FOR DRYING
          !
          if (hnm <= 0.0_fp) then
             aa (nm) = 0.0_fp
             bb (nm) = 1.0_fp
             cc (nm) = 0.0_fp
             dd (nm) = 0.0_fp
             kfu(nm) = 0
             do k = 1, kmax
                kfuz0(nm, k) = 0
             enddo
             !kfumax(nm) = -1
             itr = 1
          endif
       endif
    enddo
    call timer_stop(timer_sud_rest, gdp)
    !        print *,itr, '    ' , icx
    !
    ! Synchronize on drying before finishing solve zeta
    !
    nhystp = nxtdry(d3dflow_check_sud_dry, itr, gdp)
    !
    ! repeat computation if point is set dry
    !
    if (nhystp==d3dflow_build_adi_zeta .or.     &
      & (nhystp==noneighbors .and. itr==1)) goto 9999
    !
    ! adapt discharge boundary conditions
    !
    call timer_start(timer_sud_cucdp, gdp)
    call cucdp(kfu       ,irocol    ,norow     ,j         ,nmmaxj    , &
             & icx       ,icy       ,bb        ,gdp       )
    call timer_stop(timer_sud_cucdp, gdp)
    !
    call timer_start(timer_sud_rest, gdp)
    do nm = 1, nmmax
       if (kfu(nm) == 1) then
          do k = kfumin(nm), kmax
             if (kfuz0(nm,k) == 1) then
                pr         = aak(nm, k)*d(nm) + cck(nm, k)*d(nm + icx)
                u1(nm, k)  = (ddk(nm, k) - pr) / bbk(nm, k)
                qxk(nm, k) = dzu0(nm, k) * u1(nm, k) * guu(nm) * porosu(nm, k)
             else
                u1(nm, k)  = 0.0_fp
                qxk(nm, k) = 0.0_fp
             endif
          enddo
       elseif (kcu(nm)/=0) then
          do k = kfumin(nm), kmax
             u1(nm, k)  = 0.0_fp
             qxk(nm, k) = 0.0_fp
          enddo
       else
       endif
    enddo
    !
    ! Compute umean again based on new velocities
    !
    do nm = 1, nmmax
       umean(nm) = 0.0_fp
       hnm       = 0.0_fp
       if (kfu(nm)==1) then
          do k = kfumin(nm), kfumx0(nm)
             umean(nm) = umean(nm) + u1(nm, k)*dzu0(nm, k)
             hnm       = hnm + dzu0(nm,k)
          enddo
          hnm       = max(hnm, 0.01_fp)
          umean(nm) = umean(nm)/hnm
       endif
    enddo
    !
    nhystp = nxtstp(d3dflow_finish_wang, gdp)
    call timer_stop(timer_sud_rest, gdp)
    !
    if (kmax > 1) then
       !
       ! COMPUTATION VERTICAL VELOCITIES AND DISCHARGES
       !
       call timer_start(timer_sud_veldisch, gdp)
       !
       qzk = 0.0_fp
       w1  = 0.0_fp
       !
       do nm = 1, nmmax
          if (kcs(nm)*kfs(nm) == 1) then
             do k = kfsmin(nm), kfsmx0(nm)-1 
                qzk(nm, k) = qzk(nm, k - 1)                                             &
                           & + ( - qxk(nm, k) + qxk(nm - icx, k) + d0k(nm, k)*gsqs(nm))
                w1 (nm, k) = qzk(nm, k)/gsqs(nm)
             enddo
          endif
       enddo
       call timer_stop(timer_sud_veldisch, gdp)
    endif
end subroutine z_sud
