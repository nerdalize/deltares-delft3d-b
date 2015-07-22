subroutine wrtmap(lundia    ,error     ,trifil    ,selmap    ,itmapc    , &
                  & rhow      ,mmax      , &
                  & kmax      ,nmaxus    ,lstsci    ,ltur      , &
                  & nsrc      ,zmodel    ,kcs       ,kfs       ,kfu       , &
                  & kfv       ,kfumin    ,kfvmin    ,kfumax    ,kfvmax    , &
                  & kfsmin    ,kfsmax    ,mnksrc    ,ibuff     ,s1        , &
                  & dps       ,dzs1      ,thick     , &
                  & u1        ,v1        ,w1        ,wphy      ,r1        , &
                  & rtur1     ,taubpu    ,taubpv    ,taubsu    ,taubsv    , &
                  & vicww     ,dicww     ,rich      ,rho       ,p1        , &
                  & vortic    ,enstro    ,umnldf    ,vmnldf    ,vicuv     , &
                  & taubmx    ,windu     ,windv     ,velt      ,cvalu0    , &
                  & cvalv0    ,cfurou    ,cfvrou    ,rouflo    ,patm      , &
                  & z0ucur    ,z0vcur    ,z0urou    ,z0vrou    ,ktemp     , &
                  & precip    ,gdp       )
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
!  $Id: wrtmap.f90 1713 2012-07-22 19:21:42Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrtmap.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying groups (1 & 3) to the
!              NEFIS MAP-DAT file
!              Selection is done using SELMAP. For elements like
!              WPHY where KMAX must be > 1 this coupling between
!              KMAX and SELMAP is done in subroutine RDPRFL
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use dfparall
    use globaldata
    !
    use dffunctionals
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: mfg
    integer                         , pointer :: mlg
    integer                         , pointer :: nfg
    integer                         , pointer :: nlg
    integer                         , pointer :: nmaxgl
    integer                         , pointer :: mmaxgl
    integer                         , pointer :: nmmax
    integer                         , pointer :: celidt
    integer                         , pointer :: keva
    integer  , dimension(:)         , pointer :: smlay
    real(fp) , dimension(:,:,:)     , pointer :: fluxu
    real(fp) , dimension(:,:,:)     , pointer :: fluxuc
    real(fp) , dimension(:,:,:)     , pointer :: fluxv
    real(fp) , dimension(:,:,:)     , pointer :: fluxvc
    real(fp)                        , pointer :: rhum
    real(fp)                        , pointer :: tair
    real(fp) , dimension(:)         , pointer :: qeva_out
    real(fp) , dimension(:)         , pointer :: qco_out
    real(fp) , dimension(:)         , pointer :: qbl_out
    real(fp) , dimension(:)         , pointer :: qin_out
    real(fp) , dimension(:)         , pointer :: qnet_out
    real(fp) , dimension(:)         , pointer :: hlc_out
    real(fp) , dimension(:)         , pointer :: hfree_out
    real(fp) , dimension(:)         , pointer :: efree_out
    real(fp) , dimension(:)         , pointer :: rhumarr
    real(fp) , dimension(:)         , pointer :: tairarr
    real(fp) , dimension(:)         , pointer :: clouarr
    real(fp) , dimension(:)         , pointer :: swrfarr
    real(fp) , dimension(:)         , pointer :: qmis_out
    logical                         , pointer :: first
    logical                         , pointer :: rhum_file
    logical                         , pointer :: tair_file
    logical                         , pointer :: clou_file
    logical                         , pointer :: prcp_file
    logical                         , pointer :: swrf_file
    logical                         , pointer :: free_convec    
    type (nefiselement)             , pointer :: nefiselem
    type (flwoutputtype)            , pointer :: flwoutput
!
! Global variables
!
    integer                                                                           , intent(in)  :: itmapc      !!  Current time counter for the MAP data file
    integer                                                                                         :: kmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: ktemp       !  Description and declaration in tricom.f90
    integer                                                                                         :: lstsci      !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: ltur        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: lundia      !  Description and declaration in inout.igs
    integer                                                                                         :: mmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nmaxus      !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nsrc        !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(7, nsrc)                                                              :: mnksrc      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kcs         !  Description and declaration in esm_alloc_real.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfs         !  Description and declaration in esm_alloc_real.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfsmax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfsmin      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfu         !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfumax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfumin      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfv         !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfvmax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfvmin      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(nmaxus, mmax)                                                         :: ibuff       !  Description and declaration in esm_alloc_int.f90
    logical                                                                           , intent(out) :: error       !!  Flag=TRUE if an error is encountered
    logical                                                                           , intent(in)  :: zmodel      !  Description and declaration in procs.igs
    real(fp)                                                                          , intent(in)  :: rhow        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(kmax)                                                   , intent(in)  :: thick       !  Description and declaration in esm_alloc_real.f90
    real(prec)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: dps         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: patm        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: s1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubmx      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubpu      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubpv      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubsu      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: taubsv      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: umnldf      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: vmnldf      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: windu       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: windv       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: precip      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3)            , intent(in)  :: cfurou      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 3)            , intent(in)  :: cfvrou      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: cvalu0      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: cvalv0      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)       , intent(in)  :: dicww       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)       , intent(in)  :: rich        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)       , intent(in)  :: vicww       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)       , intent(in)  :: w1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, ltur) , intent(in)  :: rtur1       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax + 1)     , intent(in)  :: vicuv       !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: dzs1        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: enstro      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: p1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: rho         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: u1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: v1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: vortic      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: wphy        !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lstsci) , intent(in)  :: r1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: z0ucur      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: z0vcur      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: z0urou      !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: z0vrou      !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                      , intent(in)  :: trifil      !!  File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    character(4)                                                                      , intent(in)  :: rouflo      !  Description and declaration in esm_alloc_char.f90
    character(21)                                                                     , intent(in)  :: selmap      !  Description and declaration in tricom.igs
    character(10)                                                                     , intent(in)  :: velt        !! Velocity type 'eulerian' or 'GLM'
!
! Local variables
!
    integer                                       :: fds
    integer                                       :: i             ! Help var.
    integer                                       :: ierror        ! Local error flag for NEFIS files
    integer                                       :: istat
    integer                                       :: k             ! Help var.
    integer                                       :: km
    integer                                       :: kmaxout       ! number of layers to be written to the (history) output files, 0 (possibly) included
    integer                                       :: kmaxout_restr ! number of layers to be written to the (history) output files, 0 excluded
    integer                                       :: l             ! Help var.
    integer                                       :: m             ! Help var.
    integer                                       :: n             ! Help var.
    integer                                       :: nm
    integer    , dimension(1)                     :: idummy        ! Help array to read/write Nefis files
    integer    , dimension(3,5)                   :: uindex
    integer                        , external     :: putelt
    integer                        , external     :: inqmxi
    integer                        , external     :: clsnef
    integer                        , external     :: open_datdef
    integer                        , external     :: neferr
    integer    , dimension(4,0:nproc-1)           :: iarrc         ! array containing collected grid indices 
    integer                                       :: lenlo         ! length of field of current subdomain
    integer                                       :: lengl         ! length of field containing collected data
    integer    , dimension(0:nproc-1)             :: mf            ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)             :: ml            ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)             :: nf            ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)             :: nl            ! last index w.r.t. global grid in y-direction
    integer    , dimension(:,:)    , allocatable  :: ibuff2
    integer    , dimension(:)      , allocatable  :: smlay_restr   ! copy of shlay, excluding layer zero
    real(fp)   , dimension(:,:)    , allocatable  :: rbuff2
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4
    real(fp)   , dimension(:,:,:)  , allocatable  :: zkt           ! Vertical coordinates of layering interfaces
    real(sp)   , dimension(:,:,:)  , allocatable  :: rsbuff3       ! work array
    character(10)                                 :: runit
    character(16)                                 :: grnam1        ! Data-group name defined for the NEFIS-files group 1
    character(16)                                 :: grnam3        ! Data-group name defined for the NEFIS-files group 3
    character(64)                                 :: rdesc
    character(256)                                :: filnam        ! Help var. for FLOW file name
    character(6)                                  :: errmsg        ! Character var. containing the error message to be written to file. The message depend on the error.
!
! Data statements
!
    data grnam1/'map-info-series'/
    data grnam3/'map-series'/
!
!! executable statements -------------------------------------------------------
!
    nefiselem      => gdp%nefisio%nefiselem(nefiswrtmapinf)
    mfg            => gdp%gdparall%mfg
    mlg            => gdp%gdparall%mlg
    nfg            => gdp%gdparall%nfg
    nlg            => gdp%gdparall%nlg
    mmaxgl         => gdp%gdparall%mmaxgl
    nmaxgl         => gdp%gdparall%nmaxgl
    nmmax          => gdp%d%nmmax
    celidt         => nefiselem%celidt
    keva           => gdp%gdtricom%keva
    smlay          => gdp%gdpostpr%smlay
    fluxu          => gdp%gdflwpar%fluxu
    fluxuc         => gdp%gdflwpar%fluxuc
    fluxv          => gdp%gdflwpar%fluxv
    fluxvc         => gdp%gdflwpar%fluxvc
    flwoutput      => gdp%gdflwpar%flwoutput
    rhum           => gdp%gdheat%rhum
    tair           => gdp%gdheat%tair
    qeva_out       => gdp%gdheat%qeva_out
    qco_out        => gdp%gdheat%qco_out
    qbl_out        => gdp%gdheat%qbl_out
    qin_out        => gdp%gdheat%qin_out
    qnet_out       => gdp%gdheat%qnet_out
    hlc_out        => gdp%gdheat%hlc_out
    hfree_out      => gdp%gdheat%hfree_out
    efree_out      => gdp%gdheat%efree_out
    qmis_out       => gdp%gdheat%qmis_out
    rhumarr        => gdp%gdheat%rhumarr
    tairarr        => gdp%gdheat%tairarr
    clouarr        => gdp%gdheat%clouarr
    first          => nefiselem%first
    rhum_file      => gdp%gdheat%rhum_file
    tair_file      => gdp%gdheat%tair_file
    clou_file      => gdp%gdheat%clou_file
    prcp_file      => gdp%gdheat%prcp_file
    free_convec    => gdp%gdheat%free_convec
    !
    ! Initialize local variables
    !
    kmaxout = size(smlay)
    if (smlay(1) == 0) then
       kmaxout_restr = kmaxout - 1
       allocate(smlay_restr(kmaxout_restr))
       smlay_restr   = smlay(2:)
    else
       kmaxout_restr = kmaxout
       allocate(smlay_restr(kmaxout_restr))
       smlay_restr   = smlay
    endif
    filnam  = trifil(1:3) // 'm' // trifil(5:)
    errmsg  = ' '
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    if (first .and. inode == master) then
       !
       ! Set up the element chracteristics
       !
       ! map-info-series
       !
       call addelm(nefiswrtmapinf,'ITMAPC',' ','[   -   ]','INTEGER',4    , &
          & 'timestep number (ITMAPC*DT*TUNIT := time in sec from ITDATE)', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call defnewgrp(nefiswrtmapinf ,filnam    ,grnam1   ,gdp)
       !
       ! map-sed-series
       !
       if (selmap(1:1) == 'Y') then
          call addelm(nefiswrtmap,'S1',' ','[   M   ]','REAL',4              , &
             & 'Water-level in zeta point                                   ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       call addelm(nefiswrtmap,'KFU',' ','[   -   ]','INTEGER',4          , &
          & 'Non-active/active in U-point                                ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrtmap,'KFV',' ','[   -   ]','INTEGER',4          , &
          & 'Non-active/active in V-point                                ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       if (index(selmap(2:3), 'Y') > 0) then
          call addelm(nefiswrtmap,'U1',' ','[  M/S  ]','REAL',4              , &
             & 'U-velocity per layer in U-point ('//trim(velt)//')', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'V1',' ','[  M/S  ]','REAL',4              , &
             & 'V-velocity per layer in V-point ('//trim(velt)//')', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (selmap(4:4) == 'Y') then
          call addelm(nefiswrtmap,'W',' ','[  M/S  ]','REAL',4               , &
             & 'W-omega per layer in zeta point                             ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout   ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (selmap(5:5) == 'Y') then
          call addelm(nefiswrtmap,'WPHY',' ','[  M/S  ]','REAL',4            , &
             & 'W-velocity per layer in zeta point                          ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (index(selmap(6:13), 'Y') /= 0) then
          call addelm(nefiswrtmap,'R1',' ','[   -   ]','REAL',4              , &
             & 'Concentrations per layer in zeta point                      ', &
             & 4         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,lstsci    ,0      , &
             & lundia    ,gdp       )
       endif
       if (flwoutput%difuflux) then
          call addelm(nefiswrtmap,'R1FLX_UU',' ','[   -   ]','REAL',4        , &
             & 'Constituent flux in u-direction (u point)                   ', &
             & 4         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,lstsci    ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'R1FLX_VV',' ','[   -   ]','REAL',4        , &
             & 'Constituent flux in v-direction (v point)                   ', &
             & 4         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,lstsci    ,0      , &
             & lundia    ,gdp       )
       endif
       if (flwoutput%cumdifuflux) then
          call addelm(nefiswrtmap,'R1FLX_UUC',' ','[   -   ]','REAL',4       , &
             & 'Cumulative constituent flux in u-direction (u point)        ', &
             & 4         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,lstsci    ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'R1FLX_VVC',' ','[   -   ]','REAL',4       , &
             & 'Cumulative constituent flux in v-direction (v point)        ', &
             & 4         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,lstsci    ,0      , &
             & lundia    ,gdp       )
       endif
       if (index(selmap(14:15),'Y') /= 0) then
          call addelm(nefiswrtmap,'RTUR1',' ','[   -   ]','REAL',4           , &
             & 'Turbulent quantity per layer in zeta point                  ', &
             & 4         ,nmaxgl    ,mmaxgl    ,kmaxout   ,ltur      ,0      , &
             & lundia    ,gdp       )
       endif
       if (index(selmap(16:17), 'Y') > 0) then
          call addelm(nefiswrtmap,'TAUKSI',' ','[  N/M2 ]','REAL',4          , &
             & 'Bottom stress in U-point                                    ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'TAUETA',' ','[  N/M2 ]','REAL',4          , &
             & 'Bottom stress in V-point                                    ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'TAUMAX',' ','[  N/M2 ]','REAL',4          , &
             & 'Tau_max in zeta points (scalar)                             ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (selmap(18:18) == 'Y') then
          call addelm(nefiswrtmap,'VICWW',' ','[  M2/S ]','REAL',4           , &
             & 'Vertical eddy viscosity-3D in zeta point                    ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout   ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (selmap(19:19) == 'Y') then
          call addelm(nefiswrtmap,'DICWW',' ','[  M2/S ]','REAL',4           , &
             & 'Vertical eddy diffusivity-3D in zeta point                  ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout   ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (index(selmap(18:19),'Y') > 0) then
          call addelm(nefiswrtmap,'RICH',' ','[   -   ]','REAL',4            , &
             & 'Richardson number                                           ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout   ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (selmap(20:20) == 'Y') then
          call addelm(nefiswrtmap,'RHO',' ','[ KG/M3 ]','REAL',4             , &
             & 'Density per layer in zeta point                             ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (selmap(21:21) == 'Y') then
          call addelm(nefiswrtmap,'UMNLDF',' ','[  M/S  ]','REAL',4          , &
             & 'Filtered U-velocity                                         ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'VMNLDF',' ','[  M/S  ]','REAL',4          , &
             & 'Filtered V-velocity                                         ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'VICUV',' ','[ M2/S  ]','REAL',4           , &
             & 'Horizontal eddy viscosity in zeta point                     ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (nsrc > 0) then
          call addelm(nefiswrtmap,'MNKSRC',' ','[   -   ]','INTEGER',4       , &
             & '(M,N,K) indices of discharge sources and time dep. location ', &
             & 2         ,7         ,nsrc      ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (index(selmap(2:3),'Y') > 0) then
          call addelm(nefiswrtmap,'VORTIC',' ','[  1/S  ]','REAL',4          , &
             & 'Vorticity at each layer in depth point                      ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'ENSTRO',' ','[  1/S2 ]','REAL',4          , &
             & 'Enstrophy at each layer in depth point                      ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmaxout_restr,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (index(selmap(2:2), 'Y')>0 .and. zmodel) then
          call addelm(nefiswrtmap,'HYDPRES',' ','[  N/M2 ]','REAL',4         , &
             & 'Non-hydrostatic pressure at each layer in zeta point        ', &
             & 3         ,nmaxus    ,mmax      ,kmaxout_restr,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (flwoutput%air) then
          call addelm(nefiswrtmap,'WINDU',' ','[  M/S  ]','REAL',4           , &
             & 'Wind speed in x-direction (zeta point)                      ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'WINDV',' ','[  M/S  ]','REAL',4           , &
             & 'Wind speed in y-direction (zeta point)                      ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'PATM',' ','[  N/M2  ]','REAL',4           , &
             & 'Air pressure (zeta point)                                   ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          if (clou_file) then
             call addelm(nefiswrtmap,'CLOUDS',' ','[   %   ]','REAL',4          , &
                & 'Cloud coverage percentage (zeta point)                      ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (rhum_file) then
             call addelm(nefiswrtmap,'AIRHUM',' ','[   %   ]','REAL',4          , &
                & 'Relative air humidity (zeta point)                          ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (tair_file) then
             call addelm(nefiswrtmap,'AIRTEM',' ','[ DEG C ]','REAL',4          , &
                & 'Air temperature (zeta point)                                ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (prcp_file) then
             call addelm(nefiswrtmap,'PRECIP',' ','[  MM/H ]','REAL',4          , &
                & 'Precipitation (zeta point)                          ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
       endif
       if (flwoutput%temperature) then
          if (ktemp == 3) then
             !
             ! Different output for Excess Temperature model
             !
             call addelm(nefiswrtmap,'HLC', ' ' , '[W/M2K ]','REAL',4           , &
                & 'Exchange coefficient in Excess temperature model            ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrtmap,'QNET', ' ' , '[W/M2 ]','REAL',4           , &
                & 'Total nett heat flux in zeta point                          ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )          
          elseif (ktemp > 0) then
             call addelm(nefiswrtmap,'QEVA', ' ' , '[W/M2 ]','REAL',4           , &
                & 'Evaporation heat flux in zeta point                         ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrtmap,'QCO',  ' ' , '[W/M2 ]','REAL',4           , &
                & 'Heat flux of forced convection in zeta point                ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrtmap,'QBL',  ' ' , '[W/M2 ]','REAL',4           , &
                & 'Nett back radiation in zeta point                           ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrtmap,'QIN',  ' ' , '[W/M2 ]','REAL',4           , &
                & 'Nett solar radiation in zeta point                          ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrtmap,'QNET', ' ' , '[W/M2 ]','REAL',4           , &
                & 'Total nett heat flux in zeta point                          ', &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             if (free_convec) then 
                call addelm(nefiswrtmap,'HFREE',' ' , '[W/M2 ]','REAL',4           , &
                   & 'Free convection of sensible heat in zeta point              ', &
                   & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                   & lundia    ,gdp       )
                call addelm(nefiswrtmap,'EFREE',' ' , '[W/M2 ]','REAL',4           , &
                   & 'Free convection of latent heat in zeta point                ', &
                   & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                   & lundia    ,gdp       )
             endif
             if (keva == 3) then
                call addelm(nefiswrtmap,'QMIS', ' ' , '[W/M2 ]','REAL',4           , &
                   & 'Computed minus derived heat flux in zeta point              ', &
                   & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
                   & lundia    ,gdp       )
             endif
          else
          endif
       endif
       if (flwoutput%chezy) then
          call addelm(nefiswrtmap,'CFUROU',' ','[M0.5/S ]','REAL',4          , &
             & 'Chezy roughness parameter in U-point                        ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'CFVROU',' ','[M0.5/S ]','REAL',4          , &
             & 'Chezy roughness parameter in V-point                        ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (flwoutput%roughness) then
          select case (rouflo)
          case ('CHEZ')
             runit = '[ M0.5/S ]'
             rdesc = 'Chezy roughness parameter'
          case ('WHIT')
             runit = '[   M    ]'
             rdesc = 'Nikuradse roughness parameter'
          case ('MANN')
             runit = '[S/M{1/3}]'
             rdesc = 'Manning roughness parameter'
          case ('Z   ')
             runit = '[   M    ]'
             rdesc = 'Z0 roughness parameter'
          end select
          call addelm(nefiswrtmap,'ROUMETU',' ',runit      ,'REAL',4         , &
             & trim(rdesc) // ' in U-point'                                  , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'ROUMETV',' ',runit      ,'REAL',4         , &
             & trim(rdesc) // ' in V-point'                                  , &
             & 2         ,nmaxus    ,mmax      ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (flwoutput%z0cur) then
          call addelm(nefiswrtmap,'Z0UCUR',' ','[   M   ]','REAL',4          , &
             & 'Current only z0 bed roughness in U-point                    ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'Z0VCUR',' ','[   M   ]','REAL',4          , &
             & 'Current only z0 bed roughness in V-point                    ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (flwoutput%z0rou) then
          call addelm(nefiswrtmap,'Z0UROU',' ','[   M   ]','REAL',4          , &
             & 'Wave enhanced z0 bed roughness in U-point                   ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrtmap,'Z0VROU',' ','[   M   ]','REAL',4          , &
             & 'Wave enhanced z0 bed roughness in V-point                   ', &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (flwoutput%layering) then
          call addelm(nefiswrtmap,'LAYER_INTERFACE',' ','[  M  ]','REAL',4   , &
             & 'Vertical coordinate of layer interface                      ', &
             & 3         ,nmaxgl    ,mmaxgl    ,kmax + 1  ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       !
       call defnewgrp(nefiswrtmap ,filnam    ,grnam3   ,gdp)
       !
       ! Get start celidt for writing
       !
       nefiselem => gdp%nefisio%nefiselem(nefiswrtmapinf)
       first     => nefiselem%first
       celidt    => nefiselem%celidt
    endif
    !
    if (parll) then
       !
       ! allocate data arrays for collection data 
       !
       ! gather LOCAL grid indices of all partitions
       !
       call dfsync(gdp)
       call dfgather_grddim(lundia, nfg, nlg, mfg, mlg, nmaxgl, mmaxgl, &
          &                 nf, nl, mf, ml, iarrc, lengl, lenlo, gdp )
       !
       ! broadcast LOCAL grid indices to ALL partitions
       ! so every partition knows the dimensions and positions
       ! of the other partitions in the global domain
       !
       call dfbroadc ( iarrc, 4*nproc, dfint, gdp )
       call dfbroadc ( nf, nproc, dfint, gdp )
       call dfbroadc ( nl, nproc, dfint, gdp )
       call dfbroadc ( mf, nproc, dfint, gdp )
       call dfbroadc ( ml, nproc, dfint, gdp )
    endif
    !
    ierror = 0
    if (inode == master) ierror = open_datdef(filnam   ,fds      )
    if (ierror /= 0) goto 999
    if (inode == master) then
       if (first) then
          !
          ! end of initialization, don't come here again
          !
          ierror = inqmxi(fds, grnam1, celidt)
          first = .false.
       endif
       !
       ! Writing of output on every itmapc
       !
       celidt = celidt + 1
       !
       idummy(1)   = itmapc
       uindex(1,1) = celidt
       uindex(2,1) = celidt
       !
       ! Group map-series, identified with nefiswrtmap, must use the same
       ! value for celidt.
       ! Easy solution:
       gdp%nefisio%nefiselem(nefiswrtmap)%celidt = celidt
       ! Neat solution in pseudo code:
       ! subroutine wrtmap
       ! integer :: celidt
       ! call wrtmapinf(celidt)
       ! call wrtmapdat(celidt)
       ! end subroutine
       !
       ierror      = putelt(fds, grnam1, 'ITMAPC', uindex, 1, idummy)
    endif
    if (ierror/=0) goto 999
    !
    ! group 3, element 'S1' only if SELMAP( 1: 1) = 'Y'
    !
    if (selmap(1:1) == 'Y') then
       if (parll) then
          call dfgather(s1,nf,nl,mf,ml,iarrc,gdp)
       else
          call dfgather_seq(s1,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)
       endif      
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'S1', uindex, 1, glbarr2)
       endif
       ! ad hoc double precision output of S1
       ! allocate(rbuff2(maxgl,mmaxgl)
       ! rbuff2(1:nmaxgl,1:mmaxgl)  = s1(1:nmaxgl,1:mmaxgl)
       ! ierror = putelt(fds, grnam3, 'S1', uindex, 1, rbuff2)
       ! deallocate(rbuff2)  
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: element 'KFU'
    !
    if (parll) then
       call dfgather(kfu,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(kfu,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif
    if (inode == master) then
       ierror = putelt(fds, grnam3, 'KFU', uindex, 1, glbari2)
    endif
         
    if (ierror /= 0) goto 999
    !
    ! group 3: element 'KFV'
    !
    if (parll) then
       call dfgather(kfv,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(kfv,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif          
    if (inode == master) then
       ierror = putelt(fds, grnam3, 'KFV', uindex, 1, glbari2)
    endif
    if (ierror /= 0) goto 999
    !
    ! group 3: element 'U1' & 'V1' only if SELMAP( 2: 3) <> 'NN'
    !
    if (index(selmap(2:3),'Y') > 0) then
       call wrtmap_nmk(1, kmax, ierror, u1, 'U1', kfumin, kfumax)
       if (ierror /= 0) goto 999
       !
       ! group 3: element 'V1'
       !
       call wrtmap_nmk(1, kmax, ierror, v1, 'V1', kfvmin, kfvmax)
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: element 'W' only if kmax > 1 (:=  SELMAP( 4: 4) = 'Y')
    !
    if (selmap(4:4) == 'Y') then
       call wrtmap_nmk(0, kmax, ierror, w1, 'W', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: element 'WPHY' only if KMAX > 1 (:=  SELMAP( 5: 5) = 'Y')
    !
    if (selmap(5:5) == 'Y') then
       call wrtmap_nmk(1, kmax, ierror, wphy, 'WPHY', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: element 'R1', only if LSTSCI > 0
    ! (:= SELMAP( 6:13) <> 'NNNNNNNN')
    !
    if (index(selmap(6:13),'Y') /= 0) then
       call wrtmap_nmkl(1, kmax, lstsci, ierror, r1, 'R1')
       if (ierror /= 0) goto 999
       if (flwoutput%difuflux) then
          !
          ! element 'R1FLX_UU'
          !
          call wrtmap_nmklflux(1, kmax, lstsci, ierror, fluxu, 'R1FLX_UU')
          if (ierror /= 0) goto 999

          !
          ! element 'R1FLX_VV'
          !
          call wrtmap_nmklflux(1, kmax, lstsci, ierror, fluxv, 'R1FLX_VV')
          if (ierror /= 0) goto 999
       endif
       !
       if (flwoutput%cumdifuflux) then
          !
          ! element 'R1FLX_UUC'
          !
          call wrtmap_nmklflux(1, kmax, lstsci, ierror, fluxuc, 'R1FLX_UUC')
          if (ierror /= 0) goto 999
          !
          ! element 'R1FLX_VVC'
          !
          call wrtmap_nmklflux(1, kmax, lstsci, ierror, fluxvc, 'R1FLX_VVC')
          if (ierror /= 0) goto 999
       endif
    endif
    !
    ! group 3: element 'RTUR1', only if LTUR > 0
    ! (:= SELMAP(14:15) <> 'NN')
    !
    if (index(selmap(14:15),'Y') /= 0) then
       call wrtmap_nmkl(0, kmax, ltur, ierror, rtur1, 'RTUR1')
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: element 'TAUKSI' & 'TAUETA' only if SELMAP(16:17) <> 'NN'
    !
    if (index(selmap(16:17),'Y') > 0) then
       allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
       rbuff2(:, :) = -999.0_fp
       if (zmodel) then
          do m = 1, mmax
             do n = 1, nmaxus
                km = kfumin(n, m)
                if (1<=km .and. km<=kmax) then
                   rbuff2(n, m) = ( taubpu(n, m)*u1(n, m, km) + taubsu(n, m) ) * rhow
                endif
             enddo
          enddo
       else
          km = kmax
          do m = 1, mmax
             do n = 1, nmaxus
                rbuff2(n, m) = ( taubpu(n, m)*u1(n, m, km) + taubsu(n, m) ) * rhow
             enddo
          enddo
       endif
       if (parll) then
          call dfgather(rbuff2,nf,nl,mf,ml,iarrc,gdp)
       else
          call dfgather_seq(rbuff2,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)
       endif   
       deallocate(rbuff2)
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'TAUKSI', uindex, 1, glbarr2)
       endif
       if (ierror /= 0) goto 999
       !
       ! group 3: element 'TAUETA'
       !
       allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
       rbuff2(:, :) = -999.0_fp
       if (zmodel) then
          do m = 1, mmax
             do n = 1, nmaxus
                km = kfvmin(n, m)
                if (1<=km .and. km<=kmax) then
                   rbuff2(n, m) = ( taubpv(n, m)*v1(n, m, km) + taubsv(n, m) ) * rhow
                endif   
             enddo
          enddo
       else
          km = kmax
          do m = 1, mmax
             do n = 1, nmaxus
                rbuff2(n, m) = ( taubpv(n, m)*v1(n, m, km) + taubsv(n, m) ) * rhow
             enddo
          enddo
       endif
       if (parll) then
          call dfgather(rbuff2,nf,nl,mf,ml,iarrc,gdp)
       else
          call dfgather_seq(rbuff2,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)
       endif      
       deallocate(rbuff2)
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'TAUETA', uindex, 1, glbarr2)
       endif
       if (ierror /= 0) goto 999
       !
       ! group 3: element 'TAUMAX'
       !
       if (parll) then
          call dfgather(taubmx,nf,nl,mf,ml,iarrc,gdp)
       else
          call dfgather_seq(taubmx,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)
       endif       
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'TAUMAX', uindex, 1, glbarr2)
       endif
       if (ierror /= 0) goto 999
       !
    endif
    !
    ! group 3: element 'VICWW' if KMAX > 1 (:= SELMAP(18:18) = 'Y')
    ! vicww is defined on cell boundary planes
    !
    if (selmap(18:18) == 'Y') then
       call wrtmap_nmk(0, kmax, ierror, vicww, 'VICWW', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: element 'DICWW' if KMAX > 1 (:= SELMAP(19:19) = 'Y')
    ! dicww is defined on cell boundary planes
    !
    if (selmap(19:19) == 'Y') then
       call wrtmap_nmk(0, kmax, ierror, dicww, 'DICWW', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: element 'RICH' if KMAX > 1 and DICWW or VICWW written to file
    ! (:= SELMAP(18:19) <> 'NN')
    !
    if (index(selmap(18:19),'Y') > 0) then
       call wrtmap_nmk(0, kmax, ierror, rich, 'RICH', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: element 'RHO' if LSAL > 0 or LTEM > 0
    ! (:= SELMAP(20:20) = 'Y')
    !
    if (selmap(20:20) == 'Y') then
       call wrtmap_nmk(1, kmax, ierror, rho, 'RHO', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: elements 'UMNLDF', 'VMNLDF' and 'VICUV' if htur2d = true
    ! (:= SELMAP(21:21) = 'Y')
    !
    if (selmap(21:21) == 'Y') then
       !
       ! group 3: element 'UMNLDF'
       !
       if (parll) then
          call dfgather(umnldf,nf,nl,mf,ml,iarrc,gdp)
       else
          call dfgather_seq(umnldf,1-gdp%d%nlb,1-gdp%d%mlb, nmaxgl,mmaxgl)
       endif      
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'UMNLDF', uindex, 1, glbarr2)
       endif
       if (ierror /= 0) goto 999
       !
       ! group 3: element 'VMNLDF'
       !
       !
       if (parll) then
          call dfgather(vmnldf,nf,nl,mf,ml,iarrc,gdp)
       else
          call dfgather_seq(vmnldf, 1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)
       endif      
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'VMNLDF', uindex, 1, glbarr2)
       endif
       if (ierror /= 0) goto 999
       !
       ! group 3: element 'VICUV'
       ! kmax+1 contains initial values and should not be written
       !
       call wrtmap_nmk(1, kmax+1, ierror, vicuv, 'VICUV', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
    endif
    if (nsrc>0 .and. inode==master) then
       !
       ! group 3, element 'MNKSRC' when discharges are present
       !
       allocate(ibuff2(7,nsrc))
       do i=1,nsrc
          !
          ! mnksrc contains indices with respect to this partion
          ! transfer into global indices
          !
          ibuff2(1,i) = mnksrc(1,i) + mfg - 1
          ibuff2(2,i) = mnksrc(2,i) + nfg - 1
          ibuff2(3,i) = mnksrc(3,i)
          ibuff2(4,i) = mnksrc(4,i) + mfg - 1
          ibuff2(5,i) = mnksrc(5,i) + nfg - 1
          ibuff2(6,i) = mnksrc(6,i)
          ibuff2(7,i) = mnksrc(7,i)
       enddo
       ierror = putelt(fds, grnam3, 'MNKSRC', uindex, 1, ibuff2)
       deallocate(ibuff2)
       if (ierror/=0) goto 999
    endif
    !
    ! group 3: element 'VORTIC' & 'ENSTRO' only if SELMAP( 2: 3) <> 'NN'
    ! First VORTIC
    !
    if (index(selmap(2:3),'Y') > 0) then
       call wrtmap_nmk(1, kmax, ierror, vortic, 'VORTIC', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
       !
       ! Next ENSTRO
       !
       call wrtmap_nmk(1, kmax, ierror, enstro, 'ENSTRO', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
    endif
    !
    ! group 3: element 'HYDPRES'
    !
    if (index(selmap(4:4),'Y')>0 .and. zmodel) then
       call wrtmap_nmk(1, kmax, ierror, p1, 'HYDPRES', kfsmin, kfsmax)
       if (ierror /= 0) goto 999
    endif
    !
    if (flwoutput%chezy) then
       !
       ! element 'CFUROU'
       !
       call wrtmap_nm(ierror, cvalu0, 'CFUROU')
       if (ierror /= 0) goto 999
       !
       ! element 'CFVROU'
       !
       call wrtmap_nm(ierror, cvalv0, 'CFVROU')
       if (ierror /= 0) goto 999
    endif
    if (flwoutput%roughness) then
       !
       ! element 'ROUMETU'
       !
       allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
       rbuff2(:,:) = cfurou(:,:,2)
       call wrtmap_nm(ierror, rbuff2, 'ROUMETU')
       deallocate(rbuff2)
       if (ierror /= 0) goto 999
       !
       ! element 'ROUMETV'
       !
       allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
       rbuff2(:,:) = cfvrou(:,:,2)
       call wrtmap_nm(ierror, rbuff2, 'ROUMETV')
       deallocate(rbuff2)
       if (ierror /= 0) goto 999
    endif
    if (flwoutput%z0cur) then
       !
       ! element 'Z0UCUR'
       !
       call wrtmap_nm(ierror, z0ucur, 'Z0UCUR')
       if (ierror /= 0) goto 999
       !
       ! element 'Z0VCUR'
       !
       call wrtmap_nm(ierror, z0vcur, 'Z0VCUR')
       if (ierror /= 0) goto 999
    endif
    if (flwoutput%z0rou) then
       !
       ! element 'Z0UROU'
       !
       call wrtmap_nm(ierror, z0urou, 'Z0UROU')
       if (ierror /= 0) goto 999
       !
       ! element 'Z0VROU'
       !
       call wrtmap_nm(ierror, z0vrou, 'Z0VROU')
       if (ierror /= 0) goto 999
    endif
    !
    ! Output of vertical coordinates of the layer interfaces (both for Sigma- and Z-model)
    ! Note: this will NOT work in parallel (yet) !
    !
    if (.not. parll .and. flwoutput%layering) then
       !
       ! element 'LAYER_INTERFACE'
       !
       allocate (zkt(nmaxus,mmax,0:kmax), stat=istat)
       zkt(:, :, :) = -999.0_fp
       if (istat /= 0) then
          write(lundia, '(''ERROR: Memory allocation error in routine WRTMAP'')')
       endif
       !
       ! Vertical coordinates of layer interfaces requested for output?
       ! Calculate time dependent z-coordinate z(nm,k,t) of layer interfaces
       ! Both for Sigma- and Z-model
       !
       call layer_interfaces(zmodel     ,kmax      ,mmax     ,nmaxus   ,s1      , &
                           & dps        ,thick     ,dzs1     ,kcs      ,kfs     , &
                           & kfsmin     ,kfsmax    ,zkt      ,gdp      )
       call sbuff_checksize(mmax*nmaxus*(kmax+1))
       i = 0
       do k = 0, kmax
          do m = 1, mmax
             do n = 1, nmaxus
                i        = i+1
                sbuff(i) = real(zkt(n, m, k),sp)
             enddo
          enddo
       enddo
       !
       ! Deallocate the array with vertical layer coordinates
       !
       deallocate (zkt)
       !
       ierror = putelt(fds, grnam3, 'LAYER_INTERFACE', uindex, 1, sbuff)
       if (ierror /= 0) goto 999
    endif
    !
    ! Output of air parameters: wind, pressure, cloudiness, relative humidity, temperature, and precipitation
    !
    if (flwoutput%air) then
       !
       ! element 'WINDU'
       !
       call wrtmap_nm(ierror, windu, 'WINDU')
       if (ierror /= 0) goto 999
       !
       ! element 'WINDV'
       !
       call wrtmap_nm(ierror, windv, 'WINDV')
       if (ierror /= 0) goto 999
       !
       ! element 'PATM'
       !
       call wrtmap_nm(ierror, patm, 'PATM')
       if (ierror /= 0) goto 999
       !
       if (prcp_file) then
          !
          ! element 'PRECIP'
          !
          ! Convert to mm/h
          !
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2 = precip * 3600000.0_fp
          call wrtmap_nm(ierror, rbuff2, 'PRECIP')
          deallocate(rbuff2)
          if (ierror /= 0) goto 999
       endif
       !
       if (clou_file) then
          !
          ! element 'CLOUDS'
          !
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:, :) = -999.0_fp
          do nm = 1, nmmax
             call nm_to_n_and_m(nm, n, m, gdp)
             rbuff2(n,m) = clouarr(nm)
          enddo
          call wrtmap_nm(ierror, rbuff2, 'CLOUDS')
          deallocate(rbuff2)
          if (ierror /= 0) goto 999
       endif

       if (rhum_file) then
          !
          ! element 'AIRHUM'
          !
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:, :) = -999.0_fp
          do nm = 1, nmmax
             call nm_to_n_and_m(nm, n, m, gdp)
             rbuff2(n,m) = rhumarr(nm)
          enddo
          call wrtmap_nm(ierror, rbuff2, 'AIRHUM')
          deallocate(rbuff2)
          if (ierror /= 0) goto 999
       endif


       if (tair_file) then
          !
          ! element 'AIRTEM'
          !
          allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
          rbuff2(:, :) = -999.0_fp
          do nm = 1, nmmax
             call nm_to_n_and_m(nm, n, m, gdp)
             rbuff2(n,m) = tairarr(nm)
          enddo
          call wrtmap_nm(ierror, rbuff2, 'AIRTEM')
          deallocate(rbuff2)
          if (ierror /= 0) goto 999
       endif
    endif
    !
    !
    ! Output of heat fluxes from temperature model
    !
    if (flwoutput%temperature) then
       if (ktemp == 3) then
          !
          ! element 'HLC'
          !
          if (associated(hlc_out)) then
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = hlc_out(nm)
             enddo
             call wrtmap_nm(ierror, rbuff2, 'HLC')
             deallocate(rbuff2)
             if (ierror /= 0) goto 999
          endif
          !
          ! element 'QNET'
          !
          if (associated(qnet_out)) then
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = qnet_out(nm)
             enddo
             call wrtmap_nm(ierror, rbuff2, 'QNET')
             deallocate(rbuff2)
             if (ierror /= 0) goto 999
          endif
       elseif (ktemp > 0) then
          !
          ! element 'QEVA'
          !
          if (associated(qeva_out)) then
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = qeva_out(nm)
             enddo
             call wrtmap_nm(ierror, rbuff2, 'QEVA')
             deallocate(rbuff2)
             if (ierror /= 0) goto 999
          endif
          !
          ! element 'QCO'
          !
          if (associated(qco_out)) then
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = qco_out(nm)
             enddo
             call wrtmap_nm(ierror, rbuff2, 'QCO')
             deallocate(rbuff2)
             if (ierror /= 0) goto 999
          endif
          !
          ! element 'QBL'
          !
          if (associated(qbl_out)) then
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = qbl_out(nm)
             enddo
             call wrtmap_nm(ierror, rbuff2, 'QBL')
             deallocate(rbuff2)
             if (ierror /= 0) goto 999
          endif
          !
          ! element 'QIN'
          !
          if (associated(qin_out)) then
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = qin_out(nm)
             enddo
             call wrtmap_nm(ierror, rbuff2, 'QIN')
             deallocate(rbuff2)
             if (ierror /= 0) goto 999
          endif
          !
          ! element 'QNET'
          !
          if (associated(qnet_out)) then
             allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
             rbuff2(:, :) = -999.0_fp
             do nm = 1, nmmax
                call nm_to_n_and_m(nm, n, m, gdp)
                rbuff2(n,m) = qnet_out(nm)
             enddo
             call wrtmap_nm(ierror, rbuff2, 'QNET')
             deallocate(rbuff2)
             if (ierror /= 0) goto 999
          endif
          !
          if (free_convec) then
             !
             ! element 'HFREE'
             !
             if (associated(hfree_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = hfree_out(nm)
                enddo
                call wrtmap_nm(ierror, rbuff2, 'HFREE')
                deallocate(rbuff2)
                if (ierror /= 0) goto 999
             endif
             !
             ! element 'EFREE'
             !
             if (associated(efree_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = efree_out(nm)
                enddo
                call wrtmap_nm(ierror, rbuff2, 'EFREE')
                deallocate(rbuff2)
                if (ierror /= 0) goto 999
             endif
          endif

          !
          ! element 'QMIS'
          !
          if (keva == 3) then
             if (associated(qmis_out)) then
                allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
                rbuff2(:, :) = -999.0_fp
                do nm = 1, nmmax
                   call nm_to_n_and_m(nm, n, m, gdp)
                   rbuff2(n,m) = qmis_out(nm)
                enddo
                call wrtmap_nm(ierror, rbuff2, 'QMIS')
                deallocate(rbuff2)
                if (ierror /= 0) goto 999
             endif
          endif
       else
          !
          ! ktemp = 0, no additional output
       endif
    endif
    !
    if (inode == master) ierror = clsnef(fds)
    !
    ! write error message if error occured and set error= .true.
    ! the files will be closed in clsnef (called in triend)
    !
 999 continue
    if (inode == master) then
       call dfcleanup_glbarrs
    endif
    call dfsync(gdp)
    if (ierror /= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error= .true.
    endif


    contains
    !
    !======================================================================
    subroutine wrtmap_nm(ierr, var, varnam_in)
       integer                                                            :: ierr
       real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) :: var
       character(*)                                                       :: varnam_in
       ! local
       integer       :: namlen
       character(16) :: varnam
       ! body
       namlen = min (16,len(varnam_in))
       varnam = varnam_in(1:namlen)
       if (parll) then
          call dfgather(var,nf,nl,mf,ml,iarrc,gdp)
       else
          call dfgather_seq(var,1-gdp%d%nlb,1-gdp%d%mlb, nmaxgl,mmaxgl)
       endif       
       if (inode == master) then
          ierr = putelt(fds, grnam3, varnam, uindex, 1, glbarr2)
       endif
    end subroutine wrtmap_nm
    !
    !======================================================================
    subroutine wrtmap_nmk(lk, uk, ierr, var, varnam_in, kfmin, kfmax)
       integer                                                                  , intent(in)  :: lk ! lowerbound dim3(0 or 1)
       integer                                                                  , intent(in)  :: uk ! upperbound dim3(kmax or kmax+1)
       integer                                                                                :: ierr
       integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfmin
       integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)       , intent(in)  :: kfmax
       real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lk:uk), intent(in)  :: var
       character(*)                                                             , intent(in)  :: varnam_in
       ! local
       integer       :: namlen
       character(16) :: varnam
       ! body
       namlen = min (16,len(varnam_in))
       varnam = varnam_in(1:namlen)
       if (lk == 0) then
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:kmaxout),stat = istat )
          do k=1,kmaxout
             rbuff3(:,:,k) = var(:,:,smlay(k))
          enddo
          if (zmodel) then
             do m = 1, mmax
                do n = 1, nmaxus
                   do k = 1, kmaxout
                      if (smlay(k)<(kfmin(n,m)-1) .or. smlay(k)>kfmax(n, m))  rbuff3(n, m, k) = -999.0_fp
                   enddo
                enddo
             enddo
          endif
       else
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:kmaxout_restr),stat = istat )
          do k=1,kmaxout_restr
             rbuff3(:,:,k) = var(:,:,smlay_restr(k))
          enddo
          if (zmodel) then
             do m = 1, mmax
                do n = 1, nmaxus
                   do k = 1, kmaxout_restr
                      if (smlay_restr(k)<kfmin(n, m) .or. smlay_restr(k)>kfmax(n, m))  rbuff3(n, m, k) = -999.0_fp
                   enddo
                enddo
             enddo
          endif
       endif
       if (parll) then
          call dfgather(rbuff3,nf,nl,mf,ml,iarrc,gdp)
       else
          call dfgather_seq(rbuff3,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)       
       endif   
       deallocate(rbuff3)
       if (inode == master) then
          ierr = putelt(fds, grnam3, varnam, uindex, 1, glbarr3)
       endif
    end subroutine wrtmap_nmk
    !
    !======================================================================
    subroutine wrtmap_nmkl(lk, uk, ul, ierr, var, varnam_in)
       integer                                                                       :: lk, uk, ul, ierr ! lowerbound dim3(0 or 1), upperbound dim3(kmax or kmax+1), upperbound dim4
       real(fp)     , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lk:uk, ul) :: var
       character(*)                                                                  :: varnam_in
       ! local
       integer       :: namlen
       character(16) :: varnam
       ! body
       namlen = min (16,len(varnam_in))
       varnam = varnam_in(1:namlen)
       if (lk == 0) then
          allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxout,ul ))
          rbuff4(:,:,:,:) = -999.0_fp
          do l = 1, ul
             do k = 1, kmaxout
                do m = 1, mmax
                   do n = 1, nmaxus
                      if (zmodel) then
                         if (smlay(k)<(kfsmin(n,m)-1) .or. smlay(k)>kfsmax(n, m)) then
                            cycle
                         endif
                      endif
                      rbuff4(n,m,k,l) = var(n,m,smlay(k),l)
                   enddo
                enddo
             enddo
          enddo
       else
          allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxout_restr,ul ))
          rbuff4(:,:,:,:) = -999.0_fp
          do l = 1, ul
             do k = 1, kmaxout_restr
                do m = 1, mmax
                   do n = 1, nmaxus
                      if (zmodel) then
                         if (smlay_restr(k)<kfsmin(n, m) .or. smlay_restr(k)>kfsmax(n, m)) then
                            cycle
                         endif
                      endif
                      rbuff4(n,m,k,l) = var(n,m,smlay_restr(k),l)
                   enddo
                enddo
             enddo
          enddo
       endif
       if (parll) then
          call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       else 
          call dfgather_seq(rbuff4,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)   
       endif   
       deallocate(rbuff4)
       if (inode == master) then
          ierr = putelt(fds, grnam3, varnam, uindex, 1, glbarr4)
       endif
    end subroutine wrtmap_nmkl
    !
    !======================================================================
    subroutine wrtmap_nmklflux(lk, uk, ul, ierr, varptr, varnam_in)
       integer                                   :: lk, uk, ul, ierr ! lowerbound dim3(0 or 1), upperbound dim3(kmax or kmax+1), upperbound dim4
       real(fp) , dimension(:,:,:)     , pointer :: varptr ! pointer to an array that may not be associated
                                                           ! note that n and m are merged into one dimension
       character(*)                              :: varnam_in
       ! local
       integer       :: namlen
       character(16) :: varnam
       ! body
       namlen = min (16,len(varnam_in))
       varnam = varnam_in(1:namlen)
       if (lk == 0) then
          allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxout,ul ))
          rbuff4(:,:,:,:) = -999.0_fp
          if (associated(varptr)) then
             do l = 1, ul
                do k = 1, kmaxout
                   do nm = 1, nmmax
                      call nm_to_n_and_m(nm, n, m, gdp)
                      if (zmodel) then
                         if (smlay(k)<(kfsmin(n,m)-1) .or. smlay(k)>kfsmax(n, m)) then
                            cycle
                         endif
                      endif
                      rbuff4(n,m,k,l) = varptr(nm,smlay(k),l)
                   enddo
                enddo
             enddo
          endif
       else
          allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxout_restr,ul ))
          rbuff4(:,:,:,:) = -999.0_fp
          if (associated(varptr)) then
             do l = 1, ul
                do k = 1, kmaxout_restr
                   do nm = 1, nmmax
                      call nm_to_n_and_m(nm, n, m, gdp)
                      if (zmodel) then
                         if (smlay_restr(k)<kfsmin(n, m) .or. smlay_restr(k)>kfsmax(n, m)) then
                            cycle
                         endif
                      endif
                      rbuff4(n,m,k,l) = varptr(nm,smlay_restr(k),l)
                   enddo
                enddo
             enddo
          endif
       endif
       if (parll) then
          call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       else 
          call dfgather_seq(rbuff4,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)   
       endif   
       deallocate(rbuff4)
       if (inode == master) then
          ierr = putelt(fds, grnam3, varnam, uindex, 1, glbarr4)
       endif
    end subroutine wrtmap_nmklflux



end subroutine wrtmap
