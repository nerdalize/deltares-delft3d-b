subroutine wrthis(lundia    ,error     ,trifil    ,selhis    ,ithisc    , &
                  & itstrt    ,ithisi    ,zmodel    ,nostat    ,ntruv     , &
                  & kmax      ,lmax      ,lstsci    ,lsal      ,ltem      , &
                  & ltur      ,zkfs      ,zwl       ,zcuru     ,zcurv     , &
                  & zcurw     ,zqxk      ,zqyk      ,ztauks    ,ztauet    , &
                  & zvicww    ,zdicww    ,zrich     ,zrho      ,gro       , &
                  & ztur      ,zvort     ,zenst     ,hydprs    ,fltr      , &
                  & ctr       ,atr       ,dtr       ,velt      ,zdps      , &
                  & sferic    ,gdp       )
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
!  $Id: wrthis.f90 2163 2013-02-01 13:30:53Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrthis.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying groups (1 & 3) to the
!              NEFIS HIS-DAT file
!              Selection is done using SELHIS. For elements like
!              ZCURW where KMAX must be > 1 this coupling between
!              KMAX and SELHIS is done in subroutine RDPRFL
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
    integer, parameter :: station      = 1
    integer, parameter :: crosssection = 2
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: celidt
    integer       , dimension(:, :) , pointer :: elmdms
    integer       , dimension(:, :) , pointer :: mnstat
    integer                         , pointer :: mfg
    integer                         , pointer :: nfg
    integer       , dimension(:)    , pointer :: order_sta
    integer       , dimension(:)    , pointer :: order_tra
    integer       , dimension(:)    , pointer :: line_orig
    integer       , dimension(:)    , pointer :: shlay
    real(fp)      , dimension(:, :) , pointer :: xystat
    logical                         , pointer :: first
    character(20) , dimension(:)    , pointer :: namst
    character*(10)                  , pointer :: trans_unit !  Unit of the variables ATR and DTR
    type (nefiselement)             , pointer :: nefiselem
!
! Global variables
!
    integer                                        , intent(in)  :: ithisc !!  Current time counter for the history data file
    integer                                                      :: ithisi !  Description and declaration in inttim.igs
    integer                                                      :: itstrt !  Description and declaration in inttim.igs
    integer                                                      :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                      :: lmax   !  Description and declaration in dimens.igs
    integer                                                      :: lsal   !  Description and declaration in dimens.igs
    integer                                                      :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                      :: ltem   !  Description and declaration in dimens.igs
    integer                                                      :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                      :: lundia !  Description and declaration in inout.igs
    integer                                                      :: nostat !  Description and declaration in dimens.igs
    integer                                                      :: ntruv  !  Description and declaration in dimens.igs
    integer      , dimension(nostat)                             :: zkfs   !  KFS in monitoring station
    logical                                        , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    logical                                        , intent(in)  :: sferic !  Description and declaration in tricom.igs
    logical                                        , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)     , dimension(nostat)                             :: zdps   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                             :: ztauet !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                             :: ztauks !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat)                             :: zwl    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax)                     :: zdicww !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax)                     :: zrich  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax)                     :: zvicww !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, 0:kmax, ltur)               :: ztur   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: hydprs !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zcuru  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zcurv  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zcurw  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zenst  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zqxk   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zqyk   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zrho   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax)                       :: zvort  !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(nostat, kmax, lstsci)               :: gro    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv)                              :: ctr    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv)                              :: fltr   !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv, lstsci)                      :: atr    !  Description and declaration in esm_alloc_real.f90
    real(fp)     , dimension(ntruv, lstsci)                      :: dtr    !  Description and declaration in esm_alloc_real.f90
    character(*)                                   , intent(in)  :: trifil !!  File name for FLOW NEFIS output files (tri"h/m"-"casl""labl".dat/def)
    character(23)                                  , intent(in)  :: selhis !  Description and declaration in tricom.igs
    character(10)                                  , intent(in)  :: velt   !! Velocity type 'eulerian' or 'GLM'
!
! Local variables
!
    integer                                          :: fds
    integer                                          :: i             ! Help var. 
    integer                                          :: ierror        ! Local errorflag for NEFIS files 
    integer        , dimension(1)                    :: idummy        ! Help array to read/write Nefis files 
    integer                                          :: istat
    integer        , dimension(3,5)                  :: uindex
    integer                            , external    :: getelt
    integer                                          :: kmaxout       ! number of layers to be written to the (history) output files, 0 (possibly) included
    integer                                          :: kmaxout_restr ! number of layers to be written to the (history) output files, 0 excluded
    integer                            , external    :: putelt
    integer                            , external    :: inqmxi
    integer                            , external    :: clsnef
    integer                            , external    :: open_datdef
    integer                            , external    :: neferr
    integer                                          :: n             ! Help var.
    integer                                          :: m             ! Help var.
    integer                                          :: nostatgl      ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                          :: nostatto      ! total number of stations (including "duplicate" stations located in halo regions)
    integer                                          :: ntruvgl       ! global number of tracks (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                          :: ntruvto       ! total number of tracks (including "duplicate" stations located in halo regions)
    integer        , dimension(:)      , allocatable :: ibuff1        ! work array
    integer        , dimension(:,:)    , allocatable :: ibuff2        ! work array
    integer        , dimension(:,:)    , allocatable :: ibuff2b       ! work array
    integer        , dimension(:)      , allocatable :: norig
    integer        , dimension(:)      , allocatable :: nostatarr     ! number of stations per partition
    integer        , dimension(:)      , allocatable :: shlay_restr   ! copy of shlay, excluding layer zero
    logical                                          :: cross_sec     ! option to sum results from cross-sections across partitions
    real(sp)       , dimension(:)      , allocatable :: rsbuff1       ! work array
    real(sp)       , dimension(:,:)    , allocatable :: rsbuff2       ! work array
    real(sp)       , dimension(:,:)    , allocatable :: rsbuff2b      ! work array
    real(sp)       , dimension(:,:,:)  , allocatable :: rsbuff3       ! work array
    real(sp)       , dimension(:,:,:)  , allocatable :: rsbuff3b      ! work array
    character(10)                                    :: coordunit     ! Unit of X/Y coordinate: M or DEG
    character(16)                                    :: grnam1        ! Data-group name defined for the NEFIS-files group 1 
    character(16)                                    :: grnam3        ! Data-group name defined for the NEFIS-files group 3 
    character(256)                                   :: filnam        ! Help var. for FLOW file name 
    character(256)                                   :: errmsg        ! Character var. containing the errormessage to be written to file. The message depends on the error. 
!
! Data statements
!
    data grnam1/'his-info-series'/
    data grnam3/'his-series'/
!
!! executable statements -------------------------------------------------------
!
    nefiselem  => gdp%nefisio%nefiselem(nefiswrthisinf)
    first      => nefiselem%first
    celidt     => nefiselem%celidt
    elmdms     => nefiselem%elmdms
    mnstat     => gdp%gdstations%mnstat
    namst      => gdp%gdstations%namst
    mfg        => gdp%gdparall%mfg
    nfg        => gdp%gdparall%nfg
    order_sta  => gdp%gdparall%order_sta
    order_tra  => gdp%gdparall%order_tra
    line_orig  => gdp%gdstations%line_orig
    shlay      => gdp%gdpostpr%shlay
    xystat     => gdp%gdstations%xystat
    !
    ! Initialize local variables
    !
    kmaxout = size(shlay)
    if (shlay(1) == 0) then
       kmaxout_restr = kmaxout - 1
       allocate(shlay_restr(kmaxout_restr))
       shlay_restr   = shlay(2:)
    else
       kmaxout_restr = kmaxout
       allocate(shlay_restr(kmaxout_restr))
       shlay_restr   = shlay
    endif
    filnam  = trifil(1:3) // 'h' // trifil(5:)
    !
    if (sferic) then
       coordunit = '[  DEG  ]'
    else
       coordunit = '[   M   ]'
    endif
    !
    errmsg = ' '
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time

    if (parll) then 
       !
       ! Recalculates the effective number of stations, filtering out duplicates affected to more
       ! than one partition (i.e. located in halos)
       !
       call dfsync(gdp)
       call dffind_duplicate(lundia, nostat, nostatto, nostatgl, order_sta, gdp)
       !
       ! Recalculates the effective global number of cross sections
       !
       call dfsync(gdp)
       call dffind_duplicate(lundia, ntruv, ntruvto, ntruvgl, order_tra, gdp)
       !
       ! When order_tra points to line_orig, both partition-related and re-ordering-related stuff is taken care of
       !
       order_tra => gdp%gdstations%line_orig
    else
       nostatto = nostat
       nostatgl = nostat
       ntruvto  = ntruv
       ntruvgl  = ntruv
    endif

    !
    ! Set up the element dimensions
    !
    if (first .and. inode == master) then
       !
       ! Set up the element chracteristics
       !
       ! his-info-series
       !
       call addelm(nefiswrthisinf,'ITHISC',' ','[   -   ]','INTEGER',4    , &
          & 'timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)  ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call defnewgrp(nefiswrthisinf ,filnam    ,grnam1   ,gdp)
       !
       ! his-series
       !
       if (nostatgl > 0) then
          if (selhis(1:1)=='Y') then
             call addelm(nefiswrthis,'ZKFS',' ','[   -   ]','INTEGER',4              , &
                & 'Non-active (0) or active (1) zeta point (time-dependent)      ', &
                & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(1:1)=='Y') then
             call addelm(nefiswrthis,'ZWL',' ','[   M   ]','REAL',4              , &
                & 'Water-level in station (zeta point)                           ', &
                & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(2:3), 'Y')>0) then
             call addelm(nefiswrthis,'ZCURU',' ','[  M/S  ]','REAL',4              , &
                & 'U-velocity per layer in station (zeta point, '//velt//')      ', &
                & 2         ,nostatgl  ,kmaxout_restr,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrthis,'ZCURV',' ','[  M/S  ]','REAL',4              , &
                & 'V-velocity per layer in station (zeta point, '//velt//')      ', &
                & 2         ,nostatgl  ,kmaxout_restr,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(4:4)=='Y') then
             call addelm(nefiswrthis,'ZCURW',' ','[  M/S  ]','REAL',4              , &
                & 'W-velocity per layer in station (zeta point)                  ', &
                & 2         ,nostatgl  ,kmaxout_restr,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(20:20)=='Y') then
             call addelm(nefiswrthis,'ZQXK',' ','[  M3/S ]','REAL',4              , &
                & 'U-discharge per layer in station (zeta point)                 ', &
                & 2         ,nostatgl  ,kmaxout_restr,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrthis,'ZQYK',' ','[  M3/S ]','REAL',4              , &
                & 'V-discharge per layer in station (zeta point)                 ', &
                & 2         ,nostatgl  ,kmaxout_restr,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(5:12), 'Y')/=0) then
             call addelm(nefiswrthis,'GRO',' ','[   -   ]','REAL',4              , &
                & 'Concentrations per layer in station (zeta point)              ', &
                & 3         ,nostatgl  ,kmaxout_restr,lstsci    ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(13:14), 'Y')/=0) then
             call addelm(nefiswrthis,'ZTUR',' ','[   -   ]','REAL',4              , &
                & 'Turbulent quantity per layer in station (zeta point)          ', &
                & 3         ,nostatgl  ,kmaxout   ,ltur      ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(15:16), 'Y')>0) then
             call addelm(nefiswrthis,'ZTAUKS',' ','[  N/M2 ]','REAL',4              , &
                & 'Bottom stress U in station (zeta point)                       ', &
                & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
             call addelm(nefiswrthis,'ZTAUET',' ','[  N/M2 ]','REAL',4              , &
                & 'Bottom stress V in station (zeta point)                       ', &
                & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(17:17)=='Y') then
             call addelm(nefiswrthis,'ZVICWW',' ','[  M2/S ]','REAL',4              , &
                & 'Vertical eddy viscosity-3D in station (zeta point)            ', &
                & 2         ,nostatgl  ,kmaxout   ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(18:18)=='Y') then
             call addelm(nefiswrthis,'ZDICWW',' ','[  M2/S ]','REAL',4              , &
                & 'Vertical eddy diffusivity-3D in station (zeta point)          ', &
                & 2         ,nostatgl  ,kmaxout   ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (index(selhis(17:18), 'Y')>0) then
             call addelm(nefiswrthis,'ZRICH',' ','[   -   ]','REAL',4              , &
                & 'Richardson number in station (zeta point)                     ', &
                & 2         ,nostatgl  ,kmaxout   ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(19:19)=='Y') then
             call addelm(nefiswrthis,'ZRHO',' ','[ KG/M3 ]','REAL',4              , &
                & 'Density per layer in station (zeta point)                     ', &
                & 2         ,nostatgl  ,kmaxout_restr,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (zmodel) then
             if (selhis(2:2)=='Y') then
                call addelm(nefiswrthis,'HYDPRES',' ','[  N/M2 ]','REAL',4              , &
                   & 'Non-hydrostatic pressure at station (zeta point)              ', &
                   & 2         ,nostatgl  ,kmaxout_restr,0         ,0         ,0      , &
                   & lundia    ,gdp       )
             endif
          endif
          call addelm(nefiswrthis,'XYSTAT',' ',coordunit,'REAL',4              , &
             & '(X,Y) coordinates of monitoring stations                      ', &
             & 2         ,2         ,nostatgl  ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrthis,'MNSTAT',' ','[   -   ]','INTEGER',4              , &
             & '(M,N) indices of monitoring stations                          ', &
             & 2         ,2         ,nostatgl  ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrthis,'DPS',' ','[   M   ]','REAL',4              , &
             & 'Depth in station                                              ', & ! same as in wrihis
             & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (ntruvgl > 0) then
          if (selhis(20:20)=='Y') then
             call addelm(nefiswrthis,'FLTR',' ','[   M3  ]','REAL',4              , &
                & 'Total discharge through cross section (velocity points)       ', &
                & 1         ,ntruvgl   ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(21:21)=='Y') then
             call addelm(nefiswrthis,'CTR',' ','[  M3/S ]','REAL',4              , &
                & 'Monumentary discharge through cross section (velocity points) ', &
                & 1         ,ntruvgl   ,0         ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(22:22)=='Y') then
             call addelm(nefiswrthis,'ATR',' ','[   -   ]','REAL',4              , &
                & 'Advective transport through cross section (velocity points)   ', &
                & 2         ,ntruvgl   ,lstsci    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
          if (selhis(23:23)=='Y') then
             call addelm(nefiswrthis,'DTR',' ','[   -   ]','REAL',4              , &
                & 'Dispersive transport through cross section (velocity points)  ', &
                & 2         ,ntruvgl   ,lstsci    ,0         ,0         ,0      , &
                & lundia    ,gdp       )
          endif
       endif
       !
       call defnewgrp(nefiswrthis ,filnam    ,grnam3   ,gdp)
       !
       ! Get start celidt for writing
       !
       nefiselem => gdp%nefisio%nefiselem(nefiswrthisinf)
       first     => nefiselem%first
       celidt    => nefiselem%celidt
    endif
    !
    call dfsync(gdp)
    ierror = 0
    if (inode == master) ierror = open_datdef(filnam   ,fds      )
    if (ierror/= 0) goto 999
    if (inode == master) then
       if (first) then
          !
          ! end of initialization, don't come here again
          !
          ierror = inqmxi(fds, grnam1, celidt)
          first = .false.
       endif
       !
       ! Writing of output on every ithisc
       !
       celidt = celidt + 1
       !
       idummy(1) = ithisc
       uindex(1,1) = celidt
       uindex(2,1) = celidt
       !
       ! Group his-series, identified with nefiswrthis, must use the same
       ! value for celidt.
       ! Easy solution:
       gdp%nefisio%nefiselem(nefiswrthis)%celidt = celidt
       ! Neat solution in pseudo code:
       ! subroutine wrthis
       ! integer :: celidt
       ! call wrthisinf(celidt)
       ! call wrthisdat(celidt)
       ! end subroutine
       !
       ierror     = putelt(fds, grnam1, 'ITHISC', uindex, 1, idummy)
    endif
    !
    if (ierror/=0) goto 999
    !
    ! group 3, first 16 depend on NOSTAT > 0
    !
    if (nostatgl > 0) then
       !
       ! group 3: element 'ZKFS' only if SELHIS( 1: 1) = 'Y'
       !
       if (selhis(1:1)=='Y') then
          if (inode == master) then
             allocate( ibuff1(1:nostatgl) )
             ibuff1 = 0
          endif
          if (parll) then    
             call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, zkfs, ibuff1, gdp )
          else
             ibuff1 = zkfs
          endif     
          if (inode == master) then
             ierror = putelt(fds, grnam3, 'ZKFS', uindex, 1, ibuff1)
             deallocate( ibuff1 )
          endif
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZWL' only if SELHIS( 1: 1) = 'Y'
       !
       if (selhis(1:1)=='Y') then
          call wrthis_n(station, nostat, ierror, zwl, 'ZWL')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZCURU' & 'ZCURV'
       ! only if SELHIS( 2: 3) <> 'NN'
       !
       if (index(selhis(2:3), 'Y')>0) then
          call wrthis_nk(station, nostat, 1, kmax, ierror, zcuru, 'ZCURU')
          if (ierror/=0) goto 999
          !
          ! group 3: element 'ZCURV'
          !
          call wrthis_nk(station, nostat, 1, kmax, ierror, zcurv, 'ZCURV')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZCURW', only if KMAX > 1
       ! (:= SELHIS( 4: 4) = 'Y')
       !
       if (selhis(4:4)=='Y') then
          call wrthis_nk(station, nostat, 1, kmax, ierror, zcurw, 'ZCURW')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZQXK' only if SELHIS(20:20) = 'Y'
       !
       if (selhis(20:20)=='Y') then
          call wrthis_nk(station, nostat, 1, kmax, ierror, zqxk, 'ZQXK')
          if (ierror/=0) goto 999
          !
          ! group 3: element 'ZQYK' only if SELHIS(20:20) = 'Y'
          !
          call wrthis_nk(station, nostat, 1, kmax, ierror, zqyk, 'ZQYK')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'GRO', only if LSTSCI > 0
       ! (:= SELHIS( 5:12) <> 'NNNNNNNN')
       !
       if (index(selhis(5:12), 'Y')/=0) then
          call wrthis_nkl(station, nostat, 1, kmax, 1, lstsci, ierror, gro, 'GRO')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZTUR', only if LTUR > 0
       ! (:= SELHIS(13:14) <> 'NN')
       !
       if (index(selhis(13:14), 'Y')/=0) then
          call wrthis_nkl(station, nostat, 0, kmax, 1, ltur, ierror, ztur, 'ZTUR')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZTAUKS' & 'ZTAUET'
       ! only if SELHIS(15:16) <> 'NN'
       !
       if (selhis(15:15)=='Y') then
          call wrthis_n(station, nostat, ierror, ztauks, 'ZTAUKS')
          if (ierror/=0) goto 999
          !
          ! group 3: element 'ZTAUET'
          !
          call wrthis_n(station, nostat, ierror, ztauet, 'ZTAUET')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZVICWW', only if KMAX > 1
       ! (:= SELHIS(17:17) = 'Y')
       !
       if (selhis(17:17)=='Y') then
          call wrthis_nk(station, nostat, 0, kmax, ierror, zvicww, 'ZVICWW')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZDICWW', only if KMAX > 1
       ! (:= SELHIS(18:18) = 'Y')
       !
       if (selhis(18:18)=='Y') then
          call wrthis_nk(station, nostat, 0, kmax, ierror, zdicww, 'ZDICWW')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZRICH', only if KMAX > 1
       ! (:= SELHIS(17:18) <> 'NN')
       !
       if (index(selhis(17:18), 'Y')>0) then
          call wrthis_nk(station, nostat, 0, kmax, ierror, zrich, 'ZRICH')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'ZRHO', only if LSAL > 0 or LTEM > 0
       ! (:= SELHIS(19:19) = 'Y')
       !
       if (selhis(19:19)=='Y') then
          call wrthis_nk(station, nostat, 1, kmax, ierror, zrho, 'ZRHO')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'HYDPRES'
       ! only if selhis( 2: 2) <> 'N'
       !
       if (index(selhis(2:2), 'Y')>0 .and. zmodel) then
          call wrthis_nk(station, nostat, 1, kmax, ierror, hydprs, 'HYDPRES')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 'XYSTAT'
       !
       if (inode == master) allocate( rsbuff2(2,1:nostatgl) )
       if (parll) then
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, 2, order_sta, xystat, rsbuff2, gdp )
       else
          rsbuff2 = real(xystat, sp)
       endif
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'XYSTAT', uindex, 1, rsbuff2)
          deallocate( rsbuff2 )
       endif
       if (ierror/=0) goto 999
       !
       ! group 3: element 'MNSTAT'
       !
       allocate(ibuff2b(2,nostat))
       do i=1,nostat
          !
          ! mnstat contains indices with respect to this partion
          ! transfer into global indices
          !
          ibuff2b(1,i) = mnstat(1,i) + mfg - 1
          ibuff2b(2,i) = mnstat(2,i) + nfg - 1
       enddo
       if (inode == master) allocate( ibuff2(2,nostatgl) )
       if (parll) then
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, 2, order_sta, ibuff2b, ibuff2, gdp)
       else
          ibuff2 = ibuff2b
       endif 
       deallocate(ibuff2b)
       if (inode == master) then
          ierror = putelt(fds, grnam3, 'MNSTAT', uindex, 1, ibuff2)
          deallocate( ibuff2 )
       endif
       if (ierror/=0) goto 999
       !
       ! group 3: element 'DPS'
       !
       call wrthis_n(station, nostat, ierror, zdps, 'DPS')
       if (ierror/=0) goto 999
    endif
    !
    ! group 3: next 4 depend on NTRUV > 0
    !
    if (ntruvgl > 0) then
       cross_sec = .true.
       if (.not.parll) then
          !
          ! Re-arrange the order with the inverse of line_orig
          ! Parallel: order_tra points to line_orig and solves the re-ordering
          !
          allocate (norig(ntruvgl), stat=istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'wrihis: memory alloc error')
             call d3stop(1, gdp)
          endif
          do n = 1, ntruv
              norig( line_orig(n) ) = n
          enddo
       endif
       !
       ! group 3: element 20 'FLTR' only if SELHIS(20:20) = 'Y'
       !
       if (selhis(20:20)=='Y') then
          call wrthis_n(crosssection, ntruv, ierror, fltr, 'FLTR')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 21 'CTR' only if SELHIS(21:21) = 'Y'
       !
       if (selhis(21:21)=='Y') then
          call wrthis_n(crosssection, ntruv, ierror, ctr, 'CTR')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 22 'ATR', only if LSTSCI > 0
       ! (:= SELHIS(22:22) = 'Y')
       !
       if (selhis(22:22)=='Y') then
          call wrthis_nl(crosssection, ntruv, 1, lstsci, ierror, atr, 'ATR')
          if (ierror/=0) goto 999
       endif
       !
       ! group 3: element 23 'DTR', only if LSTSCI > 0
       ! (:= SELHIS(23:23) = 'Y')
       !
       if (selhis(23:23)=='Y') then
          call wrthis_nl(crosssection, ntruv, 1, lstsci, ierror, dtr, 'DTR')
          if (ierror/=0) goto 999
       endif
       deallocate (norig, stat=istat)
    endif
    !
    if (inode == master) ierror = clsnef(fds)
    !
    ! write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
    !
  999 continue
    if (parll .and. inode == master) then
       call dfcleanup_glbarrs
    endif
    call dfsync(gdp)
    if (ierror/=0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
    deallocate(shlay_restr)


    contains
    !
    !======================================================================
    subroutine wrthis_n(typ, ub1, ierr, var, varnam_in)
       integer                                :: typ, ub1, ierr ! obspoint/crosssec, upperbound dim1
       real(fp)     , dimension(ub1)          :: var
       character(*)                           :: varnam_in
       ! local
       integer       :: namlen
       integer       :: i_l
       character(16) :: varnam
       ! body
       namlen = min (16,len(varnam_in))
       varnam = varnam_in(1:namlen)
       if (inode == master) then
          if (typ == station) then
             allocate( rsbuff1(nostatgl) )
          else
             allocate( rsbuff1(ntruvgl) )
          endif
       endif
       if (parll) then
          if (typ == station) then
             call dfgather_filter(lundia, ub1, nostatto, nostatgl, order_sta, var, rsbuff1, gdp)
          else
             !
             ! order_tra re-orders the cross sections
             !
             call dfgather_filter(lundia, ub1, ntruvto, ntruvgl, order_tra, var, rsbuff1, gdp, cross_sec)
          endif
       else
          if (typ == station) then
             rsbuff1 = real(var, sp)
          else
             !
             ! re-order the cross sections using norig
             !
             do i_l=1,ub1
                rsbuff1(i_l) = real(var(norig(i_l)), sp)
             enddo
          endif
       endif  
       if (inode == master) then
          ierr = putelt(fds, grnam3, varnam, uindex, 1, rsbuff1)
          deallocate(rsbuff1)
       endif
    end subroutine wrthis_n
    !
    !======================================================================
    subroutine wrthis_nk(typ, ub1, lb2, ub2, ierr, var, varnam_in)
       integer                                :: typ, ub1, lb2, ub2, ierr ! obspoint/crosssec, upperbound dim1, lowerbound dim2(0 or 1), upperbound dim2(kmax or kmax+1)
       real(fp)     , dimension(ub1, lb2:ub2) :: var
       character(*)                           :: varnam_in
       ! local
       integer       :: namlen
       integer       :: i_l
       character(16) :: varnam
       ! body
       namlen = min (16,len(varnam_in))
       varnam = varnam_in(1:namlen)
       if (inode == master) then
          if (typ == station) then
             allocate( rsbuff2(nostatgl, lb2:ub2) )
          else
             allocate( rsbuff2(ntruvgl, lb2:ub2) )
          endif
       endif
       if (parll) then
          if (typ == station) then
             call dfgather_filter(lundia, ub1, nostatto, nostatgl, lb2, ub2, order_sta, var, rsbuff2, gdp)
          else
             !
             ! order_tra re-orders the cross sections
             !
             call dfgather_filter(lundia, ub1, ntruvto, ntruvgl, lb2, ub2, order_tra, var, rsbuff2, gdp, cross_sec)
          endif
       else
          if (typ == station) then
             rsbuff2 = real(var, sp)
          else
             !
             ! re-order the cross sections using norig
             !
             do i_l=1,ub1
                rsbuff2(i_l,:) = real(var(norig(i_l),:), sp)
             enddo
          endif
       endif  
       if (inode == master) then
          if (kmaxout /= kmax+1) then
             !
             ! Write output for a restricted number of layers
             ! Allocate rsbuff2b with the correct size
             !
             if (typ == station) then
                if (lb2 == 0) then
                   allocate(rsbuff2b(nostatgl, 1:kmaxout))
                else
                   allocate(rsbuff2b(nostatgl, 1:kmaxout_restr))
                endif
             else
                if (lb2 == 0) then
                   allocate(rsbuff2b(ntruvgl, 1:kmaxout))
                else
                   allocate(rsbuff2b(ntruvgl, 1:kmaxout_restr))
                endif
             endif
             !
             ! Fill rsbuff2b
             !
             if (lb2 == 0) then
                do i=1,kmaxout
                   rsbuff2b(:,i) = rsbuff2(:,shlay(i))
                enddo
             else
                do i=1,kmaxout_restr
                   rsbuff2b(:,i) = rsbuff2(:,shlay_restr(i))
                enddo
             endif
             !
             ! And write rsbuff2b
             !
             ierr = putelt(fds, grnam3, varnam, uindex, 1, rsbuff2b)
             deallocate(rsbuff2b)
          else
             !
             ! Write output for all layers (rsbuff2)
             !
             ierr = putelt(fds, grnam3, varnam, uindex, 1, rsbuff2)
          endif
          deallocate(rsbuff2)
       endif
    end subroutine wrthis_nk
    !
    !======================================================================
    subroutine wrthis_nl(typ, ub1, lb2, ub2, ierr, var, varnam_in)
       integer                                :: typ, ub1, lb2, ub2, ierr ! obspoint/crosssec, upperbound dim1, lowerbound dim2(1), upperbound dim2(lstsci or ltur)
       real(fp)     , dimension(ub1, lb2:ub2) :: var
       character(*)                           :: varnam_in
       ! local
       integer       :: namlen
       integer       :: i_l
       character(16) :: varnam
       ! body
       namlen = min (16,len(varnam_in))
       varnam = varnam_in(1:namlen)
       if (inode == master) then
          if (typ == station) then
             allocate( rsbuff2(nostatgl, lb2:ub2) )
          else
             allocate( rsbuff2(ntruvgl, lb2:ub2) )
          endif
       endif
       if (parll) then
          if (typ == station) then
             call dfgather_filter(lundia, ub1, nostatto, nostatgl, lb2, ub2, order_sta, var, rsbuff2, gdp)
          else
             !
             ! order_tra re-orders the cross sections
             !
             call dfgather_filter(lundia, ub1, ntruvto, ntruvgl, lb2, ub2, order_tra, var, rsbuff2, gdp, cross_sec)
          endif
       else
          if (typ == station) then
             rsbuff2 = real(var, sp)
          else
             !
             ! re-order the cross sections using norig
             !
             do i_l=1,ub1
                rsbuff2(i_l,:) = real(var(norig(i_l),:), sp)
             enddo
          endif
       endif  
       if (inode == master) then
          ierr = putelt(fds, grnam3, varnam, uindex, 1, rsbuff2)
          deallocate(rsbuff2)
       endif
    end subroutine wrthis_nl
    !
    !======================================================================
    subroutine wrthis_nkl(typ, ub1, lb2, ub2, lb3, ub3, ierr, var, varnam_in)
       integer                                         :: typ, ub1, lb2, ub2, lb3, ub3, ierr ! obspoint/crosssec, upperbound dim1, lowerbound dim2(0 or 1), upperbound dim2(kmax or kmax+1), lowerbound dim3 (1), upperbound dim3 (lstsci or ltur)
       real(fp)     , dimension(ub1, lb2:ub2, lb3:ub3) :: var
       character(*)                                    :: varnam_in
       ! local
       integer       :: namlen
       integer       :: i_l
       character(16) :: varnam
       ! body
       namlen = min (16,len(varnam_in))
       varnam = varnam_in(1:namlen)
       if (inode == master) then
          if (typ == station) then
             allocate( rsbuff3(nostatgl, lb2:ub2, lb3:ub3) )
          else
             allocate( rsbuff3(ntruvgl, lb2:ub2, lb3:ub3) )
          endif
       endif
       if (parll) then
          if (typ == station) then
             call dfgather_filter(lundia, ub1, nostatto, nostatgl, lb2, ub2, lb3, ub3, order_sta, var, rsbuff3, gdp)
          else
             ! The crosssection variant is not available
          endif
       else
          if (typ == station) then
             rsbuff3 = real(var, sp)
          else
             !
             ! re-order the cross sections using norig
             !
             do i_l=1,ub1
                rsbuff3(i_l,:,:) = real(var(norig(i_l),:,:), sp)
             enddo
          endif
       endif  
       if (inode == master) then
          if (kmaxout /= kmax+1) then
             !
             ! Write output for a restricted number of layers
             ! Allocate rsbuff3b with the correct size
             !
             if (typ == station) then
                if (lb2 == 0) then
                   allocate(rsbuff3b(nostatgl, 1:kmaxout, lb3:ub3))
                else
                   allocate(rsbuff3b(nostatgl, 1:kmaxout_restr, lb3:ub3))
                endif
             else
                if (lb2 == 0) then
                   allocate(rsbuff3b(ntruvgl, 1:kmaxout, lb3:ub3))
                else
                   allocate(rsbuff3b(ntruvgl, 1:kmaxout_restr, lb3:ub3))
                endif
             endif
             !
             ! Fill rsbuff3b
             !
             if (lb2 == 0) then
                do i=1,kmaxout
                   rsbuff3b(:,i,:) = rsbuff3(:,shlay(i),:)
                enddo
             else
                do i=1,kmaxout_restr
                   rsbuff3b(:,i,:) = rsbuff3(:,shlay_restr(i),:)
                enddo
             endif
             !
             ! And write rsbuff3b
             !
             ierr = putelt(fds, grnam3, varnam, uindex, 1, rsbuff3b)
             deallocate(rsbuff3b)
          else
             !
             ! Write output for all layers (rsbuff2)
             !
             ierr = putelt(fds, grnam3, varnam, uindex, 1, rsbuff3)
          endif
          deallocate(rsbuff3)
       endif
    end subroutine wrthis_nkl


end subroutine wrthis
