subroutine dfwrsedm(lundia    ,error     ,trifil    ,itmapc    , &
                & mmax      ,kmax      ,nmaxus    ,lsed      ,lsedtot   , &
                & sbuu      ,sbvv      ,ssuu      ,ssvv      ,ws        , &
                & rsedeq    ,dps       ,rca       ,gdp       )
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
!  $Id: dfwrsedm.F90 1983 2012-11-16 14:24:08Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/data/src/parallel_mpi/dfwrsedm.F90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying data for sediment (4 & 5)
!              to the NEFIS MAP-DAT file
!              SOutput is performed conform the times of the map
!              file and only in case lsed > 0.
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    !
    use dfparall
    use globaldata
    use dffunctionals
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                              , pointer :: first
    integer                              , pointer :: celidt
    type (nefiselement)                  , pointer :: nefiselem
    real(hp)                             , pointer :: morft
    real(fp)                             , pointer :: morfac
    real(fp)                             , pointer :: sus
    real(fp)                             , pointer :: bed
    integer                              , pointer :: nxx
    integer  , dimension(:)              , pointer :: smlay
    type (moroutputtype)                 , pointer :: moroutput
    logical                              , pointer :: scour
    real(fp), dimension(:)               , pointer :: xx
    real(fp), dimension(:)               , pointer :: rhosol
    real(fp), dimension(:)               , pointer :: cdryb
    real(fp), dimension(:)               , pointer :: dm
    real(fp), dimension(:)               , pointer :: dg
    real(fp), dimension(:)               , pointer :: dgsd
    real(fp), dimension(:,:)             , pointer :: dxx
    real(fp), dimension(:)               , pointer :: dzduu
    real(fp), dimension(:)               , pointer :: dzdvv
    real(fp), dimension(:,:)             , pointer :: fixfac
    real(fp), dimension(:,:)             , pointer :: frac
    real(fp), dimension(:)               , pointer :: mudfrac
    real(fp), dimension(:)               , pointer :: sandfrac
    real(fp), dimension(:,:)             , pointer :: hidexp
    real(fp), dimension(:,:)             , pointer :: sbcu
    real(fp), dimension(:,:)             , pointer :: sbcv
    real(fp), dimension(:,:)             , pointer :: sbcuu
    real(fp), dimension(:,:)             , pointer :: sbcvv
    real(fp), dimension(:,:)             , pointer :: sbwu
    real(fp), dimension(:,:)             , pointer :: sbwv
    real(fp), dimension(:,:)             , pointer :: sbwuu
    real(fp), dimension(:,:)             , pointer :: sbwvv
    real(fp), dimension(:,:)             , pointer :: sswu
    real(fp), dimension(:,:)             , pointer :: sswv
    real(fp), dimension(:,:)             , pointer :: sswuu
    real(fp), dimension(:,:)             , pointer :: sswvv
    real(fp), dimension(:,:)             , pointer :: sucor
    real(fp), dimension(:,:)             , pointer :: svcor
    real(fp), dimension(:,:)             , pointer :: sinkse
    real(fp), dimension(:,:)             , pointer :: sourse
    real(fp), dimension(:,:)             , pointer :: taurat
    real(fp), dimension(:)               , pointer :: ust2
    real(fp), dimension(:)               , pointer :: umod
    real(fp), dimension(:)               , pointer :: uuu
    real(fp), dimension(:)               , pointer :: vvv
    real(fp), dimension(:)               , pointer :: zumod
    integer                              , pointer :: mfg
    integer                              , pointer :: mlg
    integer                              , pointer :: nfg
    integer                              , pointer :: nlg
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
!
! Global variables
!
    integer                                                                    , intent(in)  :: itmapc !!  Current time counter for the MAP
                                                                                                       !!  data file
    integer                                                                                  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: lsedtot!  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: lundia !  Description and declaration in inout.igs
    integer                                                                                  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    logical                                                                    , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)            , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax, lsed), intent(in)  :: ws     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lsed)  , intent(in)  :: rsedeq !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed)        , intent(in)  :: rca    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbuu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot)     , intent(in)  :: sbvv   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed)        , intent(in)  :: ssuu   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed)        , intent(in)  :: ssvv   !  Description and declaration in esm_alloc_real.f90
    character(60)                                                              , intent(in)  :: trifil !!  File name for FLOW NEFIS output
                                                                                                       !!  files (tri"h/m"-"casl""labl".dat/def)
!
! Local variables
!
    real(fp)                                  :: rhol
    real(fp)                                  :: tauadd
    integer                                   :: ierror    ! Local errorflag for NEFIS files 
    integer                                   :: fds
    integer                                   :: i
    integer                                   :: k          ! Help var. 
    integer                                   :: kmaxout    ! number of layers to be written to the (history) output files
    integer                                   :: l          ! Help var. 
    integer                                   :: m          ! Help var. 
    integer                                   :: n          ! Help var. 
    integer                                   :: nm         ! Help var. 
    integer, dimension(1)                     :: idummy     ! Help array to read/write Nefis files 
    integer, dimension(3,5)                   :: uindex
    integer                        , external :: getelt
    integer                        , external :: putelt
    integer                        , external :: inqmxi
    integer                        , external :: clsnef
    integer                        , external :: open_datdef
    integer                        , external :: neferr
    integer , dimension(4,0:nproc-1)          :: iarrc  ! array containing collected grid indices 
    integer                                   :: lenlo  ! length of field of current subdomain
    integer                                   :: lengl  ! length of field containing collected data
    integer , dimension(0:nproc-1)            :: mf     ! first index w.r.t. global grid in x-direction
    integer , dimension(0:nproc-1)            :: ml     ! last index w.r.t. global grid in x-direction
    integer                                   :: msiz   ! size of present subdomain in x-direction
    integer , dimension(0:nproc-1)            :: nf     ! first index w.r.t. global grid in y-direction
    integer , dimension(0:nproc-1)            :: nl     ! last index w.r.t. global grid in y-direction
    integer                                   :: nsiz   ! size of present subdomain in y-direction
    real(sp)                                  :: sdummy
    real(fp), dimension(:,:,:,:), allocatable :: rbuff4
    character(10)                             :: transpunit
    character(16)                             :: grnam4
    character(16)                             :: grnam5
    character(16)                             :: dxname
    character(256)                            :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(60)                             :: filnam      ! Help var. for FLOW file name 
    character(64)                             :: dxdescr
    character(1024)                           :: error_string
!
! Data statements
!
    data grnam4/'map-infsed-serie'/
    data grnam5/'map-sed-series'/
!
!! executable statements -------------------------------------------------------
!
    nefiselem => gdp%nefisio%nefiselem(nefiswrsedminf)
    first               => nefiselem%first
    celidt              => nefiselem%celidt
    morft               => gdp%gdmorpar%morft
    morfac              => gdp%gdmorpar%morfac
    sus                 => gdp%gdmorpar%sus
    bed                 => gdp%gdmorpar%bed
    nxx                 => gdp%gdmorpar%nxx
    smlay               => gdp%gdpostpr%smlay
    moroutput           => gdp%gdmorpar%moroutput
    xx                  => gdp%gdmorpar%xx
    rhosol              => gdp%gdsedpar%rhosol
    cdryb               => gdp%gdsedpar%cdryb
    scour               => gdp%gdscour%scour
    dm                  => gdp%gderosed%dm
    dg                  => gdp%gderosed%dg
    dgsd                => gdp%gderosed%dgsd
    dxx                 => gdp%gderosed%dxx
    dzduu               => gdp%gderosed%dzduu
    dzdvv               => gdp%gderosed%dzdvv
    fixfac              => gdp%gderosed%fixfac
    frac                => gdp%gderosed%frac
    mudfrac             => gdp%gderosed%mudfrac
    sandfrac            => gdp%gderosed%sandfrac
    hidexp              => gdp%gderosed%hidexp
    sbcu                => gdp%gderosed%sbcu
    sbcv                => gdp%gderosed%sbcv
    sbcuu               => gdp%gderosed%sbcuu
    sbcvv               => gdp%gderosed%sbcvv
    sbwu                => gdp%gderosed%sbwu
    sbwv                => gdp%gderosed%sbwv
    sbwuu               => gdp%gderosed%sbwuu
    sbwvv               => gdp%gderosed%sbwvv
    sswu                => gdp%gderosed%sswu
    sswv                => gdp%gderosed%sswv
    sswuu               => gdp%gderosed%sswuu
    sswvv               => gdp%gderosed%sswvv
    sucor               => gdp%gderosed%sucor
    svcor               => gdp%gderosed%svcor
    sinkse              => gdp%gderosed%sinkse
    sourse              => gdp%gderosed%sourse
    taurat              => gdp%gderosed%taurat
    ust2                => gdp%gderosed%ust2
    umod                => gdp%gderosed%umod
    uuu                 => gdp%gderosed%uuu
    vvv                 => gdp%gderosed%vvv
    zumod               => gdp%gderosed%zumod
    mfg                 => gdp%gdparall%mfg
    mlg                 => gdp%gdparall%mlg
    nfg                 => gdp%gdparall%nfg
    nlg                 => gdp%gdparall%nlg
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    !
    ! Initialize local variables
    !
    kmaxout = size(smlay)
    filnam  = trifil(1:3) // 'm' // trifil(5:)
    errmsg  = ' '
    error   = .false.
    ierror  = 0
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    if (inode == master .and. first) then
       !
       ! Set up the element chracteristics
       !
       select case(moroutput%transptype)
       case (0)
          transpunit = '[ KG/S/M]'
       case (1)
          transpunit = '[ M3/S/M]'
       case (2)
          transpunit = '[ M3/S/M]'
       end select
       !
       ! map-infsed-serie
       !
       call addelm(nefiswrsedminf,'ITMAPS',' ','[   -   ]','INTEGER',4    , &
          & 'timestep number (ITMAPC*DT*TUNIT := time in sec from ITDATE)', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedminf,'MORFAC',' ','[   -   ]','REAL',4       , &
          & 'morphological acceleration factor (MORFAC)                  ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedminf,'MORFT', ' ','[  DAYS ]','REAL',8       , &
          & 'morphological time (days since start of simulation)         ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call defnewgrp(nefiswrsedminf ,filnam    ,grnam4   ,gdp)
       !
       ! map-sed-series
       !
       call addelm(nefiswrsedm,'WS',' ','[  M/S  ]','REAL',4             , &
          & 'Settling velocity per layer'                                , &
          & 4         ,nmaxgl    ,mmaxgl    ,kmaxout   ,lsed      ,0     , &
          & lundia    ,gdp       )
       if (kmax==1) then
          call addelm(nefiswrsedm,'RSEDEQ',' ','[ KG/M3 ]','REAL',4         , &
             & 'Equilibrium concentration of sediment (2D only)'            , &
             & 4         ,nmaxgl    ,mmaxgl    ,kmax      ,lsed      ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%uuuvvv) then
          call addelm(nefiswrsedm,'UUU',' ','[  M/S  ]','REAL',4            , &
             & 'Characteristic velocity u-direction (zeta point)'           , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
          call addelm(nefiswrsedm,'VVV',' ','[  M/S  ]','REAL',4            , &
             & 'Characteristic velocity v-direction (zeta point)'           , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%umod) then
          call addelm(nefiswrsedm,'UMOD',' ','[  M/S  ]','REAL',4           , &
             & 'Characteristic velocity magnitude (zeta point)'             , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%zumod) then
          call addelm(nefiswrsedm,'ZUMOD',' ','[  M/S  ]','REAL',4          , &
             & 'Height above bed for characteristic velocity (zeta point)'  , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%ustar) then
          call addelm(nefiswrsedm,'USTAR',' ','[  M/S  ]','REAL',4          , &
             & 'Bed shear velocity U* (zeta point)'                         , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%sbcuv) then
         call addelm(nefiswrsedm,'SBCU',' ',transpunit ,'REAL',4           , &
            & 'Bed-load transport u-direction due to currents (zeta point)', &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SBCV',' ',transpunit ,'REAL',4           , &
            & 'Bed-load transport v-direction due to currents (zeta point)', &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sbcuuvv) then
         call addelm(nefiswrsedm,'SBCUU',' ',transpunit ,'REAL',4          , &
            & 'Bed-load transport u-direction due to currents (u point)'   , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SBCVV',' ',transpunit ,'REAL',4          , &
            & 'Bed-load transport v-direction due to currents (v point)'   , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sbwuv) then
         call addelm(nefiswrsedm,'SBWU',' ',transpunit ,'REAL',4           , &
            & 'Bed-load transport u-direction due to waves (zeta point)'   , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SBWV',' ',transpunit ,'REAL',4           , &
            & 'Bed-load transport v-direction due to waves (zeta point)'   , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sbwuuvv) then
         call addelm(nefiswrsedm,'SBWUU',' ',transpunit ,'REAL',4          , &
            & 'Bed-load transport u-direction due to waves (u point)'      , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SBWVV',' ',transpunit ,'REAL',4          , &
            & 'Bed-load transport v-direction due to waves (v point)'      , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sswuv) then
         call addelm(nefiswrsedm,'SSWU',' ',transpunit ,'REAL',4           , &
            & 'Suspended transport u-direction due to waves (zeta point)'  , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SSWV',' ',transpunit ,'REAL',4           , &
            & 'Suspended transport v-direction due to waves (zeta point)'  , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sswuuvv) then
         call addelm(nefiswrsedm,'SSWUU',' ',transpunit ,'REAL',4          , &
            & 'Suspended transport u-direction due to waves (u point)'     , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SSWVV',' ',transpunit ,'REAL',4          , &
            & 'Suspended transport v-direction due to waves (v point)'     , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       call addelm(nefiswrsedm,'SBUU',' ',transpunit ,'REAL',4           , &
          & 'Bed-load transport u-direction (u point)'                   , &
          & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedm,'SBVV',' ',transpunit ,'REAL',4           , &
          & 'Bed-load transport v-direction (v point)'                   , &
          & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedm,'SSUU',' ',transpunit ,'REAL',4           , &
          & 'Suspended-load transport u-direction (u point)'             , &
          & 3         ,nmaxgl    ,mmaxgl    ,lsed      ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedm,'SSVV',' ',transpunit ,'REAL',4           , &
          & 'Suspended-load transport v-direction (v point)'             , &
          & 3         ,nmaxgl    ,mmaxgl    ,lsed      ,0         ,0     , &
          & lundia    ,gdp       )
       if (moroutput%suvcor) then
         call addelm(nefiswrsedm,'SUCOR',' ',transpunit ,'REAL',4          , &
            & 'Near-bed transport correction u-direction (u point)'        , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsed      ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SVCOR',' ',transpunit ,'REAL',4          , &
            & 'Near-bed transport correction v-direction (v point)'        , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsed      ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       if (moroutput%sourcesink) then
         call addelm(nefiswrsedm,'SOURSE',' ','[       ]' ,'REAL',4        , &
            & 'Source term suspended sediment fractions'                   , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsed      ,0         ,0     , &
            & lundia    ,gdp       )
         call addelm(nefiswrsedm,'SINKSE',' ','[       ]' ,'REAL',4        , &
            & 'Sink term suspended sediment fractions'                     , &
            & 3         ,nmaxgl    ,mmaxgl    ,lsed      ,0         ,0     , &
            & lundia    ,gdp       )
       endif
       call addelm(nefiswrsedm,'RCA',' ','[ KG/M3 ]','REAL',4            , &
          & 'Near-bed reference concentration of sediment'               , &
          & 3         ,nmaxgl    ,mmaxgl    ,lsed      ,0         ,0     , &
          & lundia    ,gdp       )
       call addelm(nefiswrsedm,'DPS',' ','[   M   ]','REAL',4            , &
          & 'Bottom depth (zeta point)'                                  , &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
          & lundia    ,gdp       )
       if (moroutput%dzduuvv) then
          call addelm(nefiswrsedm,'DZDUU',' ','[   -   ]','REAL',4          , &
             & 'Bed slope in u-direction (u point)'                         , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
          call addelm(nefiswrsedm,'DZDVV',' ','[   -   ]','REAL',4          , &
             & 'Bed slope in v-direction (v point)'                         , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (scour) then
          call addelm(nefiswrsedm,'TAUADD',' ','[   -   ]','REAL',4         , &
             & 'Extra shear stress due to scour feature                    ', &
             & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%taurat) then
          call addelm(nefiswrsedm,'TAURAT',' ','[   -   ]','REAL',4         , &
             & 'Excess bed shear ratio'                                     , &
             & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%dm) then
          call addelm(nefiswrsedm,'DM',' ','[   M   ]','REAL',4             , &
             & 'Arithmetic mean sediment diameter'                          , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%dg) then
          call addelm(nefiswrsedm,'DG',' ','[   M   ]','REAL',4             , &
             & 'Geometric mean sediment diameter'                           , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%dgsd) then
          call addelm(nefiswrsedm,'DGSD',' ','[   -   ]','REAL',4           , &
             & 'Geometric standard deviation of particle size mix'                 , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%percentiles) then
          do l = 1, nxx
             write(dxname,'(A,I2.2)') 'DXX',l
             write(dxdescr,'(A,F4.1,A)') 'Sediment diameter percentile '    , &
                & xx(l)*100.0,' %'
             call addelm(nefiswrsedm,dxname,' ','[   M   ]','REAL',4        , &
                & dxdescr                                                   , &
                & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0  , &
                & lundia    ,gdp       )
          enddo
       endif
       if (moroutput%frac) then
          call addelm(nefiswrsedm,'FRAC',' ','[   -   ]','REAL',4           , &
             & 'Availability fraction in top layer'                         , &
             & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%mudfrac) then
          call addelm(nefiswrsedm,'MUDFRAC',' ','[   -   ]','REAL',4        , &
             & 'Mud fraction in top layer'                                  , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%sandfrac) then
          call addelm(nefiswrsedm,'SANDFRAC',' ','[   -   ]','REAL',4        , &
             & 'Sand fraction in top layer'                                  , &
             & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (moroutput%fixfac) then
          call addelm(nefiswrsedm,'FIXFAC',' ','[   -   ]','REAL',4         , &
             & 'Reduction factor due to limited sediment thickness'         , &
             & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       if (moroutput%hidexp) then
          call addelm(nefiswrsedm,'HIDEXP',' ','[   -   ]','REAL',4         , &
             & 'Hiding and exposure factor'                                 , &
             & 3         ,nmaxgl    ,mmaxgl    ,lsedtot   ,0         ,0     , &
             & lundia    ,gdp       )
       endif
       !
       ! Add mor fields
       !
       call wrmorm(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                 & 1         ,0         ,grnam5    ,gdp       )
       call defnewgrp(nefiswrsedm ,filnam    ,grnam5   ,gdp)
       !
       ! Get start celidt for writing
       !
       nefiselem => gdp%nefisio%nefiselem(nefiswrsedminf)
       first               => nefiselem%first
       celidt              => nefiselem%celidt
    endif
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
    !    
    if (inode == master) then
       ierror = open_datdef(filnam   ,fds      )
    endif
    if (ierror/= 0) goto 9999
    if (inode == master) then
       if (first) then
          !
          ! end of initialization, don't come here again
          !
          ierror = inqmxi(fds, grnam4, celidt)
          first  = .false.
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
       ! celidt is obtained by investigating group map-infsed-serie, identified
       ! with nefiswrsedminf.
       ! Group map-sed-series, identified with nefiswrsedm, must use the same
       ! value for celidt.
       ! Easy solution:
       gdp%nefisio%nefiselem(nefiswrsedm)%celidt = celidt
       ! Neat solution in pseudo code:
       ! subroutine wrsedm
       !    integer :: celidt
       !    call wrsedminfsed(celidt)
       !    call wrsedmsed(celidt)
       ! end subroutine
       !
       ierror     = putelt(fds, grnam4, 'ITMAPS', uindex, 1, idummy)
       if (ierror/=0) goto 9999
       !
       sdummy      = real(morfac,sp)
       ierror     = putelt(fds, grnam4, 'MORFAC', uindex, 1, sdummy)
       if (ierror/=0) goto 9999
       !
       ierror     = putelt(fds, grnam4, 'MORFT',  uindex, 1, morft)
       if (ierror/=0) goto 9999
    endif ! inode==master
    !
    ! group 5: element 'WS'
    !
    allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmaxout, 1:lsed) )
    do k=1,kmaxout
       rbuff4(:, :, k, 1:lsed) = ws(:, :, smlay(k), 1:lsed)
    enddo
    call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
    deallocate(rbuff4)
    if (inode == master) then
       ierror = putelt(fds, grnam5, 'WS', uindex, 1, glbarr4)
    endif
    if (ierror/=0) goto 9999
    !
    if (kmax==1) then
       !
       ! group 5: element 'RSEDEQ'
       ! kmax=1: don't use kmaxout/smlay
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax, lsed) )
       do k=1,kmax
          rbuff4(:, :, k, 1:lsed) = rsedeq(:, :, k, 1:lsed)
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'RSEDEQ', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%uuuvvv) then
       !
       ! group 5: element 'UUU'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       if (associated(uuu)) then
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, 1, 1) = uuu(nm)
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'UUU', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'VVV'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       if (associated(vvv)) then
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, 1, 1) = vvv(nm)
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'VVV', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%umod) then
       !
       ! group 5: element 'UMOD'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       if (associated(umod)) then
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, 1, 1) = umod(nm)
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'UMOD', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%zumod) then
       !
       ! group 5: element 'ZUMOD'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       if (associated(zumod)) then
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, 1, 1) = zumod(nm)
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'ZUMOD', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%ustar) then
       !
       ! group 5: element 'USTAR'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       if (associated(ust2)) then
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, 1, 1) = sqrt(ust2(nm))
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'USTAR', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%sbcuv) then
       !
       ! group 5: element 'SBCU'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sbcu)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sbcu(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SBCU', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'SBCV'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sbcv)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sbcv(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SBCV', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%sbcuuvv) then
       !
       ! group 5: element 'SBCUU'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sbcuu)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sbcuu(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SBCUU', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'SBCVV'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sbcvv)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sbcvv(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SBCVV', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%sbwuv) then
       !
       ! group 5: element 'SBWU'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sbwu)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sbwu(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SBWU', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'SBWV'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sbwv)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sbwv(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SBWV', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%sbwuuvv) then
       !
       ! group 5: element 'SBWUU'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sbwuu)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sbwuu(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SBWUU', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'SBWVV'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sbwvv)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sbwvv(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SBWVV', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%sswuv) then
       !
       ! group 5: element 'SSWU'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sswu)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sswu(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SSWU', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'SSWV'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sswv)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sswv(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SSWV', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%sswuuvv) then
       !
       ! group 5: element 'SSWUU'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sswuu)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sswuu(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SSWUU', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'SSWVV'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(sswvv)) then
          do l = 1, lsedtot
             select case(moroutput%transptype)
             case (0)
                rhol = 1.0_fp
             case (1)
                rhol = cdryb(l)
             case (2)
                rhol = rhosol(l)
             end select
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sswvv(nm,l)/rhol
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SSWVV', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    ! group 5: element 'SBUU'
    !
    allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
    do l = 1, lsedtot
       select case(moroutput%transptype)
       case (0)
          rhol = 1.0_fp
       case (1)
          rhol = cdryb(l)
       case (2)
          rhol = rhosol(l)
       end select
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff4(n, m, l, 1) = sbuu(n, m, l)/rhol
          enddo
       enddo
    enddo
    call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
    deallocate(rbuff4)
    if (inode == master) then
       ierror = putelt(fds, grnam5, 'SBUU', uindex, 1, glbarr4)
    endif
    if (ierror/=0) goto 9999
    !
    ! group 5: element 'SBVV'
    !
    allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
    do l = 1, lsedtot
       select case(moroutput%transptype)
       case (0)
          rhol = 1.0_fp
       case (1)
          rhol = cdryb(l)
       case (2)
          rhol = rhosol(l)
       end select
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff4(n, m, l, 1) = sbvv(n, m, l)/rhol
          enddo
       enddo
    enddo
    call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
    deallocate(rbuff4)
    if (inode == master) then
       ierror = putelt(fds, grnam5, 'SBVV', uindex, 1, glbarr4)
    endif
    if (ierror/=0) goto 9999
    !
    ! group 5: element 'SSUU'
    !
    allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsed, 1) )
    do l = 1, lsed
       select case(moroutput%transptype)
       case (0)
          rhol = 1.0_fp
       case (1)
          rhol = cdryb(l)
       case (2)
          rhol = rhosol(l)
       end select
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff4(n, m, l, 1) = ssuu(n, m, l)/rhol
          enddo
       enddo
    enddo
    call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
    deallocate(rbuff4)
    if (inode == master) then
       ierror = putelt(fds, grnam5, 'SSUU', uindex, 1, glbarr4)
    endif
    if (ierror/=0) goto 9999
    !
    ! group 5: element 'SSVV'
    !
    allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsed, 1) )
    do l = 1, lsed
       select case(moroutput%transptype)
       case (0)
          rhol = 1.0_fp
       case (1)
          rhol = cdryb(l)
       case (2)
          rhol = rhosol(l)
       end select
       do m = 1, mmax
          do n = 1, nmaxus
             rbuff4(n, m, l, 1) = ssvv(n, m, l)/rhol
          enddo
       enddo
    enddo
    call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
    deallocate(rbuff4)
    if (inode == master) then
       ierror = putelt(fds, grnam5, 'SSVV', uindex, 1, glbarr4)
    endif
    if (ierror/=0) goto 9999
    !
    ! group 5: element 'SUCOR'
    !
    if (moroutput%suvcor) then
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsed, 1) )
       do l = 1, lsed
          select case(moroutput%transptype)
          case (0)
             rhol = 1.0_fp
          case (1)
             rhol = cdryb(l)
          case (2)
             rhol = rhosol(l)
          end select
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, l, 1) = sucor(nm, l)/rhol
             enddo
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SUCOR', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'SVCOR'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsed, 1) )
       do l = 1, lsed
          select case(moroutput%transptype)
          case (0)
             rhol = 1.0_fp
          case (1)
             rhol = cdryb(l)
          case (2)
             rhol = rhosol(l)
          end select
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, l, 1) = svcor(nm, l)/rhol
             enddo
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SVCOR', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    ! group 5: element 'SOURSE'
    !
    if (moroutput%sourcesink) then
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsed, 1) )
       if (associated(sourse)) then
          do l = 1, lsed
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sourse(nm, l)
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsed, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SOURSE', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'SINKSE'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsed, 1) )
       if (associated(sinkse)) then
          do l = 1, lsed
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = sinkse(nm, l)
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsed, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SINKSE', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    ! group 5: element 'RCA'
    !
    allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsed, 1) )
    do l = 1, lsed
       do m = 1, mmax
          do n = 1, nmaxus
             !
             ! near-bed reference concentration of sediment
             !
             rbuff4(n, m, l, 1) = rca(n, m, l)
          enddo
       enddo
    enddo
    call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
    deallocate(rbuff4)
    if (inode == master) then
       ierror = putelt(fds, grnam5, 'RCA', uindex, 1, glbarr4)
    endif
    if (ierror/=0) goto 9999
    !
    ! group 5: element 'DPS'
    !
    allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
    i = 0
    do m = 1, mmax
       do n = 1, nmaxus
          i        = i+1
          rbuff4(n, m, 1, 1) = dps(n, m)
       enddo
    enddo
    call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
    deallocate(rbuff4)
    if (inode == master) then
       ierror = putelt(fds, grnam5, 'DPS', uindex, 1, glbarr4)
    endif
    if (ierror/=0) goto 9999
    !
    if (moroutput%dzduuvv) then
       !
       ! group 5: element 'DZDUU'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       if (associated(dzduu)) then
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, 1, 1) = dzduu(nm)
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'DZDUU', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
       !
       ! group 5: element 'DZDVV'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       if (associated(dzdvv)) then
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, 1, 1) = dzduu(nm)
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'DZDVV', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (scour) then
       !
       ! group 5: element 'TAUADD'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       do m = 1, mmax
          do n = 1, nmaxus
             call n_and_m_to_nm(n, m, nm, gdp)
             call shearx(tauadd, nm, gdp)
             rbuff4(n, m, 1, 1) = tauadd
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'TAUADD', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%taurat) then
       !
       ! group 5: element 'TAURAT'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(taurat)) then
          do l = 1, lsedtot
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = taurat(nm, l)
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'TAURAT', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    ! group 5: element 'DM'
    !
    if (moroutput%dm) then
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       do m = 1, mmax
          do n = 1, nmaxus
             call n_and_m_to_nm(n, m, nm, gdp)
             rbuff4(n, m, 1, 1) = dm(nm)
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'DM', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    ! group 5: element 'DG'
    !
    if (moroutput%dg) then
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       do m = 1, mmax
          do n = 1, nmaxus
             call n_and_m_to_nm(n, m, nm, gdp)
             rbuff4(n, m, 1, 1) = dg(nm)
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'DG', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    ! group 5: element 'DGSD'
    !
    if (moroutput%dgsd) then
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       do m = 1, mmax
          do n = 1, nmaxus
             call n_and_m_to_nm(n, m, nm, gdp)
             rbuff4(n, m, 1, 1) = dgsd(nm)
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'DGSD', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%percentiles) then
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       do l = 1, nxx
          write(dxname,'(A,I2.2)') 'DXX',l
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, 1, 1) = dxx(nm, l)
             enddo
          enddo
          call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
          if (inode == master) then
             ierror = putelt(fds, grnam5, dxname, uindex, 1, glbarr4)
          endif
          if (ierror/=0) goto 9999
       enddo
       deallocate(rbuff4)
    endif
    !
    if (moroutput%frac) then
       !
       ! group 5: element 'FRAC'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       do l = 1, lsedtot
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, l, 1) = frac(nm, l)
             enddo
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'FRAC', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%mudfrac) then
       !
       ! group 5: element 'MUDFRAC'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       do m = 1, mmax
          do n = 1, nmaxus
             call n_and_m_to_nm(n, m, nm, gdp)
             rbuff4(n, m, 1, 1) = mudfrac(nm)
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'MUDFRAC', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    
    if (moroutput%sandfrac) then
       !
       ! group 5: element 'SANDFRAC'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1, 1) )
       do m = 1, mmax
          do n = 1, nmaxus
             call n_and_m_to_nm(n, m, nm, gdp)
             rbuff4(n, m, 1, 1) = sandfrac(nm)
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'SANDFRAC', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%fixfac) then
       !
       ! group 5: element 'FIXFAC'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       if (associated(fixfac)) then
          do l = 1, lsedtot
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff4(n, m, l, 1) = fixfac(nm, l)
                enddo
             enddo
          enddo
       else
          rbuff4(1:nmaxus, 1:mmax, 1:lsedtot, 1) = -999.0
       endif
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'FIXFRAC', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    if (moroutput%hidexp) then
       !
       ! group 5: element 'HIDEXP'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 1:lsedtot, 1) )
       do l = 1, lsedtot
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff4(n, m, l, 1) = gdp%gderosed%hidexp(nm, l)
             enddo
          enddo
       enddo
       call dfgather(rbuff4,nf,nl,mf,ml,iarrc,gdp)
       deallocate(rbuff4)
       if (inode == master) then
          ierror = putelt(fds, grnam5, 'HIDEXP', uindex, 1, glbarr4)
       endif
       if (ierror/=0) goto 9999
    endif
    !
    ! Add mor fields
    !
    call wrmorm(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
              & 2         ,fds       ,grnam5    ,gdp       )
    if (error) goto 9999
    !
    if (inode == master) ierror = clsnef(fds)
    !
    ! write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
 9999 continue
    if (inode == master) then
       call dfcleanup_glbarrs
    endif
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine dfwrsedm
