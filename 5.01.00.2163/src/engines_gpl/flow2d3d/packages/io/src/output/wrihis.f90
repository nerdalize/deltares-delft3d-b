subroutine wrihis(lundia    ,error     ,trifil    ,selhis    ,simdat    , &
                & itdate    ,tzone     ,tunit     ,dt        ,nostat    , &
                & ntruv     ,nmax      ,mmax      ,kmax      ,lmax      , &
                & lstsci    ,ltur      ,grdang    ,sferic    ,lsed      , &
                & lsedtot   ,zbot      ,zmodel    ,namcon    ,namsed    , &
                & xz        ,yz        ,alfas     ,dps       ,thick     , &
                & zk        ,rbuff     ,rbuffc    ,rbuffz    ,gdp       )
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
!  $Id: wrihis.f90 2163 2013-02-01 13:30:53Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrihis.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the initial group 2 ('his-const') to
!              HIS-DAT
!              Selection is done using SELHIS. For elements like
!              NAMCON where LMAX must be > 0 this coupling between
!              LMAX and SELHIS is done in subroutine RDPRFL
!              This routine works for both sequential and parallel computations.
!              Note that, for a sequential computation, (inode ==MASTER) = TRUE.  

! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sp_buffer
    use globaldata
    use dfparall
    !
    use dffunctionals
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type (nefiselement)             , pointer :: nefiselem
    integer                         , pointer :: mfg
    integer                         , pointer :: nfg
    integer       , dimension(:, :) , pointer :: mnit
    integer       , dimension(:, :) , pointer :: mnstat
    integer                         , pointer :: celidt
    integer       , dimension(:, :) , pointer :: elmdms
    integer       , dimension(:)    , pointer :: order_sta
    integer       , dimension(:)    , pointer :: order_tra
    integer       , dimension(:)    , pointer :: line_orig
    integer       , dimension(:)    , pointer :: shlay
    real(fp)      , dimension(:, :) , pointer :: xystat
    logical                         , pointer :: first
    character(20) , dimension(:)    , pointer :: namst
    character(20) , dimension(:)    , pointer :: namtra
!
! Local parameters
!
    integer, parameter :: nelmx = 29
!
! Global variables
!
    integer                                                             , intent(in)  :: itdate !  Description and declaration in exttim.igs
    integer                                                                           :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: lmax   !  Description and declaration in dimens.igs
    integer                                                             , intent(in)  :: lsed   !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: lsedtot!  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                                             , intent(in)  :: ltur   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    integer                                                                           :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                           :: nostat !  Description and declaration in dimens.igs
    integer                                                                           :: ntruv  !  Description and declaration in dimens.igs
    logical                                                             , intent(out) :: error  !  Flag=TRUE if an error is encountered
    logical                                                             , intent(in)  :: sferic !  Description and declaration in tricom.igs
    logical                                                             , intent(in)  :: zmodel !  Description and declaration in procs.igs
    real(fp)                                                            , intent(in)  :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                            , intent(in)  :: grdang !  Description and declaration in tricom.igs
    real(fp)                                                            , intent(in)  :: tunit  !  Description and declaration in exttim.igs
    real(fp)                                                            , intent(in)  :: tzone  !  Description and declaration in exttim.igs
    real(fp)                                                            , intent(in)  :: zbot   !  Description and declaration in zmodel.igs
    real(fp)      , dimension(4, ntruv)                                               :: rbuffc !!  Help arrays for writing NEFIS files
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: alfas  !  Description and declaration in esm_alloc_real.f90
    real(prec)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: dps    !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: xz     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: yz     !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(kmax + 1)                                               :: rbuffz
    real(fp)      , dimension(kmax)                                                   :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(0:kmax)                                   , intent(in)  :: zk     !  Vertical coordinates of cell interfaces
                                                                                                !  Flag for activation of Z-MODEL
    real(fp)      , dimension(nostat)                                                 :: rbuff  !  Description and declaration in r-i-ch.igs
    character(*)                                                        , intent(in)  :: trifil !  File name for FLOW NEFIS output
                                                                                                !  files (tri"h/m"-"casl""labl".dat/def)
    character(16)                                                       , intent(in)  :: simdat !  Simulation date representing the flow condition at this date
    character(20) , dimension(lmax)                                     , intent(in)  :: namcon !  Description and declaration in esm_alloc_char.f90
    character(20) , dimension(lsedtot)                                  , intent(in)  :: namsed !  Description and declaration in esm_alloc_char.f90
    character(23)                                                       , intent(in)  :: selhis !  Description and declaration in tricom.igs
!
! Local variables
!
    integer                                           :: fds
    integer                                           :: ierror      ! Local errorflag for NEFIS files
    integer                                           :: istat
    integer                                           :: k
    integer                                           :: kmaxout
    integer                                           :: l      
    integer                                           :: lhlp        ! Help var. for teller constituents and turbulent quantities 
    integer                                           :: lsedbl      ! Number of bed load fractions: lsedtot-lsed
    integer                                           :: m           ! Help var. 
    integer                                           :: m1     
    integer                                           :: m2     
    integer                                           :: n           ! Help var. 
    integer                                           :: n1     
    integer                                           :: n2     
    integer        , dimension(:,:)     , allocatable :: ibuff       ! Help array for (n,m)-coordinates of cross section locations
    integer        , dimension(1)                     :: idummy      ! Help array to read/write Nefis files 
    integer        , dimension(2)                     :: ival        ! Local array for writing ITDATE and time (:= 00:00:00) 
    integer        , dimension(nelmx)                 :: nbytsg      ! Array containing the number of by- tes of each single ELMTPS 
    integer        , dimension(:)       , allocatable :: norig
    integer        , dimension(3,5)                   :: uindex
    integer                            , external     :: getelt
    integer                            , external     :: putelt
    integer                            , external     :: inqmxi
    integer                            , external     :: clsnef
    integer                            , external     :: open_datdef
    integer                            , external     :: neferr
    logical                                           :: wrswch      ! Flag to write file .TRUE. : write to  file .FALSE.: read from file 
    real(sp)       , dimension(1)                     :: rdummy      ! Help array to read/write Nefis files 
    character(10)  , dimension(nelmx)                 :: elmunt      ! Array with element physical unit 
    character(16)                                     :: grnam2      ! Data-group name defined for the NEFIS-files 
    character(16)  , dimension(1)                     :: cdum16      ! Help array to read/write Nefis files 
    character(16)  , dimension(nelmx)                 :: elmnms      ! Element name defined for the NEFIS-files 
    character(16)  , dimension(nelmx)                 :: elmqty      ! Array with element quantity 
    character(16)  , dimension(nelmx)                 :: elmtps      ! Array containing the types of the elements (real, ch. , etc. etc.) 
    character(20)  , dimension(:)       , allocatable :: namhlp      ! Help array for name constituents and turbulent quantities 
    character(23)  , dimension(1)                     :: cdum23      ! Help array to read/write Nefis files 
    character(256)                                    :: filnam      ! Help var. for FLOW file name 
    character(256)                                    :: errmsg      ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(64)  , dimension(nelmx)                 :: elmdes      ! Array with element description 
    integer                                           :: nostatgl    ! global number of stations (i.e. original number
                                                                     ! excluding duplicate stations located in the halo regions)
    integer                                           :: nostatto    ! total number of stations (including "duplicate" stations located in halo regions)
    integer        , dimension(:)       , allocatable :: nostatarr   ! number of stations per partition
    integer        , dimension(:,:)     , allocatable :: mnstatgl    ! mn indices per partition (excluding duplicates)
    integer                                           :: ntruvgl     ! global number of tracks (i.e. original number
                                                                     ! excluding duplicate stations located in the halo regions)
    integer                                           :: ntruvto     ! total number of tracks (including "duplicate" stations located in halo regions)
    integer        , dimension(:)       , allocatable :: ntruvarr    ! number of tracks per partition
    real(sp)       , dimension(:)       , allocatable :: rsbuff      ! work array for gathering reals (1 dim)
    real(sp)       , dimension(:,:)     , allocatable :: rsbuff2     ! work array for gathering reals (2 dim)
    real(sp)       , dimension(:,:)     , allocatable :: rsbuff2b    ! work array for gathering reals (2 dim)
    character(20)  , dimension(:)       , allocatable :: cbuff1      ! work array for gathering names of stations/cross sections
!
! Data statements
!
    data grnam2/'his-const'/
!
!! executable statements -------------------------------------------------------
!
    nefiselem  => gdp%nefisio%nefiselem(nefiswrihis)
    mfg        => gdp%gdparall%mfg
    nfg        => gdp%gdparall%nfg
    mnit       => gdp%gdstations%mnit
    mnstat     => gdp%gdstations%mnstat
    celidt     => nefiselem%celidt
    elmdms     => nefiselem%elmdms
    order_sta  => gdp%gdparall%order_sta
    order_tra  => gdp%gdparall%order_tra
    shlay      => gdp%gdpostpr%shlay
    xystat     => gdp%gdstations%xystat
    first      => nefiselem%first
    namst      => gdp%gdstations%namst
    namtra     => gdp%gdstations%namtra
    line_orig  => gdp%gdstations%line_orig
    !
    ! LSTSCI var. name in HIS FILE must remain LSTCI for GPP to work
    ! properly
    !
    !
    ! Initialize local variables
    !
    kmaxout = size(shlay)
    ierror  = 0
    celidt  = 1
    lsedbl  = lsedtot - lsed
    !
    filnam  = trifil(1:3) // 'h' // trifil(5:)
    errmsg  = ' '
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    ! Redefine elmunt for sferic coordinates
    !
    if (sferic) then
       elmunt(12) = '[  DEG  ]'
       elmunt(19) = '[  DEG  ]'
    endif
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
       call addelm(nefiswrihis, 'ITDATE', ' ', '[YYYYMMDD]', 'INTEGER', 4, &
          & 'Initial date (input) & time (default 00:00:00)                ', &
          & 1         ,2         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'TZONE', ' ', '[ HOUR  ]', 'REAL', 4, &
          & 'Local time zone                                               ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'TUNIT', ' ', '[   S   ]', 'REAL', 4, &
          & 'Time scale related to seconds                                 ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'DT', ' ', '[   -   ]', 'REAL', 4, &
          & 'Time step (DT*TUNIT sec)                                      ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'SIMDAT', ' ', '[   -   ]', 'CHARACTER', 16, &
          & 'Simulation date and time [YYYYMMDD  HHMMSS]                   ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'SELHIS', ' ', '[   -   ]', 'CHARACTER', 23, &
          & 'Selection flag for time histories                             ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'NOSTAT', ' ', '[   -   ]', 'INTEGER', 4, &
          & 'Number of monitoring stations                                 ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'NTRUV', ' ', '[   -   ]', 'INTEGER', 4, &
          & 'Number of monitoring cross-sections                           ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'LSTCI', ' ', '[   -   ]', 'INTEGER', 4, &
          & 'Number of constituents                                        ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'LTUR', ' ', '[   -   ]', 'INTEGER', 4, &
          & 'Number of turbulence quantities                               ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'KMAX', ' ', '[   -   ]', 'INTEGER', 4, &
          & 'Number of layers                                             ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       if (nostatgl > 0) then
          call addelm(nefiswrihis, 'MNSTAT', ' ', '[   -   ]', 'INTEGER', 4, &
             & '(M,N) indices of monitoring stations                          ', &
             & 2         ,2         ,nostatgl  ,0         ,0         ,0         , lundia, gdp)
          call addelm(nefiswrihis, 'XYSTAT', ' ', '[   M   ]', 'REAL', 4, &
             & '(X,Y) coordinates of monitoring stations                      ', &
             & 2         ,2         ,nostatgl  ,0         ,0         ,0         , lundia, gdp)
          call addelm(nefiswrihis, 'NAMST', ' ', '[   -   ]', 'CHARACTER', 20, &
             & 'Name of monitoring station                                    ', &
             & 1         ,nostatgl  ,0         ,0         ,0         ,0         , lundia, gdp)
       endif
       call addelm(nefiswrihis, 'GRDANG', ' ', '[  DEG  ]', 'REAL', 4, &
          & 'Edge between y-axis and real north                            ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       if (nostatgl > 0) then
          call addelm(nefiswrihis, 'ALFAS', ' ', '[  DEG  ]', 'REAL', 4, &
             & 'Orientation ksi-axis w.r.t. pos.x-axis at water level point   ', &
             & 1         ,nostatgl  ,0         ,0         ,0         ,0         , lundia, gdp)
          call addelm(nefiswrihis, 'DPS', ' ', '[   M   ]', 'REAL', 4, &
             & 'Depth in station                                              ', &
             & 1         ,nostatgl  ,0         ,0         ,0         ,0         , lundia, gdp)
       endif
       call addelm(nefiswrihis, 'THICK', ' ', '[ .01*% ]', 'REAL', 4, &
          & 'Fraction part of layer thickness of total water-height        ', &
          & 1         ,kmax      ,0         ,0         ,0         ,0         , lundia, gdp)
       if (ntruvgl > 0) then
          call addelm(nefiswrihis, 'MNTRA', ' ', '[   -   ]', 'INTEGER', 4, &
             & '(M1,N1)-(M2,N2) indices of monitoring cross-sections          ', &
             & 2         ,4         ,ntruvgl   ,0         ,0         ,0         , lundia, gdp)
          call addelm(nefiswrihis, 'XYTRA', ' ', '[   M   ]', 'REAL', 4, &
             & '(X1,Y1)-(X2,Y2) coordinates of monitoring cross-sections      ', &
             & 2         ,4         ,ntruvgl   ,0         ,0         ,0         , lundia, gdp)
          call addelm(nefiswrihis, 'NAMTRA', ' ', '[   -   ]', 'CHARACTER', 20, &
             & 'Name of monitoring cross-section                             ', &
             & 1         ,ntruvgl   ,0         ,0         ,0         ,0         , lundia, gdp)
       endif
       lhlp = 0
       if (index(selhis(5:12), 'Y')/=0 .or. index(selhis(22:23), 'Y')/=0) &
        & lhlp = lhlp + lstsci
       if (index(selhis(13:14), 'Y')/=0) lhlp = lhlp + ltur
       lhlp = max(1, lhlp)
       call addelm(nefiswrihis, 'NAMCON', ' ', '[   -   ]', 'CHARACTER', 20, &
          & 'Name of constituents / turbulent quantities                   ', &
          & 1         ,lhlp      ,0         ,0         ,0         ,0         , lundia, gdp)
       if (lsed>0) then
          call addelm(nefiswrihis, 'LSED', ' ', '[   -   ]', 'INTEGER', 4, &
             & 'Number of sediment constituents                               ', &
             & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       endif
       if (lsedbl>0) then
          call addelm(nefiswrihis, 'LSEDBL', ' ', '[   -   ]', 'INTEGER', 4, &
             & 'Number of bedload sediment fractions                          ', &
             & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       endif
       if (lsedtot>0) then
          call addelm(nefiswrihis, 'NAMSED', ' ', '[   -   ]', 'CHARACTER', 20, &
             & 'Name of sediment fraction                                     ', &
             & 1         ,lsedtot   ,0         ,0         ,0         ,0         , lundia, gdp)
       endif
       if (zmodel) then
          call addelm(nefiswrihis, 'ZK', ' ', '[   M   ]', 'REAL', 4, &
             & 'Vertical coordinates of cell interfaces                       ', &
             & 1         ,kmax + 1  ,0         ,0         ,0         ,0         , lundia, gdp)
       endif
       call addelm(nefiswrihis, 'COORDINATES', ' ', '[   -   ]', 'CHARACTER', 16, &
          & 'Cartesian or Spherical coordinates                            ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'LAYER_MODEL', ' ', '[   -   ]', 'CHARACTER', 16, &
          & 'Sigma-model or Z-model                                        ', &
          & 1         ,1         ,0         ,0         ,0         ,0         , lundia, gdp)
       call addelm(nefiswrihis, 'OUTPUT_LAYERS', ' ', '[   -   ]', 'INTEGER', 4, &
          & 'User selected output layers                                   ', &
          & 1         ,kmaxout   ,0         ,0         ,0         ,0         , lundia, gdp)
       call defnewgrp(nefiswrihis ,filnam    ,grnam2   ,gdp)
       !
       ! Get start celidt for writing
       !
       nefiselem => gdp%nefisio%nefiselem(nefiswrihis)
       first     => nefiselem%first
       celidt    => nefiselem%celidt
    endif
    ierror = 0
    if (inode == master) ierror = open_datdef(filnam   ,fds      )
    if (ierror/= 0) goto 999
    if (inode == master) then
       if (first) then
          !
          ! end of initialization, don't come here again
          !
          ierror = inqmxi(fds, grnam2, celidt)
          first = .false.
       endif
    endif
    !
    if (inode == master) then
       !
       ! group 2, element 'ITDATE'
       !
       ival(1) = itdate
       ival(2) = 000000
       ierror = putelt(fds, grnam2, 'ITDATE', uindex, 1, ival)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'TZONE'
       !
       rdummy(1) = tzone
       ierror = putelt(fds, grnam2, 'TZONE', uindex, 1, rdummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'TUNIT'
       !
       rdummy(1) = tunit
       ierror = putelt(fds, grnam2, 'TUNIT', uindex, 1, rdummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'DT'
       !
       rdummy(1) = dt
       ierror = putelt(fds, grnam2, 'DT', uindex, 1, rdummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'SIMDAT'
       !
       cdum16(1) = simdat
       ierror = putelt(fds, grnam2, 'SIMDAT', uindex, 1, cdum16)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'SELHIS'
       !
       cdum23(1) = selhis
       ierror = putelt(fds, grnam2, 'SELHIS', uindex, 1, cdum23)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NOSTAT'
       !
       idummy(1) = nostatgl
       ierror = putelt(fds, grnam2, 'NOSTAT', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NTRUV'
       !
       idummy(1) = ntruvgl
       ierror = putelt(fds, grnam2, 'NTRUV', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'LSTCI' Variable is now LSTSCI
       !
       idummy(1) = 0
       if ((index(selhis(5:12), 'Y')/=0 .or. index(selhis(22:23), 'Y')/=0) .and.   &
         & lstsci>0) idummy(1) = lstsci
       ierror = putelt(fds, grnam2, 'LSTCI', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'LTUR'
       !
       idummy(1) = 0
       if (index(selhis(13:14), 'Y')/=0 .and. ltur>0) idummy(1) = ltur
       ierror = putelt(fds, grnam2, 'LTUR', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'KMAX'
       !
       idummy(1) = kmax
       ierror = putelt(fds, grnam2, 'KMAX', uindex, 1, idummy)
       if (ierror/=0) goto 999
    endif ! inode==master
    !
    ! only if nostat > 0 (next 3 elements)
    !
    call dfsync(gdp)
    !
    if (nostatgl > 0) then
       !
       ! group 2, element 'MNSTAT'
       !
       if (inode == master) allocate(mnstatgl(2,nostatgl), stat=istat)
       if (parll) then
          allocate(ibuff(2,nostat), stat=istat)
          do k=1,nostat
             !
             ! mnstat contains indices with respect to this partition
             ! transfer into global indices
             !
             ibuff(1,k) = mnstat(1,k) + mfg - 1
             ibuff(2,k) = mnstat(2,k) + nfg - 1
          enddo
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, 2, order_sta, ibuff, mnstatgl, gdp)
          deallocate(ibuff, stat=istat)
       else
          mnstatgl = mnstat   
       endif 
       if (inode == master) then
          ierror = putelt(fds, grnam2, 'MNSTAT', uindex, 1, mnstatgl)
          deallocate(mnstatgl, stat=istat)
       endif
       if (ierror/=0) goto 999
       !
       ! group 2, element 'XYSTAT'
       !
       allocate(rsbuff2(nostat,2), stat=istat)
       do k = 1, nostat
          m              = mnstat(1,k)
          n              = mnstat(2,k)
          xystat(1,k)    = xz(n,m)
          xystat(2,k)    = yz(n,m)
          rsbuff2(k,1:2) = real((/xz(n,m), yz(n,m)/),sp)
       enddo
       !
       ! Gather location from other nodes (and swap arrays from dimension (nostat,2) to dimension (2,nostat))
       !
       if (inode == master) then
          allocate(rsbuff2b(nostatgl,2), stat=istat)
       endif
       if (parll) then
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, 2, order_sta, rsbuff2, rsbuff2b, gdp)
       else
          if (inode == master) then
             rsbuff2b = rsbuff2   ! not parallel, so it is on the master node
          endif
       endif
       deallocate(rsbuff2, stat=istat)
       if (inode == master) then
          allocate(rsbuff2(2,nostatgl), stat=istat)
          do k=1,nostatgl
             rsbuff2(:,k) = rsbuff2b(k,:)
          enddo
          deallocate(rsbuff2b, stat=istat)
          ierror = putelt(fds, grnam2, 'XYSTAT', uindex, 1, rsbuff2)
          deallocate(rsbuff2, stat=istat)
       endif
       
       if (ierror/=0) goto 999
       !
       ! group 2, element  'NAMST'
       !
       ! Filtering out duplicates from names list
       !
       call dfsync(gdp)
       if (inode == master) allocate(cbuff1(nostatgl), stat=istat)
       if (parll) then 
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, namst, cbuff1, gdp)
       else
          cbuff1 = namst
       endif     
       if (inode == master) then
          ierror = putelt(fds, grnam2, 'NAMST', uindex, 1, cbuff1)
          deallocate(cbuff1, stat=istat)
       endif
       if (ierror/=0) goto 999
    endif  ! nostatgl > 0 
    if (inode == master) then
       !
       ! group 2, element 'GRDANG'
       !
       rdummy(1) = grdang
       ierror = putelt(fds, grnam2, 'GRDANG', uindex, 1, rdummy)
       if (ierror/=0) goto 999
    endif
    !
    ! only if nostat > 0 (next 2 elements)
    !
    if (nostatgl > 0) then
       !
       ! group 2, element 'ALFAS'
       !
       do k = 1, nostat
          m = mnstat(1, k)
          n = mnstat(2, k)
          rbuff(k) = alfas(n, m)
       enddo
       if (inode == master) allocate(rsbuff(nostatgl), stat=istat)
       if (parll) then
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, rbuff, rsbuff, gdp)
       else
         rsbuff = real(rbuff, sp)
       endif
       if (inode == master) then
          ierror = putelt(fds, grnam2, 'ALFAS', uindex, 1, rsbuff)
          deallocate(rsbuff, stat=istat)
       endif
       if (ierror/=0) goto 999
       !
       ! group 2, element 'DPS'
       !
       do k = 1, nostat
          m = mnstat(1, k)
          n = mnstat(2, k)
          rbuff(k) = real(dps(n, m),fp)
       enddo
       if (inode == master) allocate(rsbuff(nostatgl), stat=istat)

       if (parll) then
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, rbuff, rsbuff, gdp)
       else
          rsbuff = rbuff
       endif   
       if (inode == master) then
          ierror = putelt(fds, grnam2, 'DPS', uindex, 1, rsbuff)
          deallocate(rsbuff, stat=istat)
       endif
       if (ierror/=0) goto 999
    endif
    if (inode == master) then
       !
       ! group 2, element 'THICK'
       !
       allocate(rsbuff(kmax), stat=istat)
       rsbuff=real(thick,sp)
       ierror = putelt(fds, grnam2, 'THICK', uindex, 1, rsbuff)
       deallocate(rsbuff, stat=istat)
    endif
    if (ierror/=0) goto 999
    !
    ! only if ntruv  > 0
    ! the next element of group 3 will be written
    !
    if (ntruvgl > 0) then
       if (.not.parll) then
          !
          ! Re-arrange the order with the inverse of line_orig
          ! Parallel: order_tra points to line_orig and solves the re-ordering
          !
          allocate(norig(ntruvgl), stat=istat)
          if (istat /= 0) then
             call prterr(lundia, 'U021', 'wrihis: memory alloc error')
             call d3stop(1, gdp)
          endif
          do n = 1, ntruv
              norig( line_orig(n) ) = n
          enddo
       endif
       !
       ! group 2, element 'MNTRA'
       !
       if (inode == master) then
          allocate(ibuff(4,ntruvgl), stat=istat)
          ibuff = 0
          if (parll) then
             do k = 1, ntruvgl
                !
                ! mnit_global contains the global mn indices of all partitions, before local re-ordering
                ! so don't use gather_filter
                !
                ibuff(:,k) = gdp%gdparall%mnit_global(:,k)
             enddo
          else
             do k = 1, ntruv
                !
                ! mnit is re-ordered; use norig to get the original order
                !
                ibuff(:,k) = mnit(:,norig(k))
             enddo
          endif
             ierror = putelt(fds, grnam2, 'MNTRA', uindex, 1, ibuff)
          deallocate(ibuff, stat=istat)
       endif
       if (ierror/=0) goto 999
       !
       ! group 2, element 'XYTRA'
       !
       ! WARNING
       ! When one of the two points of a cross section is outside this partition, it is not possible
       ! to obtain the correct x and y coordinate. The output values are rubbish
       ! This must be solved in a new subroutine called dfgather_filter_combine, using information
       ! of multiple partitions.
       !
       allocate(rsbuff2(ntruv,4), stat=istat)
       do k = 1, ntruv
          m1           = mnit(1,k)
          n1           = mnit(2,k)
          m2           = mnit(3,k)
          n2           = mnit(4,k)
          rsbuff2(k,1) = real(xz(n1,m1),sp)
          rsbuff2(k,2) = real(yz(n1,m1),sp)
          rsbuff2(k,3) = real(xz(n2,m2),sp)
          rsbuff2(k,4) = real(yz(n2,m2),sp)
       enddo
       if (inode == master) then
          allocate(rsbuff2b(ntruvgl,4), stat=istat)
       endif
       if (parll) then
          !
          ! combine all local values (re-ordered) into global values (original global order)
          !
          call dfgather_filter(lundia, ntruv, ntruvto, ntruvgl, 1, 4, order_tra, rsbuff2, rsbuff2b, gdp)
       else
          !
          ! xytra is re-ordered; use norig to get the original order
          !
          do k = 1, ntruv
             rsbuff2b(k,:) = rsbuff2(norig(k),:)
          enddo
       endif
       deallocate(rsbuff2, stat=istat)
       if (inode == master) then
          ierror = putelt(fds, grnam2, 'XYTRA', uindex, 1, rsbuff2b)
          deallocate(rsbuff2b, stat=istat)
       endif
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NAMTRA'
       !
       if (inode == master) allocate(cbuff1(ntruvgl), stat=istat)
       if (parll) then
          !
          ! combine all local values (re-ordered) into global values (original global order)
          !
          call dfgather_filter(lundia, ntruv, ntruvto, ntruvgl, order_tra, namtra, cbuff1, gdp)
       else
          !
          ! namtra is re-ordered; use norig to get the original order
          !
          do k = 1, ntruv
             cbuff1(k) = namtra(norig(k))
          enddo
       endif
       if (inode == master) then
          ierror = putelt(fds, grnam2, 'NAMTRA', uindex, 1, cbuff1)
          deallocate(cbuff1, stat=istat)
       endif
       deallocate(norig, stat=istat)
       if (ierror/=0) goto 999
    endif  ! (ntruvgl > 0)
    !
    if (inode == master) then
       !
       ! group 2, only if lmax   > 0 (:= selhis( 5:14) <> 'NNNNNNNNNN')
       !
       if (index(selhis(5:14), 'Y')>0 .or. index(selhis(22:23), 'Y')>0) then
          allocate(namhlp(lstsci+ltur), stat=istat)
          lhlp = 0
          if (index(selhis(5:12), 'Y')>0 .or. index(selhis(22:23), 'Y')/=0) then
             do l = 1, lstsci
                namhlp(l) = namcon(l)
             enddo
             lhlp = lhlp + lstsci
          endif
          if (index(selhis(13:14), 'Y')>0) then
             do l = 1, ltur
                namhlp(lhlp + l) = namcon(lstsci + l)
             enddo
          endif
          !
          ! group 2, element 'NAMCON'
          !
          ierror = putelt(fds, grnam2, 'NAMCON', uindex, 1, namhlp)
          if (ierror/=0) goto 999
          deallocate(namhlp, stat=istat)
       endif
       !
       ! group 2, element 'LSED'
       !
       if (lsed>0) then
          idummy(1) = lsed
          ierror = putelt(fds, grnam2, 'LSED', uindex, 1, idummy)
          if (ierror/=0) goto 999
       endif
       !
       ! group 2, element 'LSEDBL'
       !
       if (lsedbl>0) then
          idummy(1) = lsedbl
          ierror = putelt(fds, grnam2, 'LSEDBL', uindex, 1, idummy)
          if (ierror/=0) goto 999
       endif
       !
       ! group 2, element 'NAMSED'
       !
       if (lsedtot>0) then
          ierror = putelt(fds, grnam2, 'NAMSED', uindex, 1, namsed)
          if (ierror/=0) goto 999
       endif
       !
       ! group 2, element 'ZK'
       !
       if (zmodel) then
          allocate(rsbuff(kmax+1), stat=istat)
          do k = 1, kmax
             rsbuff(k + 1) = zk(k)
          enddo
          rsbuff(1) = zbot
          ierror = putelt(fds, grnam2, 'ZK', uindex, 1, rsbuff)
          if (ierror/=0) goto 999
          deallocate(rsbuff, stat=istat)
       endif
       !
       ! group 2, element 'COORDINATES'
       !
       if (sferic) then
          cdum16(1) = 'SPHERICAL'
       else
          cdum16(1) = 'CARTESIAN'
       endif
       ierror = putelt(fds, grnam2, 'COORDINATES', uindex, 1, cdum16)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'LAYER_MODEL'
       !
       if (zmodel) then
          cdum16(1) = 'Z-MODEL'
       else
          cdum16(1) = 'SIGMA-MODEL'
       endif
       ierror = putelt(fds, grnam2, 'LAYER_MODEL', uindex, 1, cdum16)
       if (ierror/=0) goto 999
    endif ! inode==master
    if (inode == master) then
       !
       ! group 2, element 'OUTPUT_LAYERS'
       !
       ierror = putelt(fds, grnam2, 'OUTPUT_LAYERS', uindex, 1, shlay)
    endif
    if (ierror/=0) goto 999
    !
    if (inode == master) ierror = clsnef(fds)
    !
    ! write errormessage if error occurred and set error = .true.
    ! the files will be closed in clsnef (called in triend)
    !
    !
  999 continue
    if (ierror/=0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrihis
