subroutine wrimap(lundia      ,error     ,trifil    ,selmap    ,simdat    , &
                  & itdate    ,tzone     ,tunit     ,dt        ,mmax      , &
                  & kmax      ,lmax      ,lstsci    ,ltur      ,nmaxus    , &
                  & noroco    ,norow     ,nostat    ,nsrc      ,ntruv     , &
                  & grdang    ,dpsopt    ,sferic    ,lsed      ,lsedtot   , &
                  & zmodel    ,zbot      ,namsrc    ,namcon    ,namsed    , &
                  & kcu       ,kcv       ,kcs       ,irocol    ,ibuff     , &
                  & xcor      ,ycor      ,xz        ,yz        ,alfas     , &
                  & dp        ,thick     ,zk        ,rbuff     ,rbuff1    , &
                  & dps       ,dpu       ,dpv       ,gsqs      ,gdp       )
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
!  $Id: wrimap.f90 1817 2012-09-04 14:55:36Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/flow2d3d/packages/io/src/output/wrimap.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the initial group 2 ('map-const') to
!              MAP-DAT
!              Selection is done using SELMAP. For elements like
!              NAMCON where LMAX must be > 0 this coupling between
!              LMAX and SELMAP is done in subroutine RDPRFL
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use buffer
    use dfparall
    use globaldata
    use dffunctionals
    !
    implicit none
    !
    type(globdat), target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    type (nefiselement)             , pointer :: nefiselem
    integer       , dimension(:, :) , pointer :: mnit
    integer       , dimension(:, :) , pointer :: mnstat
    integer                         , pointer :: celidt
    integer                         , pointer :: mfg
    integer                         , pointer :: mlg
    integer                         , pointer :: nfg
    integer                         , pointer :: nlg
    integer                         , pointer :: nmaxgl
    integer                         , pointer :: mmaxgl
    integer       , dimension(:)    , pointer :: order_sta
    integer       , dimension(:)    , pointer :: order_tra
    integer       , dimension(:)    , pointer :: smlay
    logical                         , pointer :: first
    character(20) , dimension(:)    , pointer :: namst
    character(20) , dimension(:)    , pointer :: namtra
!
! Global variables
!
    integer                                                         , intent(in)  :: itdate  !  Description and declaration in exttim.igs
    integer                                                                       :: kmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: lmax    !  Description and declaration in dimens.igs
    integer                                                         , intent(in)  :: lsed    !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: lsedtot !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: lstsci  !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: ltur    !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: lundia  !  Description and declaration in inout.igs
    integer                                                                       :: mmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: nmax    !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: nmaxus  !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: noroco  !  Description and declaration in esm_alloc_int.f90
    integer                                                         , intent(in)  :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: nostat  !  Description and declaration in dimens.igs
    integer                                                                       :: nsrc    !  Description and declaration in esm_alloc_int.f90
    integer                                                                       :: ntruv   !  Description and declaration in dimens.igs
    integer , dimension(5, noroco)                                                :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: kcs     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: kcu     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: kcv     !  Description and declaration in esm_alloc_int.f90
    integer , dimension(nmaxus, mmax)                                             :: ibuff   !  Description and declaration in esm_alloc_int.f90
    logical                                                         , intent(out) :: error   !!  Flag=TRUE if an error is encountered
    logical                                                         , intent(in)  :: sferic  !  Description and declaration in tricom.igs
    logical                                                         , intent(in)  :: zmodel  !  Description and declaration in procs.igs
    real(fp)                                                        , intent(in)  :: dt      !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                        , intent(in)  :: grdang  !  Description and declaration in tricom.igs
    real(fp)                                                        , intent(in)  :: tunit   !  Description and declaration in exttim.igs
    real(fp)                                                        , intent(in)  :: tzone   !  Description and declaration in exttim.igs
    real(fp)                                                        , intent(in)  :: zbot    !  Description and declaration in zmodel.igs
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: alfas   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: dp      !  Description and declaration in esm_alloc_real.f90
    real(prec), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: dps     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: dpu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: dpv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: gsqs    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: xcor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: xz      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: ycor    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)   , intent(in)  :: yz      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax + 1)                                   , intent(out) :: rbuff1  
    real(fp), dimension(kmax)                                                     :: thick   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(0:kmax)                                     , intent(in)  :: zk      !!  Vertical coordinates of cell interfaces
                                                                                             !!  Flag for activation of Z-MODEL
    real(fp), dimension(nmaxus, mmax)                                             :: rbuff   !  Description and declaration in r-i-ch.igs
    character(*)                                                    , intent(in)  :: trifil  !!  File name for FLOW NEFIS output
                                                                                             !!  files (tri"h/m"-"casl""labl".dat/def)
    character(16)                                                   , intent(in)  :: simdat  !!  Simulation date representing the
                                                                                             !!  flow condition at this date
    character(20), dimension(lmax)                                  , intent(in)  :: namcon  !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(lsedtot)                               , intent(in)  :: namsed  !  Description and declaration in esm_alloc_char.f90
    character(20), dimension(nsrc)                                                :: namsrc  !  Description and declaration in esm_alloc_char.f90
    character(21)                                                   , intent(in)  :: selmap  !  Description and declaration in tricom.igs
    character(8)                                                    , intent(in)  :: dpsopt  !  Description and declaration in numeco.igs
!
! Local variables
!
    integer                                      :: fds
    integer                                      :: i         ! WARNING: i and i+1, denoting the parameter to be written, can only be used when inode == master
    integer      , dimension(4,0:nproc-1)        :: iarrc     ! array containing collected grid indices 
    integer      , dimension(1)                  :: idummy    ! Help array to read/write Nefis files
    integer                                      :: ierror    ! Local errorflag for NEFIS files
    integer                                      :: ip        ! node number 
    integer      , dimension(:,:)  , allocatable :: isbuff
    integer      , dimension(2)                  :: ival      ! Local array for writing ITDATE and time (:= 00:00:00)
    integer                                      :: k
    integer                                      :: kmaxout
    integer                                      :: l
    integer                                      :: len       ! length of field of current subdomain
    integer                                      :: lengl     ! length of field containing collected data
    integer                                      :: lenlo     ! length of field containing collected data
    integer                                      :: lhlp      ! Help variable for teller constituents and turbulent quantities 
    integer                                      :: lsedbl    ! Number of bed load fractions: lsedtot-lsed
    integer                                      :: m         ! Help variable 
    integer      , dimension(0:nproc-1)          :: mf        ! first index w.r.t. global grid in x-direction
    integer      , dimension(0:nproc-1)          :: ml        ! last index w.r.t. global grid in x-direction
    integer      , dimension(:,:)  , allocatable :: mnstatgl  ! mn indices per partition (excluding duplicates)
    integer                                      :: n         ! Help variable 
    integer      , dimension(0:nproc-1)          :: nf        ! first index w.r.t. global grid in y-direction
    integer      , dimension(0:nproc-1)          :: nl        ! last index w.r.t. global grid in y-direction
    integer                                      :: nostatgl  ! global number of stations (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                      :: nostatto  ! total number of stations (including "duplicate" stations located in halo regions)
    integer                                      :: ntruvgl   ! global number of tracks (i.e. original number excluding duplicate stations located in the halo regions)
    integer                                      :: ntruvto   ! total number of tracks (including "duplicate" stations located in halo regions)
    integer      , dimension(3,5)                :: uindex
    integer                        , external    :: inqmxi
    integer                        , external    :: neferr
    integer                        , external    :: clsnef
    integer                        , external    :: open_datdef
    integer                        , external    :: putelt
    real(sp)     , dimension(1)                  :: rdummy    ! Help array to read/write Nefis files
    real(sp)     , dimension(:)  , allocatable   :: sbuff1d
    real(sp)     , dimension(:,:), allocatable   :: sbuff2d
    character(8) , dimension(1)                  :: cdum8     ! Help array to read/write Nefis files 
    character(16), dimension(1)                  :: cdum16    ! Help array to read/write Nefis files
    character(21), dimension(1)                  :: cdum21    ! Help array to read/write Nefis files
    character(20), dimension(:)    , allocatable :: csbuff2   ! work array for gathering names of stations (exc. duplicates)
    character(256)                               :: errmsg    ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    character(16)                                :: grnam2    ! Data-group name defined for the NEFIS-files
    character(256)                               :: filnam    ! Help var. for FLOW file name
    character(20), dimension(:)    , allocatable :: namhlp    ! Help array for name constituents and turbulent quantities
    character(10)                                :: coordunit ! Unit of X/Y coordinate: M or DEG
!
! Data statements
!
    data grnam2/'map-const'/
!
!! executable statements -------------------------------------------------------
!
    nefiselem  => gdp%nefisio%nefiselem(nefiswrimap)
    mnit       => gdp%gdstations%mnit
    mnstat     => gdp%gdstations%mnstat
    celidt     => nefiselem%celidt
    order_sta  => gdp%gdparall%order_sta
    order_tra  => gdp%gdparall%order_tra
    smlay      => gdp%gdpostpr%smlay
    first      => nefiselem%first
    namst      => gdp%gdstations%namst
    namtra     => gdp%gdstations%namtra
    !
    ! LSTSCI var. name in MAP FILE must remain LSTCI for GPP to work
    ! properly
    !
    !
    ! Initialize local variables
    !
    kmaxout = size(smlay)
    error   = .false.
    filnam  = trifil(1:3) // 'm' // trifil(5:)
    errmsg  = ' '
    lsedbl  = lsedtot - lsed
    !
    call dfsync(gdp)
    mfg    => gdp%gdparall%mfg
    mlg    => gdp%gdparall%mlg
    nfg    => gdp%gdparall%nfg
    nlg    => gdp%gdparall%nlg
    mmaxgl => gdp%gdparall%mmaxgl
    nmaxgl => gdp%gdparall%nmaxgl
    !
    ! initialize group index time dependent data
    !
    uindex (1,1) = 1 ! start index
    uindex (2,1) = 1 ! end index
    uindex (3,1) = 1 ! increment in time
    !
    call init_buffer()
    !
    ! Define coordinate unit
    !
    if (sferic) then
       coordunit = '[  DEG  ]'
    else
       coordunit = '[   M   ]'
    endif
    
    if (parll) then
       !
       ! Recalculates the effective number of stations, filtering out duplicates affected to more
       ! than one partition (i.e. located in halos)
       !
       call dffind_duplicate(lundia, nostat, nostatto, nostatgl, order_sta, gdp)
       !
       ! Recalculates the effective global number of cross sections
       !
       call dffind_duplicate(lundia, ntruv, ntruvto, ntruvgl, order_tra, gdp)
       !    
    else
       nostatto = nostat
       nostatgl = nostat
       ntruvto = ntruv
       ntruvgl = ntruv
    endif
    
    if (first .and. inode /= master) first = .false. ! or master to broadcast after following line
    if (first .and. inode == master) then
       !
       ! map-const
       !
       call addelm(nefiswrimap,'ITDATE',' ','[YYYYMMDD]','INTEGER',4    , &
          & 'Initial date (input) & time (default 00:00:00)                ', &
          & 1         ,2         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'TZONE',' ','[ HOUR  ]','REAL',4    , &
          & 'Local time zone                                               ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'TUNIT',' ','[   S   ]','REAL',4    , &
          & 'Time scale related to seconds                                 ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'DT',' ','[   -   ]','REAL',4    , &
          & 'Time step (DT*TUNIT sec)                                      ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'SIMDAT',' ','[   -   ]','CHARACTER',16   , &
          & 'Simulation date and time [YYYYMMDD  HHMMSS]                   ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'SELMAP',' ','[   -   ]','CHARACTER',21   , &
          & 'Selection flag for field values (2dH, 1dV & 2dV)              ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'NMAX',' ','[   -   ]','INTEGER',4   , &
          & 'Number of N-grid points                                       ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'MMAX',' ','[   -   ]','INTEGER',4   , &
          & 'Number of M-grid points                                       ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'KMAX',' ','[   -   ]','INTEGER',4   , &
          & 'Number of layers                                              ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'LSTCI',' ','[   -   ]','INTEGER',4   , &
          & 'Number of constituents                                        ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'LTUR',' ','[   -   ]','INTEGER',4   , &
          & 'Number of turbulence quantities                              ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'NOSTAT',' ','[   -   ]','INTEGER',4   , &
          & 'Number of monitoring stations                                 ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'NSRC',' ','[   -   ]','INTEGER',4   , &
          & 'Number of discharge                                           ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'NTRUV',' ','[   -   ]','INTEGER',4   , &
          & 'Number of monitoring cross-sections                           ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'GRDANG',' ','[  DEG  ]','REAL',4   , &
          & 'Edge between y-axis and real north                            ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'XCOR',' ',coordunit,'REAL',4   , &
          & 'X-coordinate of grid points                                   ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'YCOR',' ',coordunit,'REAL',4   , &
          & 'Y-coordinate of grid points                                   ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'XZ',' ',coordunit,'REAL',4   , &
          & 'X-coordinate of cell centres                                  ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'YZ',' ',coordunit,'REAL',4   , &
          & 'Y-coordinate of cell centres                                  ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'ALFAS',' ','[  DEG  ]','REAL',4   , &
          & 'Orientation ksi-axis w.r.t. pos.x-axis at water level point   ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'KCU',' ','[   -   ]','INTEGER',4   , &
          & 'Mask array for U-velocity points                             ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'KCV',' ','[   -   ]','INTEGER',4   , &
          & 'Mask array for V-velocity points                              ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'KCS',' ','[   -   ]','INTEGER',4   , &
          & 'Non-active/active water-level point                           ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'DP0',' ','[   M   ]','REAL',4   , &
          & 'Initial bottom depth (positive down)                          ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'DPS0',' ','[   M   ]','REAL',4   , &
          & 'Initial bottom depth at zeta points (positive down)           ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'DPU0',' ','[   M   ]','REAL',4   , &
          & 'Initial bottom depth at u points (positive down)              ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'DPV0',' ','[   M   ]','REAL',4   , &
          & 'Initial bottom depth at v points (positive down)              ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'DRYFLP',' ','[   -   ]','CHARACTER',8   , &
          & 'Criterium to calculate depth in zeta points                   ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'NOROW',' ','[   -   ]','INTEGER',4   , &
          & 'Number of rows for IROCOL table                               ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'NOROCO',' ','[   -   ]','INTEGER',4   , &
          & 'Number of rows & columns of IROCOL table                      ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'IROCOL',' ','[   -   ]','INTEGER',4   , &
          & 'Administration of zeta points                                ', &
          & 2         ,5         ,noroco    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'THICK',' ','[ .01*% ]','REAL',4   , &
          & 'Fraction part of layer thickness of total water-height        ', &
          & 1         ,kmax      ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       lhlp = 0
       if (index(selmap(6:13), 'Y')/=0) lhlp = lhlp + lstsci
       if (index(selmap(14:15), 'Y')/=0) lhlp = lhlp + ltur
       lhlp = max(1, lhlp)
       call addelm(nefiswrimap,'NAMCON',' ','[   -   ]','CHARACTER',20  , &
          & 'Name of constituent & turbulent quantity                      ', &
          & 1         ,lhlp      ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       if (nostatgl>0) then
          call addelm(nefiswrimap,'MNSTAT',' ','[   -   ]','INTEGER',4   , &
             & '(M,N) indices of monitoring stations                          ', &
             & 2         ,2         ,nostatgl  ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrimap,'NAMST',' ','[   -   ]','CHARACTER',20  , &
             & 'Name of monitoring station                                    ', &
             & 1         ,nostatgl  ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (nsrc>0) then
          call addelm(nefiswrimap,'NAMSRC',' ','[   -   ]','CHARACTER',20  , &
             & 'Name of discharge source                                      ', &
             & 1         ,nsrc      ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (ntruvgl>0) then
          call addelm(nefiswrimap,'MNTRA',' ','[   -   ]','INTEGER',4   , &
             & '(M1,N1)-(M2,N2) indices of monitoring cross-sections          ', &
             & 2         ,4         ,ntruvgl   ,0         ,0         ,0      , &
             & lundia    ,gdp       )
          call addelm(nefiswrimap,'NAMTRA',' ','[   -   ]','CHARACTER',20  , &
             & 'Name of monitoring cross-section                              ', &
             & 1         ,ntruvgl   ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (lsed>0) then
          call addelm(nefiswrimap,'LSED',' ','[   -   ]','INTEGER',4  , &
             & 'Number of sediment constituents                               ', &
             & 1         ,1         ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (lsedbl>0) then
          call addelm(nefiswrimap,'LSEDBL',' ','[   -   ]','INTEGER',4  , &
             & 'Number of bedload sediment fractions                          ', &
             & 1         ,1         ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (lsedtot>0) then
          call addelm(nefiswrimap,'NAMSED',' ','[   -   ]','CHARACTER',20 , &
             & 'Name of sediment fraction                                     ', &
             & 1         ,lsedtot   ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       if (zmodel) then
          call addelm(nefiswrimap,'ZK',' ','[   M   ]','REAL',4  , &
             & 'Vertical coordinates of cell interfaces                       ', &
             & 1         ,kmax + 1  ,0         ,0         ,0         ,0      , &
             & lundia    ,gdp       )
       endif
       call addelm(nefiswrimap,'COORDINATES',' ','[   -   ]','CHARACTER',16  , &
          & 'Cartesian or Spherical coordinates                            ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'LAYER_MODEL',' ','[   -   ]','CHARACTER',16  , &
          & 'Sigma-model or Z-model                                        ', &
          & 1         ,1         ,0         ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'GSQS',' ','[  M2   ]','REAL',4   , &
          & 'Horizontal area of computational cell                         ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'PPARTITION',' ','[   -   ]','INTEGER',4   , &
          & 'Partition                                                     ', &
          & 2         ,nmaxgl    ,mmaxgl    ,0         ,0         ,0      , &
          & lundia    ,gdp       )
       call addelm(nefiswrimap,'OUTPUT_LAYERS',' ','[   -   ]','INTEGER',4, &
          & 'User selected output layers                                   ', &
          & 1         ,kmaxout   ,0         ,0         ,0         ,0       , &
          & lundia    ,gdp)
       call defnewgrp(nefiswrimap ,filnam    ,grnam2   ,gdp)
       !
       ! Get start celidt for writing
       !
       nefiselem => gdp%nefisio%nefiselem(nefiswrimap)
       first     => nefiselem%first
       celidt    => nefiselem%celidt
       ierror = open_datdef(filnam   ,fds      )
       if (ierror/= 0) goto 999
       !
       ! end of initialization, don't come here again
       !
       ierror = inqmxi(fds, grnam2, celidt)
       !
       ! group 2, element 'ITDATE'
       !
       ival(1) = itdate
       ival(2) = 000000
       !
       ierror = putelt(fds, grnam2, 'ITDATE', uindex, 1, ival)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'TZONE'
       !
       rdummy(1) = real(tzone,sp)
       ierror = putelt(fds, grnam2, 'TZONE', uindex, 1, rdummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'TUNIT'
       !
       rdummy(1) = real(tunit,sp)
       ierror = putelt(fds, grnam2, 'TUNIT', uindex, 1, rdummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'DT'
       !
       rdummy(1) = real(dt,sp)
       ierror = putelt(fds, grnam2, 'DT', uindex, 1, rdummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'SIMDAT'
       !
       cdum16(1) = simdat
       ierror = putelt(fds, grnam2, 'SIMDAT', uindex, 1, cdum16)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'SELMAP'
       !
       cdum21(1) = selmap
       ierror = putelt(fds, grnam2, 'SELMAP', uindex, 1, cdum21)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NMAX'
       !
       idummy(1) = nmaxgl
       ierror = putelt(fds, grnam2, 'NMAX', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'MMAX'
       !
       idummy(1) = mmaxgl
       ierror = putelt(fds, grnam2, 'MMAX', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'KMAX'
       !
       idummy(1) = kmax
       ierror = putelt(fds, grnam2, 'KMAX', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'LSTCI' Variable is now LSTSCI
       !
       idummy(1) = 0
       if (index(selmap(6:13), 'Y')/=0 .and. lstsci>0) idummy(1) = lstsci
       ierror = putelt(fds, grnam2, 'LSTCI', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'LTUR'
       !
       idummy(1) = 0
       if (index(selmap(14:15), 'Y')/=0 .and. ltur>0) idummy(1) = ltur
       ierror = putelt(fds, grnam2, 'LTUR', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NOSTAT'
       !
       idummy(1) = nostatgl
       ierror = putelt(fds, grnam2, 'NOSTAT', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NSRC'
       !
       idummy(1) = nsrc
       ierror = putelt(fds, grnam2, 'NSRC', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NTRUV'
       !
       idummy(1) = ntruvgl
       ierror = putelt(fds, grnam2, 'NTRUV', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'GRDANG'
       !
       rdummy(1) = real(grdang,sp)
       ierror = putelt(fds, grnam2, 'GRDANG', uindex, 1, rdummy)
       if (ierror/=0) goto 999
       !
       first = .false.
    else
       ierror = 0
    endif ! inode==master
    !
    ! gather grid indices of all subdomains
    !
    if (parll) then
       call dfsync(gdp)
       call dfgather_grddim(lundia, nfg, nlg, mfg, mlg, nmaxgl, mmaxgl, &
       &                 nf, nl, mf, ml, iarrc, lengl, lenlo, gdp )
       call dfbroadc ( iarrc, 4*nproc, dfint, gdp )
       call dfbroadc ( nf, nproc, dfint, gdp )
       call dfbroadc ( nl, nproc, dfint, gdp )
       call dfbroadc ( mf, nproc, dfint, gdp )
       call dfbroadc ( ml, nproc, dfint, gdp )
       call dfsync(gdp)
    endif
    !
    ! group 2, element 'XCOR'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff2d(n, m) = real(xcor(n, m),sp)
       enddo
    enddo
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(sbuff2d,1-gdp%d%nlb,1-gdp%d%mlb, nmaxgl,mmaxgl)
    endif   
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'XCOR', uindex, 1, glbarr2)
    endif
    deallocate( sbuff2d )
    
    if (ierror/=0) goto 999
    !
    ! group 2, element 'YCOR'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff2d(n, m) = real(ycor(n, m),sp)
       enddo
    enddo
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(sbuff2d,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'YCOR', uindex, 1, glbarr2)
    endif
    deallocate( sbuff2d )
           
    if (ierror/=0) goto 999
    !
    ! group 2, element 'XZ'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff2d(n, m) = real(xz(n, m),sp)
       enddo
    enddo
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(sbuff2d,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif
       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'XZ', uindex, 1, glbarr2)
    endif
    deallocate( sbuff2d )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'YZ'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff2d(n, m) = real(yz(n, m),sp)
       enddo
    enddo
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(sbuff2d,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'YZ', uindex, 1, glbarr2)
    endif

    deallocate( sbuff2d )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'ALFAS'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    do m = 1, mmax
       do n = 1, nmaxus
          sbuff2d(n, m) = real(alfas(n, m),sp)
       enddo
    enddo
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(sbuff2d,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'ALFAS', uindex, 1, glbarr2)
    endif

    deallocate( sbuff2d )    
    if (ierror/=0) goto 999
    !
    ! group 2, element 'KCU'
    !
    if (parll) then
       call dfgather(kcu,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(kcu,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'KCU', uindex, 1, glbari2)
    endif

    if (ierror/=0) goto 999
    !
    ! group 2, element 'KCV'
    !
    if (parll) then
       call dfgather(kcv,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(kcv,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'KCV', uindex, 1, glbari2)
    endif
    if (ierror/=0) goto 999
    !
    ! group 2, element 'KCS'
    !
    if (parll) then
       call dfgather(kcs,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(kcs,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'KCS', uindex, 1, glbari2)
    endif
  
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DP0'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    if (dpsopt == 'DP') then
       do n = gdp%d%nlb, gdp%d%nub
          do m = gdp%d%mlb, gdp%d%mub
             sbuff2d(n, m) = real(dps(n, m),sp)
          enddo
       enddo
    else
       do n = gdp%d%nlb, gdp%d%nub
          do m = gdp%d%mlb, gdp%d%mub
             sbuff2d(n, m) = real(dp(n, m),sp)
          enddo
       enddo
    endif
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(sbuff2d,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'DP0', uindex, 1, glbarr2)
    endif
   
    deallocate( sbuff2d )
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DPS0'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    do n = gdp%d%nlb, gdp%d%nub
       do m = gdp%d%mlb, gdp%d%mub
          sbuff2d(n, m) = real(dps(n, m),sp)
       enddo
    enddo
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(sbuff2d,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'DPS0', uindex, 1, glbarr2)
    endif

    deallocate( sbuff2d )       
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DPU0'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    do n = gdp%d%nlb, gdp%d%nub
       do m = gdp%d%mlb, gdp%d%mub
          sbuff2d(n, m) = real(dpu(n, m),sp)
       enddo
    enddo
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(sbuff2d,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'DPU0', uindex, 1, glbarr2)
    endif
  
    deallocate( sbuff2d )    
    if (ierror/=0) goto 999
    !
    ! group 2, element 'DPV0'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    do n = gdp%d%nlb, gdp%d%nub
       do m = gdp%d%mlb, gdp%d%mub
          sbuff2d(n, m) = real(dpv(n, m),sp)
       enddo
    enddo
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
    else
       call dfgather_seq(sbuff2d,1-gdp%d%nlb,1-gdp%d%mlb,nmaxgl,mmaxgl)    
    endif       
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'DPV0', uindex, 1, glbarr2)
    endif

    deallocate( sbuff2d )       
    if (ierror/=0) goto 999
    if (inode == master) then
       !
       ! The necessary information is currently held by DPSOPT but
       ! for backward compatibility the quantity is still called
       ! DRYFLP on the TRIM file.
       !
       ! group 2, element 'DRYFLP'
       !
       cdum8(1) = dpsopt
       ierror = putelt(fds, grnam2, 'DRYFLP', uindex, 1, cdum8)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NOROW'
       !
       idummy(1) = norow
       ierror = putelt(fds, grnam2, 'NOROW', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NOROCO'
       !
       idummy(1) = noroco
       ierror = putelt(fds, grnam2, 'NOROCO', uindex, 1, idummy)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'IROCOL'
       !
       ierror = putelt(fds, grnam2, 'IROCOL', uindex, 1, irocol)
       if (ierror/=0) goto 999
       !
       ! group 2, element 'THICK'
       !
       allocate(sbuff1d(kmax))
       sbuff1d(:) = real(thick(:),sp)
       ierror = putelt(fds, grnam2, 'THICK', uindex, 1, sbuff1d)
       deallocate(sbuff1d)
       if (ierror/=0) goto 999
       !
       ! group 2, only if lmax   > 0  (:= SELMAP( 6:15) <> 'NNNNNNNNNN')
       !
       if (index(selmap(6:15), 'Y')/=0) then
          allocate(namhlp(lstsci+ltur))
          lhlp = 0
          if (index(selmap(6:13), 'Y')>0) then
             do l = 1, lstsci
                namhlp(l) = namcon(l)
             enddo
             lhlp = lhlp + lstsci
          endif
          if (index(selmap(14:15), 'Y')>0) then
             do l = 1, ltur
                namhlp(lhlp + l) = namcon(lstsci + l)
             enddo
          endif
          !
          ! group 2, element 'NAMCON'
          !
          ierror = putelt(fds, grnam2, 'NAMCON', uindex, 1, namhlp)
          if (ierror/=0) goto 999
          deallocate(namhlp)
       endif
    endif ! inode==master
    !
    ! group 2, only if nostat > 0
    !
    if (nostatgl > 0) then
       !
       ! group 2, element 'MNSTAT'
       !
       if (parll) then
          allocate(isbuff(2,nostat))
          do k=1,nostat
             !
             ! mnstat contains indices with respect to this partion
             ! transfer into global indices
             !
             isbuff(1,k) = mnstat(1,k) + mfg - 1
             isbuff(2,k) = mnstat(2,k) + nfg - 1
          enddo
          if (inode == master) allocate(mnstatgl(2,nostatgl))
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, 1, 2, order_sta, isbuff, mnstatgl, gdp)
          deallocate(isbuff)
          if (inode == master) then
             ierror = putelt(fds, grnam2, 'MNSTAT', uindex, 1, mnstatgl)
             deallocate(mnstatgl)
          endif 
       else
          ierror = putelt(fds, grnam2, 'MNSTAT', uindex, 1, mnstat)            
       endif
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NAMST'
       !
       if (parll) then
          if (inode == master) allocate( csbuff2(1:nostatgl) )
          call dfgather_filter(lundia, nostat, nostatto, nostatgl, order_sta, namst, csbuff2, gdp)
          if (inode == master) then
             ierror = putelt(fds, grnam2, 'NAMST', uindex, 1, csbuff2)
             deallocate( csbuff2 )
          endif
       else
          ierror = putelt(fds, grnam2, 'NAMST', uindex, 1, namst)             
       endif   
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, only if nsrc   > 0
    !
    if (inode == master) then
       if (nsrc > 0) then
          !
          ! group 2, element 'NAMSRC'
          !
          ierror = putelt(fds, grnam2, 'NAMSRC', uindex, 1, namsrc)
          if (ierror/=0) goto 999
       endif
    endif
    !
    ! group 2, only if ntruv  > 0
    !
    if (ntruvgl > 0) then
       !
       ! group 2, element 'MNTRA'
       !
       if (parll) then
          allocate(isbuff(4,ntruv))
          isbuff = 0
          do k = 1, ntruv
             !
             ! mnit contains indices with respect to this partion
             ! transfer into global indices
             !
             isbuff(1,k) = mnit(1,k) + mfg - 1
             isbuff(2,k) = mnit(2,k) + nfg - 1
             isbuff(3,k) = mnit(3,k) + mfg - 1
             isbuff(4,k) = mnit(4,k) + nfg - 1
          enddo
          !
          ! Filtering out duplicates from list
          !
          if (inode == master) allocate(mnstatgl(4,ntruvgl))
          call dfgather_filter(lundia, ntruv, ntruvto, ntruvgl, 1, 4, order_tra, isbuff, mnstatgl, gdp)
          if (inode == master) then
             ierror = putelt(fds, grnam2, 'MNTRA', uindex, 1, mnstatgl)
             deallocate(mnstatgl)
          endif
          deallocate(isbuff)
       else
          ierror = putelt(fds, grnam2, 'MNTRA', uindex, 1, mnit)       
       endif   
       if (ierror/=0) goto 999
       !
       ! group 2, element 'NAMTRA'
       !
       if (parll) then
          if (inode == master) allocate(csbuff2(ntruvgl))
          call dfgather_filter(lundia, ntruv, ntruvto, ntruvgl, order_tra, namtra, csbuff2, gdp)
          if (inode == master) then
             ierror = putelt(fds, grnam2, 'NAMTRA', uindex, 1, csbuff2)
             deallocate(csbuff2)
          endif
          if (ierror/=0) goto 999
       else
             ierror = putelt(fds, grnam2, 'NAMTRA', uindex, 1, namtra)          
       endif     
    endif
    !
    ! group 2, element 'LSED'
    !
    if (lsed>0 .and. inode == master) then
       idummy(1) = lsed
       ierror = putelt(fds, grnam2, 'LSED', uindex, 1, idummy)
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'LSEDBL'
    !
    if (lsedbl>0 .and. inode == master) then
       idummy(1) = lsedbl
       ierror = putelt(fds, grnam2, 'LSEDBL', uindex, 1, idummy)
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'NAMSED'
    !
    if (lsedtot>0 .and. inode == master) then
       ierror = putelt(fds, grnam2, 'NAMSED', uindex, 1, namsed)
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'ZK'
    !
    if (zmodel .and. inode == master) then
       allocate(sbuff1d(kmax+1))
       do k = 1, kmax
          sbuff1d(k+1) = real(zk(k),sp)
          rbuff1(k+1) = zk(k)
       enddo
       sbuff1d(1) = real(zbot,sp)
       rbuff1(1) = zbot
       ierror = putelt(fds, grnam2, 'ZK', uindex, 1, sbuff1d)
       deallocate(sbuff1d)
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'COORDINATES'
    !
    if (inode == master) then
       if (sferic) then
          cdum16(1) = 'SPHERICAL'
       else
          cdum16(1) = 'CARTESIAN'
       endif
       ierror = putelt(fds, grnam2, 'COORDINATES', uindex, 1, cdum16)
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'LAYER_MODEL'
    !
    if (inode == master) then
       if (zmodel) then
          cdum16(1) = 'Z-MODEL'
       else
          cdum16(1) = 'SIGMA-MODEL'
       endif
       ierror = putelt(fds, grnam2, 'LAYER_MODEL', uindex, 1, cdum16)
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'GSQS'
    !
    allocate(sbuff2d(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub))
    do n = gdp%d%nlb, gdp%d%nub
       do m = gdp%d%mlb, gdp%d%mub
          sbuff2d(n, m) = real(gsqs(n, m),sp)
       enddo
    enddo
    if (parll) then
       call dfgather(sbuff2d,nf,nl,mf,ml,iarrc,gdp)
       if (inode == master) then
          ierror = putelt(fds, grnam2, 'GSQS', uindex, 1, glbarr2)
       endif
    else
          ierror = putelt(fds, grnam2, 'GSQS', uindex, 1, sbuff2d)    
    endif
    deallocate( sbuff2d )   
    if (ierror/=0) goto 999
    !
    ! Partition
    !
    if (parll .and. inode == master) then
       allocate(isbuff(nmaxgl,mmaxgl))
       do ip = 0, nproc-1
          do n = nf(ip), nl(ip)
             do m = mf(ip), ml(ip)
                isbuff(n, m) = ip
             enddo
          enddo
       enddo
       ierror = putelt(fds, grnam2, 'PPARTITION', uindex, 1, isbuff)
       deallocate( isbuff )
       if (ierror/=0) goto 999
    endif
    !
    ! group 2, element 'OUTPUT_LAYERS'
    !
    if (inode == master) then
       ierror = putelt(fds, grnam2, 'OUTPUT_LAYERS', uindex, 1, smlay)
       if (ierror/=0) goto 999
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
    if (ierror/= 0) then
       ierror = neferr(0, errmsg)
       call prterr(lundia, 'P004', errmsg)
       error = .true.
    endif
end subroutine wrimap
