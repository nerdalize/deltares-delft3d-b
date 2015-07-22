module swan_input
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
!  $Id: swan_input.f90 2083 2013-01-02 10:52:35Z ye $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/5.01.00.2163/src/engines_gpl/wave/packages/data/src/swan_input.f90 $
!!--description-----------------------------------------------------------------
!
! WAVE-GUI version number dependencies:
! 4.87.00 (Older than)             : not supported
!
!
!         NOTHING
!         ......
!         NOTHING
!         ......
!         * Level of test output, debug level, Y/N compute waves
!         0 0 1
! 4.88.08 (Newer than or equal to) :
!         * Reflection (0/1), specular or diffuse (1/2), reflection coefficient [0-1]
!         0 1  0.0000000e+000
!         ......
!         * Diffraction, smoothing coefficient, smoothing steps, adaptation of propagation
!         *   - interactions: 0 = de-activated, 1 = activated
!         1  0.2 1 1
!         ......
!         * Level of test output, debug level, Y/N compute waves, Y/N activate hotstart file
!         0 0 1 0
!
!
!         * Number of tidal time points
!         39
! 4.89.05 (Newer than or equal to) :
!         * Number of tidal time points, Reference date
!         39 2006-02-07
!
!
!         * Water level correction
!         0.0000000e+000
! 4.90.00 (Newer than or equal to) :
!         * Water level correction, Extrapolate flow data on the last # grid(s)
!         0.0000000e+000 0
!
!
!         * Y/N Use bathmetry, use waterlevel, use current
!         0 0 0
!         ......
!         * Water level correction, Extend flow data on the last # grid(s),
!         0.0000000e+000 1
!         ......
!         * Level of test output, debug level, Y/N compute waves, Y/N activate hotstart file
!         0 0 1 0
! 4.90.06 (Newer than or equal to) :
!         * Y/N Use bathmetry, use waterlevel, use current, use wind
!         0 0 0 0
!         ......
!         * Water level correction, Extend flow data on the last # grid(s),
!         * Extend bathymetry, water level, current, wind
!         0.0000000e+000 1 1 0 0 0
!         ......
!         * Level of test output, debug level, Y/N compute waves, Y/N activate hotstart file
!         * Output time interval, Computational mode: 0 = stationary, 1 = non-stationary
!         * Non-stationary interval, timestep
!         0 0 1 0  2.2000000e+001 0 6.00000+001 2.00000+000
!
!
!         '40.01'
! 4.91.00 (Newer than or equal to) :
!         NOTHING
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use wave_data
    use handles
    use table_handles
    use utilities
    use rdsec_module
    !
    type swan_dom
       real                                    :: freqmax          ! maximum frequency
       real                                    :: freqmin          ! minimum frequency
       real                                    :: enddir           ! end direction for sector
       real                                    :: startdir         ! start direction for sector
       real                                    :: veg_height       ! vegetation height per layer
       real                                    :: veg_diamtr       ! vegetation diameter
       real                                    :: veg_drag         ! vegetation drag coefficient
       integer                                 :: veg_nstems       ! the number of plant stands per square meter
       integer                                 :: curvibot   
       integer                                 :: dirspace         ! 1: circle, 2: sector
       integer                                 :: ndir             ! number of directional bins
       integer                                 :: nfreq            ! number of frequency bins
       integer                                 :: nestnr
       integer                                 :: n_meteofiles_dom ! number of meteo input files
       integer                                 :: mxb
       integer                                 :: myb
       integer                                 :: mxc
       integer                                 :: myc
       integer                                 :: vegetation
       integer       , dimension(4)            :: qextnd           ! 0: not used, 1: used and not extended, 2: used and extended
       integer                                 :: flowVelocityType = FVT_DEPTH_AVERAGED
                                                                   ! Possible values:
                                                                   !    FVT_SURFACE_LAYER           : use FLOW velocity at surface
                                                                   !    FVT_DEPTH_AVERAGED (default): use depth averaged FLOW velocity
                                                                   !    FVT_WAVE_DEPENDENT          : use FLOW velocity, averaged in a wave dependent way
       logical                                 :: cgnum
       character(256)                          :: botfil
       character(256)                          :: curlif
       character(256)                          :: depfil
       character(256)                          :: nesfil
       character(37)                           :: vegfil
       character(20)                           :: nesnam           ! dummy
       character(80), dimension(:), allocatable :: meteofile_dom
    end type swan_dom
    !
    type swan_bnd
       integer                                 :: parread     ! 1 = from-file, 2 = parametric
       integer                                 :: sshape      ! 1 = Jonswap, 2 = Pierson-Moskowitz, 3 = Gauss
       integer                                 :: periodtype  ! 1 = Peak, 2 = Mean
       integer                                 :: dsprtype    ! 1 = Power, 2 = Degrees
       integer                                 :: bndtyp      ! 1 = orientation, 2 = grid-coordinates, 3 = xy-coordinates
       integer                                 :: orient      ! 1 = N, 2 = NW, 3 = W, 4 = SW, 5 = S, 6 = SE, 7 = E, 8 = NE
       integer                                 :: turn        ! 0 = clockwise, 1 = counterclockwise (distance measurement along boundary)
       integer                                 :: convar      ! 1 = uniform, 2 = space-varying
       integer                                 :: nsect       ! previously swani(iindx+9)
       integer       , dimension(4)            :: bndcrd_mn
       real          , dimension(4)            :: bndcrd_xy
       real                                    :: gamma0
       real                                    :: sigfr
       !
       integer       , dimension(4)            :: ts_hs
       integer       , dimension(4)            :: ts_tp
       integer       , dimension(4)            :: ts_wd
       integer       , dimension(4)            :: ts_ds
       !
       real          , dimension(:), pointer   :: distance
       real          , dimension(:), pointer   :: waveheight
       real          , dimension(:), pointer   :: period
       real          , dimension(:), pointer   :: direction
       real          , dimension(:), pointer   :: dirspread
       character(20)                           :: name
       character(37) , dimension(:), pointer   :: spectrum
    end type swan_bnd
    !
    type swan
       integer                                 :: maxbound
       integer                                 :: maxcurv
       integer                                 :: maxnest
       integer                                 :: maxobst
       integer                                 :: maxpoints
       integer       , dimension(:), pointer   :: maxsect
       integer                                 :: maxsteps
       !
       integer                                 :: diffraction
       integer                                 :: diffr_smsteps
       integer                                 :: diffr_adapt_propag
       integer                                 :: error
       integer                                 :: frictype
       integer                                 :: genmode
       integer                                 :: inrhog
       integer                                 :: itermx
       integer                                 :: itest
       integer                                 :: itrace
       integer                                 :: modsim           ! 0: stationary, 2: non-stationary input, 3: non-stationary input and calculation
                                                                   ! modsim = 1 may not be used: is replaced by hotfile
                                                                   ! stationary: modsim is set to 2 (0 is not used anymore)
       integer                                 :: mxr
       integer                                 :: mxw
       integer                                 :: myr
       integer                                 :: myw
       integer                                 :: nbound
       integer                                 :: ncrp
       integer                                 :: ncrv
       integer                                 :: ncurv
       integer                                 :: n_meteofiles_gen
       integer                                 :: nnest
       integer                                 :: nobst
       integer                                 :: npoints
       integer                                 :: nscr
       integer                                 :: nttide
       integer                                 :: refjulday
       integer                                 :: whitecap         ! 0: off, 1: on, 2: westhuysen
       integer                                 :: nloc
       integer                                 :: swdis
       !
       integer       , dimension(4)            :: ts_wl
       integer       , dimension(4)            :: ts_xv
       integer       , dimension(4)            :: ts_yv
       integer       , dimension(4)            :: ts_ws
       integer       , dimension(4)            :: ts_wd
       !
       integer       , dimension(:), pointer   :: reflection
       integer       , dimension(:), pointer   :: refl_type        ! 1: specular, 2: diffuse
       integer       , dimension(:), pointer   :: nclin
       integer       , dimension(:), pointer   :: nlin
       !
       logical                                 :: append_com
       logical                                 :: breaking
       logical                                 :: checkVersionNumber = .true.
       logical                                 :: compmode
       logical                                 :: corht
       logical                                 :: curvi
       logical                                 :: curviwind
       logical                                 :: fshift
       logical                                 :: hotfile
       logical                                 :: nautconv
       logical                                 :: output_points
       logical                                 :: output_pnt_file
       logical                                 :: output_spec1d
       logical                                 :: output_spec2d
       logical                                 :: output_table
       logical                                 :: quadruplets
       logical                                 :: refraction
       logical                                 :: setup
       logical                                 :: sferic
       logical                                 :: swbot
       logical                                 :: swflux
       logical                                 :: swmor
       logical                                 :: swuvi
       logical                                 :: swuvt
       logical                                 :: swwindt
       logical                                 :: swwav
       logical                                 :: swwlt
       logical                                 :: timedependent
       logical                                 :: triads
       logical                                 :: useflowdata      ! true when FLOW data is used
       logical                                 :: varwin
       logical                                 :: varfri
       logical                                 :: windgrowth
       !
       real                                    :: alpw
       real                                    :: cdd
       real                                    :: cfbr1
       real                                    :: cfbr2
       real                                    :: cftriad1
       real                                    :: cftriad2
       real                                    :: css
       real                                    :: deltc            ! used when modsim = 3: Time step in non-stat SWAN runs
       real                                    :: deltcom          ! used when modsim = 3: Interval of communication FLOW-WAVE
       real                                    :: inthotf
       real                                    :: depmin
       real                                    :: dh_abs
       real                                    :: diffr_coeff
       real                                    :: drel
       real                                    :: dt_abs
       real                                    :: dxw
       real                                    :: dyw
       real                                    :: excval
       real                                    :: frcof
       real                                    :: gamma0           ! Default gamma0, having a realistic value even if no boundaries are modelled. If boundaries are present gamma0 =  bnd(1)%gamma0
       real                                    :: grav
       real                                    :: northdir
       real                                    :: percwet
       real                                    :: rho
       real                                    :: rhomud
       real                                    :: viscmud
       real                                    :: xw
       real                                    :: yw
       real                                    :: wavm_write_interval
       real                                    :: int2keephotfile
       real                                    :: veg_height
       real                                    :: veg_diamtr
       integer                                 :: veg_nstems
       integer                                 :: maxerr       = 2 ! Corresponds to maxerr in SWAN: maximum level of errors with which the calculation will continue
       real                                    :: veg_drag
       !
       real                                    :: wlevelcorr       ! Overall water level correction; see Time frame input in GUI
       real          , dimension(:), pointer   :: timwav
       real          , dimension(:), pointer   :: zeta             ! Default water level of a selected time point (when running stand-alone); see Time frame input in GUI
       real          , dimension(:), pointer   :: ux0
       real          , dimension(:), pointer   :: uy0
       real          , dimension(:), pointer   :: wdir
       real          , dimension(:), pointer   :: wvel
       !
       real          , dimension(:), pointer   :: f
       real          , dimension(:), pointer   :: obet
       real          , dimension(:), pointer   :: ogam
       real          , dimension(:), pointer   :: refl_coeff
       real          , dimension(:), pointer   :: trane
       real          , dimension(:), pointer   :: xpcu
       real          , dimension(:), pointer   :: xpob
       real          , dimension(:), pointer   :: ypcu
       real          , dimension(:), pointer   :: ypob
       real          , dimension(:,:), pointer :: xyloc
       !
       character(4)                             :: prnumb
       character(7) , dimension(:), allocatable :: add_out_names
       character(80), dimension(:), allocatable :: meteofile_gen
       character(20)                            :: versionNumberOK = '40.51a' ! No capitals!!!
       character(16)                            :: prname
       character(37)                            :: rgfout
       character(37)                            :: wfil
       character(37)                            :: ffil
       character(37)                            :: curvefil
       character(37)                            :: pntfil
       character(72)                            :: title1
       character(72)                            :: title2
       character(72)                            :: title3
       character(256)                           :: casl
       character(256)                           :: filcom
       character(256)                           :: filnam
       character(256)                           :: specfile
       character(15)                            :: usehottime    = '00000000.000000'       ! Time in the name of the hotfile that has to be used by SWAN
       character(15)                            :: writehottime  = '00000000.000000'       ! Time in the name of the hotfile that has to be written by SWAN
       character(15)                            :: keephottime   = '00000000.000000'       ! Time in the name of the hotfile that should not be deleted
       character(20), dimension(:), allocatable :: pntfilnam
       !
       type(handletype)                         :: tseriesfile
       !
       type(swan_bnd), dimension(:), pointer    :: bnd
       type(swan_dom), dimension(:), pointer    :: dom
    end type swan
    !
    type (swan),save :: swan_run
    !
    integer, parameter :: q_bath = 1 ! used as index in array qextnd
    integer, parameter :: q_wl   = 2 ! used as index in array qextnd
    integer, parameter :: q_cur  = 3 ! used as index in array qextnd
    integer, parameter :: q_wind = 4 ! used as index in array qextnd
contains
!
!
!==============================================================================
subroutine alloc_swan(sr)
   implicit none
   !
   type (swan) :: sr
   integer     :: i
   !
   allocate (sr%timwav  (sr%maxsteps ))
   allocate (sr%zeta    (sr%maxsteps ))
   allocate (sr%ux0     (sr%maxsteps ))
   allocate (sr%uy0     (sr%maxsteps ))
   allocate (sr%wvel    (sr%maxsteps ))
   allocate (sr%wdir    (sr%maxsteps ))
   !
   allocate (sr%nclin   (sr%maxcurv))
   allocate (sr%nlin    (sr%maxobst))
   allocate (sr%f       (sr%maxobst))
   allocate (sr%obet    (sr%maxobst))
   allocate (sr%ogam    (sr%maxobst))
   allocate (sr%trane   (sr%maxobst))
   allocate (sr%xpcu    (sr%maxcurv))
   allocate (sr%xpob    (sr%maxobst))
   allocate (sr%ypcu    (sr%maxcurv))
   allocate (sr%ypob    (sr%maxobst))
   !
   ! Only allocate the array below if output to locations has been defined
   ! in the mdw file
   !
   if (sr%output_points .and. .not. sr%output_pnt_file) &
   allocate (sr%xyloc   (2,sr%maxpoints))
   
   allocate (sr%reflection (sr%maxobst))
   allocate (sr%refl_type  (sr%maxobst))
   allocate (sr%refl_coeff (sr%maxobst))
   allocate (sr%bnd     (sr%maxbound ))
   do i = 1, sr%maxbound
      allocate (sr%bnd(i)%distance  (sr%maxsect(i)))
      allocate (sr%bnd(i)%waveheight(sr%maxsect(i)))
      allocate (sr%bnd(i)%period    (sr%maxsect(i)))
      allocate (sr%bnd(i)%direction (sr%maxsect(i)))
      allocate (sr%bnd(i)%dirspread (sr%maxsect(i)))
      allocate (sr%bnd(i)%spectrum  (sr%maxsect(i)))
   enddo
   allocate (sr%dom     (sr%maxnest  ))
   sr%dom(:)%n_meteofiles_dom = 0
end subroutine alloc_swan
!
!
!==============================================================================
subroutine read_swan (filnam, sr, wavedata)
   implicit none
   !
   character(256)              :: filnam
   type(swan)                  :: sr
   type(wave_data_type)        :: wavedata
   !
   integer            :: ind
   integer            :: iuntim 
   integer            :: istat
   integer            :: it01
   integer, external  :: new_lun
   real               :: tscale
   logical            :: ex
   logical            :: keywbased
   character(256)     :: line
   !
   sr%filnam        = filnam
   sr%prname        = ''
   sr%prnumb        = ''
   sr%title1        = ''
   sr%title2        = ''
   sr%title3        = ''
   sr%useflowdata   = .false.
   sr%swmor         = .false.
   sr%swwlt         = .false.
   sr%swuvt         = .false.
   sr%swwindt       = .false.
   sr%timedependent = .false.
   !
   keywbased = .false.
   call read_keyw_mdw(sr, wavedata, keywbased)
   if (.not.keywbased) then
      !
      ! The following select case is copied from waves_main to here and to the keyword reading version
      ! to enable "online" activation via the keyword based mdw-file
      !
      select case (wavedata%mode)
      case (stand_alone)
         write(*,'(a)') '*** MESSAGE: Delft3D-WAVE runs stand alone'
      case (flow_online)
         write(*,'(a)') '*** MESSAGE: Delft3D-WAVE runs online with Delft3D-FLOW'
      case (flow_mud_online)
         write(*,'(a)') '*** MESSAGE: Delft3D-WAVE runs online with Delft3D-FLOW, including MUD'
      end select
      !
      ! overcome the problem of fixed array sizes by first scanning the mdw file
      !
      call scan_mdw(sr)
      !
      ! Return to main code if scan routine gave errors
      !
      if ( sr%error /= 0 ) return
      !
      ! default memory allocation for old files
      !
      call alloc_swan(swan_run)
      !
      ! The following if part is moved from wave_init to here
      ! to avoid "waves_alone" reading when the mdw-file is keyword based
      !
      if (wavedata%mode == stand_alone) then
         !
         ! Old way of running stand alone:
         ! File with name 'waves_alone', containing tscale and it01
         ! New way of running stand alone:
         ! Read it01 from mdw-file (see file swan_input.f90)
         !
         inquire (file = 'waves_alone', exist = ex)
         if (ex) then
            iuntim = new_lun()
            open (iuntim, file = 'waves_alone', status = 'old', iostat = istat)
            if (istat /= 0) goto 999
            !
            read (iuntim, "(A)", iostat = istat) line
            if (istat /= 0) goto 999
            read (line, * ,iostat = istat) tscale
            if (istat /= 0) goto 999
            call settscale(wavedata%time, tscale)
            !
            read (iuntim, "(A)", iostat = istat) line
            if (istat /= 0) goto 999
            read (line, * ,iostat = istat) it01
            if (istat /= 0) goto 999
            call setrefdate(wavedata%time, it01)
            !
            close (iuntim)
            write (*,'(a)') '  File ''waves_alone'' read.'
         endif
      endif
      call read_swan_mdw(sr%casl     ,wavedata   , &
                       & sr%swmor    ,sr%swwlt   ,sr%swuvt   , &
                       & sr%swwav    ,sr%swuvi   ,sr%corht   ,sr%curvi  , &
                       & sr%swbot    ,sr%swflux  ,sr%filcom  ,sr%rgfout ,sr%prname , &
                       & sr%prnumb   ,sr%title1  ,sr%title2  ,sr%title3 , &
                       & sr%nnest    ,sr%nttide  ,sr%itest   ,sr%itrace , &
                       & sr%zeta     ,sr%ux0     ,sr%uy0     ,sr%css    ,sr%cdd    , &
                       & sr%grav     ,sr%rho    , &
                       & sr%timwav   ,sr%wfil    ,sr%nobst   ,sr%nscr   , &
                       & sr%xw       ,sr%yw      ,sr%alpw    ,sr%mxw    ,sr%myw    , &
                       & sr%dxw      ,sr%dyw     ,sr%trane   ,sr%f      ,sr%ogam   , &
                       & sr%obet     ,sr%xpob    ,sr%ypob    ,sr%nlin   ,sr%varwin , &
                       & sr%ncurv    ,sr%ncrv    ,sr%nclin   ,sr%xpcu   ,sr%ypcu   , &
                       & sr%ncrp     ,sr%filnam  ,sr%error   ,sr%inrhog , &
                       & sr%mxr      ,sr%myr     ,sr%ffil    , &
                       & sr%maxsteps ,sr%maxobst ,sr%maxcurv ,sr        )
   endif
   ind=index(filnam,'.mdw')
   sr%casl=filnam(1:ind-1)
   return
999 continue
   write (*,'(a)') '*** ERROR: While reading file ''waves_alone''.'
   stop 
end subroutine read_swan
!
!
!==============================================================================
subroutine scan_mdw(sr)
    !
    implicit none
    !
    ! Global variables
    !
    type(swan)                  :: sr
    !
    ! Local variables
    !
    integer                     :: bndtyp
    integer                     :: convar
    integer                     :: cp
    integer                     :: cs
    integer                     :: error
    integer                     :: gridtype
    integer                     :: i, j
    integer                     :: ios
    integer                     :: ipfl
    integer                     :: irec
    integer                     :: iuni
    integer                     :: nclin
    integer                     :: ncurv
    integer                     :: nlin
    integer                     :: nnest_rec
    integer                     :: nttide_rec
    integer                     :: nwind_rec
    integer                     :: num
    integer                     :: obstyp
    integer                     :: parread
    integer                     :: pnts
    integer                     :: swmr
    integer                     :: swwt
    integer                     :: swut
    integer                     :: swwnd
    integer                     :: turn
    integer                     :: windtype
    integer, external           :: new_lun
    integer, parameter          :: NWIND       = 7
    integer, parameter          :: NNEST       = 8
    integer, parameter          :: NTTIDE      = 14
    real                        :: xpcu
    real                        :: ypcu
    character(len=256)          :: line
    character(len=256)          :: vers
    character(len=256)          :: bndnam
    character(len=256)          :: wfil
!
! Executable statements --------------------------------------------------------
!

    num  = 0
    irec = 0
    iuni = new_lun()
    open (iuni, file = sr%filnam, iostat = ios)
    if (ios /= 0) then   
        write (*,'(a,a,a)') '*** ERROR: While opening file ''',trim(sr%filnam),''','
        write (*,'(a,i5,a,a,a)')  '           Error on Record ',irec, ' in file ''',trim(sr%filnam),'''.'
        sr%error = 1
        return
    endif
    rewind (iuni, iostat = ios)
    if (ios /= 0) then
        write (*,'(a,a,a)') '*** ERROR: While reading file ''',trim(sr%filnam),''','
        write (*,'(a,i5)')  '           Record ',irec
        sr%error = 4
        return
    endif
    read (iuni, '(a)', iostat = ios) line
    if (ios /= 0) then
        write (*,'(a,a,a)') '*** ERROR: While reading file ''',trim(sr%filnam),''','
        write (*,'(a,i5)')  '           Record ',irec
        sr%error = 4
        return
    endif
    !
    ! We need to know the swan version number before scanning the mdw file
    ! The code below takes care of that
    i = index (line,'version')
    read (line(i+8:),'(a)') vers
    !
    ! Checking on versionnumber is done on character basis (as in WAVE-GUI)
    ! This can be disturbed when the strings contains trailing spaces
    ! Clean up the variable vers, such that it has the format '1.11.11'
    ! Do not bother about wrongly placed dots or digits
    !
    vers = adjustl(vers)
    do j=1,len(vers)
       select case(vers(j:j))
       case(' ')
          if (j==2 .or. j==5) then
             vers(j:j) = '.'
          else
             vers(j:j) = '0'
          endif
       case('.')
          ! nothing
       case('0':'9')
          ! nothing
       case default
          write (*,'(a)') '*** ERROR: Unable to read WAVE-GUI version number'
          write (*,'(a,a,a)') '*** ERROR: While reading file ''',trim(sr%filnam),''','
          write (*,'(a,i5)')  '           Record ',irec
          sr%error = 4
          return
       end select
    enddo
    if (i==0 .or. llt(vers,'4.87.00')) then
       write (*,'(3a)') '*** ERROR: mdw_file created with WAVE-GUI version ', &
            & trim(vers), ' is not supported.'
       write (*,'(a,a,a)') '*** ERROR: While reading file ''',trim(sr%filnam),''','
       write (*,'(a,i5)')  '           Record ',irec
       sr%error = 4
    endif

    !------SWAN version number---------------------------------------
    ! The records within the mdw file shift a single line when there
    ! is no SWAN version number anymore in the newer versions
    if (lge(vers, '4.91.00')) then
       !
       ! SWAN version number is removed
       !
       irec = 0
    else
       read (iuni, '(a)', iostat = ios) line
       irec = irec + 1
    endif
    !
    ! This is the record for checking wind use
    nwind_rec = NWIND + irec
    ! This is the record for the number of computational grids
    nnest_rec = NNEST + irec
    ! This is the record for the number of tidal points
    nttide_rec = NTTIDE + irec
    !
    ! Now scan the file to obtain the maxbound, maxnest, maxpoints, and maxsteps values
    ! to be able to allocate the arrays dynamically in the alloc_swan routine
    !
    do while ( ios == 0 )
        if ( line(1:1) /= '*' ) then
            !
            ! This is a 'true' record so increase irec
            irec = irec + 1
            
            ! Fixed lines in the input file contain the wanted information so obtain them
            ! using the correct record indices
            if (irec == nwind_rec) then
                swmr  = 0
                swwt  = 0
                swut  = 0
                swwnd = 0
                if (lge(vers, '4.90.06')) then
                   read (line, *, iostat = ios) swmr, swwt, swut, swwnd
                   if (swwnd == 1) sr%swwindt = .true.
                else
                   sr%swwindt = .false.
                endif
            elseif (irec == nnest_rec) then
                ! This is the record that contains the number of computational grids
                read (line, *, iostat = ios) sr%maxnest
            elseif (irec == (nttide_rec + (sr%maxnest - 1)*5)) then
                ! Obtain the number of tidal points to be able to find the correct line
                ! for the boundaries
                read (line, *, iostat = ios) sr%maxsteps
            elseif (irec == (nttide_rec + ((sr%maxnest - 1)*5) + sr%maxsteps + 2)) then
                ! This is the record that contains the number of boundaries
                read (line, *, iostat = ios) sr%maxbound
            elseif (irec == (nttide_rec + ((sr%maxnest - 1)*5) + sr%maxsteps + 3)) then
                ! Here the records that contain information about sections within boundaries
                ! start, so allocate the memory for those sections (only if maxbound > 0)
                if (sr%maxbound > 0) allocate (sr%maxsect(sr%maxbound))
                do i = 1, sr%maxbound
                    ! Now scan the properties of the boundaries to find out the value of nsect
                    ! Skip lines with unnecessary information and increase irec
                    if ( line(1:1) /= '*' ) then
                        ! This is the first boundary in the mdw file so line has already been read
                        read (line, *, iostat = ios) bndnam, parread, bndtyp, convar
                    else
                        ! This is not the first boundary so read parameters from file and increase irec
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        read (line, *, iostat = ios) bndnam, parread, bndtyp, convar
                        irec = irec + 1
                    endif
                    ! Again skip the lines that start with a '*'
                    do
                        read (iuni, '(a)', iostat = ios) line
                        if ( line(1:1) /= '*' ) exit
                    enddo
                    irec = irec + 1
                    if (parread==2) then
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        irec = irec + 1
                    endif
    
                    if (convar==1) then
                        sr%maxsect(i) = 1
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        irec = irec + 1
                    else
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        read (line, *, iostat = ios) sr%maxsect(i), turn
                        irec = irec + 1
                        do j = 1, sr%maxsect(i)
                            ! Again skip the lines that start with a '*'
                            do
                                read (iuni, '(a)', iostat = ios) line
                                if ( line(1:1) /= '*' ) exit
                            enddo
                            irec = irec + 1
                        enddo
                    endif
                    line = '*'
                enddo
                ! At this point we are sure that we are at the location where the obstacles are defined
                ! Again skip the lines that start with a '*'
                if ( line(1:1) == '*' ) then
                    do
                        read (iuni, '(a)', iostat = ios) line
                        if ( line(1:1) /= '*' ) exit
                    enddo
                    irec = irec + 1
                endif
                ! This is the record that contains the number of obstacles
                read (line, *, iostat = ios) sr%maxobst
                cp     = 0
                if (sr%maxobst>0) then
                    ! Loop over all obstacles to find out the amount of corner points (cp)
                    ! Then make sure maxobst has the size of the amount of cornerpoints for
                    ! allocation purposes
                    do i = 1, sr%maxobst
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        read (line, *, iostat = ios) obstyp
                        irec = irec + 1
                        if (obstyp==1) then
                            ! Again skip the lines that start with a '*'
                            do
                                read (iuni, '(a)', iostat = ios) line
                                if ( line(1:1) /= '*' ) exit
                            enddo
                            irec = irec + 1
                        elseif (obstyp==2) then
                            ! Again skip the lines that start with a '*'
                            do
                                read (iuni, '(a)', iostat = ios) line
                                if ( line(1:1) /= '*' ) exit
                            enddo
                            irec = irec + 1
                        endif
                        if (lge(vers, '4.88.08')) then
                            ! Again skip the lines that start with a '*'
                            do
                                read (iuni, '(a)', iostat = ios) line
                                if ( line(1:1) /= '*' ) exit
                            enddo
                            irec = irec + 1
                        endif
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        read (line, *, iostat = ios) nlin
                        irec = irec + 1
                        do j = 1, nlin
                            cp = cp + 1
                            ! Again skip the lines that start with a '*'
                            do
                                read (iuni, '(a)', iostat = ios) line
                                if ( line(1:1) /= '*' ) exit
                            enddo
                            irec = irec + 1
                        enddo
                    enddo
                    sr%maxobst = cp
                endif
                ! Continue scanning the file until (finally) we've reached the curves part
                ! Again skip the lines that start with a '*'
                ! Gravity etc.
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                ! Convention, setup, forces
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                !  Wind, only when not using FLOW wind
                !
                if (.not. sr%swwindt) then
                    ! If wind info is present, read it
                    ! Again skip the lines that start with a '*'
                    do
                        read (iuni, '(a)', iostat = ios) line
                        if ( line(1:1) /= '*' ) exit
                    enddo
                    irec = irec + 1
                    read (line, *, iostat = ios) windtype
                    if (windtype == 1) then
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        irec = irec + 1
                    elseif (windtype == 2) then
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        irec = irec + 1
                        read (line, *, iostat = ios) gridtype, wfil
                        if (gridtype==2) then
                            ! Again skip the lines that start with a '*'
                            do
                                read (iuni, '(a)', iostat = ios) line
                                if ( line(1:1) /= '*' ) exit
                            enddo
                            irec = irec + 1
                        endif
                    else
                        write (*,'(a,a,a)') '*** ERROR: Wrong wind type in file ''',trim(sr%filnam),''','
                        write (*,'(a,a,a)') '*** ERROR: While reading file ''',trim(sr%filnam),''','
                        write (*,'(a,i5)')  '           Record ',irec
                        sr%error = 4
                        return
                    endif
                endif
                !
                ! Type of formulations
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                ! Depth induced breaking, alpha, gamma
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                ! Bottom friction, friction coefficient
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                ! Non-linear triad interactions, alpha, beta
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                ! Diffraction, smoothing coefficient, smoothing steps, adaptation of propagation
                ! Again skip the lines that start with a '*'
                if (lge(vers, '4.88.08')) then
                    do
                        read (iuni, '(a)', iostat = ios) line
                        if ( line(1:1) /= '*' ) exit
                    enddo
                    irec = irec + 1
                endif
                !
                ! Y/N windgrowth, white-capping, quadruplets, refraction, frequency shift
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                ! Directional space, frequency space
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                ! Hs-Tm01, Hs, Tm01, percentage of wet grid points, maximum number of iterations
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                ! Finally we've reached the point where we can determine sr%maxcurv!
                ! Loop over all curves to find out the amount of curve segments (cs)
                ! Then make sure maxcurv has the size of the amount of curve segments for
                ! allocation purposes
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                read (line, *, iostat = ios) ncurv
                cs = 0
                if (ncurv>0) then
                    do i = 1, ncurv
                        cs     = cs + 1
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        irec = irec + 1
                        read (line, *, iostat = ios) nclin, xpcu, ypcu
                        nclin = nclin + 1
                        do j = 2, nclin
                            cs = cs + 1
                            ! Again skip the lines that start with a '*'
                            do
                                read (iuni, '(a)', iostat = ios) line
                                if ( line(1:1) /= '*' ) exit
                            enddo
                            irec = irec + 1
                            read (line, *, iostat = ios) nclin, xpcu, ypcu
                        enddo
                   enddo
                endif
                sr%maxcurv = cs
                !
                ! Level of test output, debug level, Y/N compute waves, Y/N activate hotstart file
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                ! Y/N output to Flow grid; filename of Flow grid
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                !
                ! Y/N output to locations, this is the place to find the maxpoints number to
                ! dynamically allocate sr%xyloc within the alloc_swan routine
                ! Again skip the lines that start with a '*'
                do
                    read (iuni, '(a)', iostat = ios) line
                    if ( line(1:1) /= '*' ) exit
                enddo
                irec = irec + 1
                read (line, *, iostat = ios) pnts
                if (pnts == 1) then
                    sr%output_points = .true.
                    ! Again skip the lines that start with a '*'
                    do
                        read (iuni, '(a)', iostat = ios) line
                        if ( line(1:1) /= '*' ) exit
                    enddo
                    irec = irec + 1
                    read (line, *, iostat = ios) ipfl
                    if (ipfl == 1) then
                        sr%output_pnt_file = .true.
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        irec = irec + 1
                    else
                        sr%output_pnt_file = .false.
                        ! Here the number of locations are defined, which
                        ! define maxpoints
                        ! Again skip the lines that start with a '*'
                        do
                            read (iuni, '(a)', iostat = ios) line
                            if ( line(1:1) /= '*' ) exit
                        enddo
                        irec = irec + 1
                        read (line, *, iostat = ios) sr%maxpoints
                    endif
                endif
            endif
        endif
        !
        ! Read the next line
        !
        read (iuni, '(a)', iostat = ios, end = 100) line
    enddo
    
    if (ios /= 0) then
        ! An error during the reading of the file, no eof found yet
        write (*,'(a,a,a)') '*** ERROR: While reading file ''',trim(sr%filnam),''','
        write (*,'(a,i5)')  '           Record ',irec
        sr%error = 4
        return
    endif
    
    100 continue
    
    ! The maxbound variable may not be zero, since it will lead to an error within
    ! the read_swan_mdw routine. Change it here to not disturb the scanning of the file
    ! Also allocate sr%maxsect since it has not been done within the loop since maxbound = 0
    if (sr%maxbound == 0) then
        sr%maxbound = 1
        allocate (sr%maxsect(sr%maxbound))
        sr%maxsect = 0
    endif
    
    close(iuni, iostat = ios)
    if (ios /= 0) then
        write (*,'(a,a,a)') '*** ERROR: While closing file ''',trim(sr%filnam),''','
        write (*,'(a,i5,a,a,a)')  '           Error on Record ',irec, ' in file ''',trim(sr%filnam),'''.'
        sr%error = 2
        return
    endif
    
end subroutine scan_mdw
!
!
!==============================================================================
subroutine read_keyw_mdw(sr          ,wavedata   ,keywbased )
    use properties
    use read_grids
    implicit none
    !
    type(swan)                  :: sr
    type(wave_data_type)        :: wavedata
    logical                     :: keywbased
    !
    type(tree_data)   , pointer :: mdw_ptr
    type(tree_data)   , pointer :: gen_ptr
    type(tree_data)   , pointer :: out_ptr    
    type(tree_data)   , pointer :: node_ptr
    type(tree_data)   , pointer :: obst_ptr
    type(tree_data)   , pointer :: pol_ptr
    type(tree_data)   , pointer :: bnd_ptr
    type(tree_data)   , pointer :: dom_ptr
    type(tree_data)   , pointer :: tmp_ptr
    integer                     :: boundnr
    integer                     :: def_dirspace
    integer                     :: def_ndir
    integer                     :: def_nfreq
    integer                     :: domainnr
    integer                     :: i
    integer                     :: ii
    integer                     :: io
    integer                     :: istat
    integer                     :: j
    integer                     :: nbound
    integer                     :: nlocc
    integer                     :: jj    
    integer                     :: ndomains
    integer                     :: nobst
    integer                     :: nobstpnt
    integer                     :: nsect
    integer                     :: ntimes
    integer                     :: obstnr
    integer                     :: obstpnt
    integer                     :: refdate
    integer                     :: sectnr
    integer                     :: timenr
    integer                     :: timetable
    integer                     :: iter
    integer                     :: n_outpars
    integer                     :: par
    integer, dimension(4)       :: def_ts_hs
    integer, dimension(4)       :: def_ts_tp
    integer, dimension(4)       :: def_ts_wd
    integer, dimension(4)       :: def_ts_ds
    logical                     :: flag
    logical                     :: success
    real                        :: def_startdir
    real                        :: def_enddir
    real                        :: def_freqmin
    real                        :: def_freqmax
    real                        :: tscale
    real, dimension(2)          :: xy
    character(10)               :: versionstring
    character(37)               :: obstfil
    character(37)               :: tseriesfilename
    character(37)               :: polylinefile
    character(80)               :: parname
    character(256)              :: errorstring
    character(80),dimension(:), allocatable :: tmp_add_out_names
    character(80),dimension(:), allocatable :: tmp_meteofile
    character(1), dimension(:), pointer     :: data_ptr
    type(swan_bnd)            , pointer     :: bnd
    type(swan_dom)            , pointer     :: dom
    !
    ! Try opening wave file as keyword based file
    !
    nullify(mdw_ptr)
    call tree_create('Delft3D-WAVE input', mdw_ptr)
    istat = 0
    call prop_file('ini', trim(sr%filnam), mdw_ptr, istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          write(*,*) '*** ERROR File: '//trim(sr%filnam)//' not found'
       case(3)
          write(*,*) '*** ERROR Premature EOF in file: '//trim(sr%filnam)
       case default
          write(*,*) '*** ERROR Read error from file: '//trim(sr%filnam)
       endselect
       stop
    endif
    !
    ! Check version number of wave input file
    !
    versionstring = ''
    call prop_get_string(mdw_ptr, 'WaveFileInformation', 'FileVersion', versionstring)
    if (trim(versionstring) /= '02.00') return
    keywbased = .true.
    !
    ! gen_ptr is used later on
    !
    call tree_get_node_by_name( mdw_ptr, 'General', gen_ptr )
    !
    ! From here on we know that we read the input data from a keyword based
    ! mdw file.
    !
    call prop_get_string (mdw_ptr, 'General', 'Projectname'    , sr%prname)
    call prop_get_string (mdw_ptr, 'General', 'Projectnr'      , sr%prnumb)
    call prop_get_string (mdw_ptr, 'General', 'Description1'   , sr%title1)
    call prop_get_string (mdw_ptr, 'General', 'Description2'   , sr%title2)
    call prop_get_string (mdw_ptr, 'General', 'Description3'   , sr%title3)
    call prop_get_logical(mdw_ptr, 'General', 'OnlyInputVerify', flag)
    sr%compmode = .not. flag
    !
    sr%deltc = -999.0
    parname  = ''
    call prop_get_string (mdw_ptr, 'General', 'SimMode', parname)
    select case (parname)
    case ('stationary')
       !
       ! Use modsim = 2 also for stationary input
       !
       sr%modsim = 2
    case ('quasi-stationary')
       sr%modsim = 2
    case ('non-stationary')
       sr%modsim = 3
       call prop_get_real   (mdw_ptr, 'General', 'TimeStep', sr%deltc)
       if (sr%deltc < 0.0) then
          write(*,*) 'SWAN_INPUT: missing or invalid non-stationary time step'
          goto 999
       endif
    case default
       write(*,*) 'SWAN_INPUT: missing or invalid simulation mode'
       goto 999
    end select
    !
    parname = ''
    call prop_get_string (mdw_ptr, 'General', 'FlowFile', parname)
    if (parname /= ' ') then
       call setmode(wavedata, flow_online)
       parname = ''
       call prop_get_string (mdw_ptr, 'General', 'FlowMudFile', parname)
       if (parname /= ' ') then
          call setmode(wavedata, flow_mud_online)
       endif
    endif
    select case (wavedata%mode)
    case (stand_alone)
       write(*,'(a)') '*** MESSAGE: Delft3D-WAVE runs stand alone'
    case (flow_online)
       write(*,'(a)') '*** MESSAGE: Delft3D-WAVE runs online with Delft3D-FLOW'
    case (flow_mud_online)
       write(*,'(a)') '*** MESSAGE: Delft3D-WAVE runs online with Delft3D-FLOW, including MUD'
    end select
    !
    parname = ''
    call prop_get_string (mdw_ptr, 'General', 'DirConvention', parname)
    call lowercase(parname, len(parname))
    select case (parname)
    case ('nautical')
      sr%nautconv = .true.
    case ('cartesian')
      sr%nautconv = .false.
    case default
       write(*,*) 'SWAN_INPUT: missing or invalid direction convention'
       goto 999
    end select
    obstfil = ''
    call prop_get_string (mdw_ptr, 'General', 'ObstacleFile', obstfil)
    !
    ! Determine reference date. Date string converted from YYYY-MM-DD
    ! to YYYYMMDD.
    !
    parname = ''
    call prop_get_string (mdw_ptr, 'General', 'ReferenceDate', parname)
    parname(5:6) = parname(6:7)
    parname(7:8) = parname(9:10)
    parname(9:) = ' '
    read(parname,*,iostat=istat) refdate
    if (istat /= 0) then
       write(*,*) 'SWAN_INPUT: missing or invalid reference date'
       goto 999
    endif
    call setrefdate(wavedata%time,refdate)
    call juldat(refdate ,sr%refjulday)
    !
    tscale = 60.0
    call prop_get_real   (mdw_ptr, 'General', 'TScale', tscale)
    call settscale(wavedata%time, tscale)
    !
    tseriesfilename = ''
    call prop_get_string (mdw_ptr, 'General', 'TSeriesFile', tseriesfilename)
    if (tseriesfilename /= ' ') then
       sr%timedependent = .true.
       call readtable(sr%tseriesfile, newlun(), tseriesfilename, sr%refjulday, errorstring)
       if (errorstring /= ' ') then
          write(*,'(A)') trim(errorstring)
          goto 999
       endif
    endif
    !
    ! Time points
    !
    timetable = -999
    if (sr%timedependent) then
       call prop_get_integer(mdw_ptr, 'General', 'TimePntBlock', timetable)
    endif
    if (timetable > 0) then
       !
       ! time points from TSeriesFile
       !
       ntimes = gettablentimes(sr%tseriesfile, timetable, errorstring)
       if (errorstring /= ' ') then
          write(*,'(A)') trim(errorstring)
          goto 999
       endif
    else
       !
       ! Count number of time points
       !
       ntimes = 0
       do i = 1, size(mdw_ptr%child_nodes)
          tmp_ptr => mdw_ptr%child_nodes(i)%node_ptr
          parname = tree_get_name( tmp_ptr )
          if (parname == 'timepoint') ntimes = ntimes + 1
       enddo
       !
    endif
    ntimes    = max(ntimes,1)
    sr%nttide = ntimes
    !
    istat = 0
                  allocate (sr%timwav  (ntimes  ), stat = istat)
    if (istat==0) allocate (sr%zeta    (ntimes  ), stat = istat)
    if (istat==0) allocate (sr%ux0     (ntimes  ), stat = istat)
    if (istat==0) allocate (sr%uy0     (ntimes  ), stat = istat)
    if (istat==0) allocate (sr%wdir    (ntimes  ), stat = istat)
    if (istat==0) allocate (sr%wvel    (ntimes  ), stat = istat)
    !
    if (istat/=0) then
       write(*,*) 'SWAN_INPUT: memory alloc error (ntimes)'
       goto 999
    endif
    !
    sr%timwav = -999.0
    sr%zeta   = 0.0
    sr%ux0    = 0.0
    sr%uy0    = 0.0
    sr%wvel   = 0.0
    sr%wdir   = 0.0
    sr%swuvi  = .true.
    !
    ! Get default values for time-varying quantities
    !
    call prop_get_real   (mdw_ptr, 'General', 'WaterLevel', sr%zeta(1))
    sr%zeta = sr%zeta(1)
    !
    call prop_get_real   (mdw_ptr, 'General', 'XVeloc'    , sr%ux0(1))
    sr%ux0 = sr%ux0(1)
    !
    call prop_get_real   (mdw_ptr, 'General', 'YVeloc'    , sr%uy0(1))
    sr%uy0 = sr%uy0(1)
    !
    call prop_get_real   (mdw_ptr, 'General', 'WindSpeed' , sr%wvel(1))
    sr%wvel = sr%wvel(1)
    !
    call prop_get_real   (mdw_ptr, 'General', 'WindDir'   , sr%wdir(1))
    sr%wdir = sr%wdir(1)
    if (timetable > 0) then
       !
       ! Times obtained from table module are in hours
       !
       call gettabletimes(sr%tseriesfile, timetable, sr%timwav, sr%refjulday, &
                        & errorstring)
       sr%timwav = sr%timwav * 60.0
    else
       timenr = 0
       i = 1
       do j = 1, size(mdw_ptr%child_nodes)
          tmp_ptr => mdw_ptr%child_nodes(j)%node_ptr
          parname = tree_get_name( tmp_ptr )
          select case (parname)
          case ('timepoint')
             timenr = timenr + 1
             i      = timenr
             call prop_get_real   (tmp_ptr, '*', 'Time'      , sr%timwav(timenr))
             call prop_get_real   (tmp_ptr, '*', 'WaterLevel', sr%zeta(i))
             if (timenr==0) sr%zeta = sr%zeta(1)
             call prop_get_real   (tmp_ptr, '*', 'XVeloc'    , sr%ux0(i))
             if (timenr==0) sr%ux0 = sr%ux0(1)
             call prop_get_real   (tmp_ptr, '*', 'YVeloc'    , sr%uy0(i))
             if (timenr==0) sr%uy0 = sr%uy0(1)
             call prop_get_real   (tmp_ptr, '*', 'WindSpeed' , sr%wvel(i))
             if (timenr==0) sr%wvel = sr%wvel(1)
             call prop_get_real   (tmp_ptr, '*', 'WindDir'   , sr%wdir(i))
             if (timenr==0) sr%wdir = sr%wdir(1)
          case default
             !
             ! nothing
             !
          end select
       enddo
    endif
    !
    ! The following if statement is added to compare with old version
    !
    if (sr%ux0(1) == 0.0 .and. sr%uy0(1) == 0.0 ) then
       sr%swuvi = .false.
    endif
    !
    !
    ! Optionally find these general quantities in the tables
    !
    if (sr%timedependent) then
       call gettable(sr%tseriesfile, 'General', 'WaterLevel', sr%ts_wl, &
                   & 0     , errorstring)
       if (sr%ts_wl(3) > 1) then
          write(*,*) 'SWAN_INPUT: too many WaterLevel entries in TSeriesFile'
          goto 999
       endif
       !
       call gettable(sr%tseriesfile, 'General', 'XVeloc', sr%ts_xv, &
                   & 0     , errorstring)
       if (sr%ts_xv(3) > 1) then
          write(*,*) 'SWAN_INPUT: too many XVeloc entries in TSeriesFile'
          goto 999
       endif
       !
       call gettable(sr%tseriesfile, 'General', 'YVeloc', sr%ts_yv, &
                   & 0     , errorstring)
       if (sr%ts_yv(3) > 1) then
          write(*,*) 'SWAN_INPUT: too many YVeloc entries in TSeriesFile'
          goto 999
       endif
       !
       call gettable(sr%tseriesfile, 'General', 'WindSpeed', sr%ts_ws, &
                   & 0     , errorstring)
       if (sr%ts_ws(3) > 1) then
          write(*,*) 'SWAN_INPUT: too many WindSpeed entries in TSeriesFile'
          goto 999
       endif
       !
       call gettable(sr%tseriesfile, 'General', 'WindDir', sr%ts_wd, &
                   & 0     , errorstring)
       if (sr%ts_wd(3) > 1) then
          write(*,*) 'SWAN_INPUT: too many WindDir entries in TSeriesFile'
          goto 999
       endif
    endif
    !
    ! Default settings for domains
    !
    parname = ''
    call prop_get_string (mdw_ptr, 'General', 'DirSpace', parname)
    call lowercase(parname, len(parname))
    def_dirspace = -999
    select case (parname)
    case ('circle')
       def_dirspace = 1
    case ('sector')
       def_dirspace = 2
    case default
       if (parname /= '') then
          write(*,*) 'SWAN_INPUT: unknown General/DirSpace: ', parname
          goto 999
       endif
    end select
    def_ndir     = -999
    def_startdir = -999.0
    def_enddir   = -999.0
    def_nfreq    = -999
    def_freqmin  = -999.0
    def_freqmax  = -999.0
    call prop_get_integer(mdw_ptr, 'General', 'NDir'    , def_ndir)
    call prop_get_real   (mdw_ptr, 'General', 'StartDir', def_startdir)
    call prop_get_real   (mdw_ptr, 'General', 'EndDir'  , def_enddir)
    call prop_get_integer(mdw_ptr, 'General', 'NFreq'   , def_nfreq)
    call prop_get_real   (mdw_ptr, 'General', 'FreqMin' , def_freqmin)
    call prop_get_real   (mdw_ptr, 'General', 'FreqMax' , def_freqmax)
    !
    ! Count number of meteofiles in group General
    !
    call count_occurrences(mdw_ptr, 'General', 'meteofile', sr%n_meteofiles_gen)
    if (sr%n_meteofiles_gen  > 0) then
       !
       ! Allocate temporary array for meteofiles
       !
       allocate(tmp_meteofile(sr%n_meteofiles_gen), stat = istat)
       tmp_meteofile = ''
       !
       ! The input can be read
       !
       par     = 0
       call tree_get_node_by_name(mdw_ptr, 'General', tmp_ptr)
       do j = 1, size(tmp_ptr%child_nodes)
          !
          ! Does tmp_ptr contain one or more children with name MeteoFile?
          !
          node_ptr => tmp_ptr%child_nodes(j)%node_ptr
          !
          parname = ''
          call prop_get_string(node_ptr, '*', 'meteofile', parname)
          if ( parname /= '') then
             par = par + 1
             !
             ! Read value for keyword meteofile
             !
             call tree_get_data_string(node_ptr, parname, success)
             !
             ! Check double occurrences
             !
             do i = 1, par
                if (tmp_meteofile(i) == parname) then
                   write(*,'(a,a,a)') 'SWAN input: Group General: MeteoFile ', trim(parname), ' has already been read'
                   par = par - 1
                   sr%n_meteofiles_gen = sr%n_meteofiles_gen - 1
                   exit
                endif
             enddo
             !
             if (tmp_meteofile(par) == '') then
                !
                ! Array location for MeteoFile empty, so store the read item in the temporary meteofile array
                !
                tmp_meteofile(par) = trim(parname)
             endif
             if (par == sr%n_meteofiles_gen) then
                !
                ! Correct number of meteofiles read
                !
                exit
             endif
          endif
       enddo
       !
       ! Allocate array for meteofiles in group General
       !
       allocate (sr%meteofile_gen(sr%n_meteofiles_gen), stat = istat)
       !
       ! Fill the array with meteofiles in group General
       !
       sr%meteofile_gen(1:sr%n_meteofiles_gen) = tmp_meteofile(1:sr%n_meteofiles_gen)
       sr%swwindt = .true.
       !
       deallocate(tmp_meteofile)
       !
    endif
    !
    ! Constants
    !
    sr%grav       = 9.81
    sr%rho        = 1025.0
    sr%northdir   = 90.0
    sr%depmin     = 0.05
    sr%inrhog     = 1
    sr%wlevelcorr = 0.0
    sr%maxerr     = 2
    call prop_get_real   (mdw_ptr, 'Constants', 'Gravity'             , sr%grav)
    call prop_get_real   (mdw_ptr, 'Constants', 'WaterDensity'        , sr%rho)
    call prop_get_real   (mdw_ptr, 'Constants', 'NorthDir'            , sr%northdir)
    call prop_get_real   (mdw_ptr, 'Constants', 'MinimumDepth'        , sr%depmin)
    call prop_get_real   (mdw_ptr, 'Constants', 'WaterLevelCorrection', sr%wlevelcorr)
    call prop_get_integer(mdw_ptr, 'Constants', 'MaxErrorLevel'       , sr%maxerr)
    !
    ! Processes
    !
    sr%genmode       = -999
    sr%setup         = .false.
    sr%breaking      = .true.
    sr%cfbr1         = 1.0
    sr%cfbr2         = 0.73
    sr%triads        = .false.
    sr%cftriad1      = 0.1
    sr%cftriad2      = 2.2
    sr%frictype      = 1
    sr%frcof         = 0.067
    sr%diffr_coeff   = 0.2
    sr%diffr_smsteps = 5
    sr%windgrowth    = .true.
    sr%whitecap      = 1
    sr%quadruplets   = .false.
    sr%refraction    = .true.
    sr%fshift        = .true.
    !
    call prop_get_integer(mdw_ptr, 'Processes', 'GenModePhys', sr%genmode)
    if (sr%genmode < 0 .or. sr%genmode > 3) then
       write(*,*) 'SWAN_INPUT: missing or invalid generation mode'
       goto 999
    endif
    !
    call prop_get_logical(mdw_ptr, 'Processes', 'WaveSetup' , sr%setup)
    call prop_get_logical(mdw_ptr, 'Processes', 'Breaking'  , sr%breaking)
    if (sr%breaking) then
       call prop_get_real   (mdw_ptr, 'Processes', 'BreakAlpha', sr%cfbr1)
       call prop_get_real   (mdw_ptr, 'Processes', 'BreakGamma', sr%cfbr2)
    endif
    call prop_get_logical(mdw_ptr, 'Processes', 'Triads'    , sr%triads)
    if (sr%triads) then
       call prop_get_real   (mdw_ptr, 'Processes', 'TriadsAlpha', sr%cftriad1)
       call prop_get_real   (mdw_ptr, 'Processes', 'TriadsBeta' , sr%cftriad2)
    endif
    !
    parname = ''
    call prop_get_string (mdw_ptr, 'Processes', 'BedFriction', parname)
    call lowercase(parname,len(parname))
    select case (parname)
    case ('none', ' ')
      sr%frictype = 0
      sr%frcof    = 0.0
    case ('jonswap')
      sr%frictype = 1
      sr%frcof    = 0.067
    case ('collins')
      sr%frictype = 2
      sr%frcof    = 0.015
    case ('madsen et al.')
      sr%frictype = 3
      sr%frcof    = 0.05
    case default
       write(*,*) 'SWAN_INPUT: invalid bed friction type'
       goto 999
    end select
    if (sr%frictype > 0) then
       call prop_get_real   (mdw_ptr, 'Processes', 'BedFricCoef', sr%frcof)
    endif
    !
    flag           = .true.
    sr%diffraction = 1
    call prop_get_logical(mdw_ptr, 'Processes', 'Diffraction', flag)
    if (.not. flag) sr%diffraction = 0
    if (sr%diffraction == 1) then
       call prop_get_real   (mdw_ptr, 'Processes', 'DiffracCoef' , sr%diffr_coeff)
       call prop_get_integer(mdw_ptr, 'Processes', 'DiffracSteps', sr%diffr_smsteps)
       !
       flag                  = .true.
       sr%diffr_adapt_propag = 1
       call prop_get_logical(mdw_ptr, 'Processes', 'DiffracProp' , flag)
       if (.not. flag) sr%diffr_adapt_propag = 0
    endif
    !
    call prop_get_logical(mdw_ptr, 'Processes', 'WindGrowth'  , sr%windgrowth)
    parname = ''
    call prop_get_string (mdw_ptr, 'Processes', 'WhiteCapping', parname)
    call lowercase(parname, len(parname))
    select case (parname)
    case ('off')
      sr%whitecap = WC_OFF
    case ('komen',' ')
      sr%whitecap = WC_KOMEN
    case ('westhuysen')
      sr%whitecap = WC_WESTHUYSEN
      if (sr%genmode /= 3) then
        write (*,'(2a,i0)') 'SWAN_INPUT: WhiteCapping=Westhuysen can not be', &
             & ' combined with formulations of generation ',sr%genmode
        goto 999
      endif
    case default
       write(*,*) 'SWAN_INPUT: [Processes] WhiteCapping: invalid input:',trim(parname)
       goto 999
    end select
    call prop_get_logical(mdw_ptr, 'Processes', 'Quadruplets', sr%quadruplets)
    call prop_get_logical(mdw_ptr, 'Processes', 'Refraction' , sr%refraction)
    call prop_get_logical(mdw_ptr, 'Processes', 'FreqShift'  , sr%fshift)
    !
    parname = ''
    call prop_get_string (mdw_ptr, 'Processes', 'WaveForces', parname)
    call lowercase(parname, len(parname))
    select case (parname)
    case ('radiation stresses')
      sr%swdis = 1
    case ('dissipation',' ')
      sr%swdis = 2
    case ('dissipation 3d')
      sr%swdis = 3
    case default
       write(*,*) 'SWAN_INPUT: invalid method to compute wave forces'
       goto 999
    end select
    !
    ! Numerics
    !
    sr%cdd     = 0.5
    sr%css     = 0.5
    sr%drel    = 0.02
    sr%dh_abs  = 0.02
    sr%dt_abs  = 0.02
    sr%percwet = 98.0
    sr%itermx  = 15
    sr%gamma0  = 3.3
    !
    call prop_get_real   (mdw_ptr, 'Numerics', 'DirSpaceCDD'  , sr%cdd)
    call prop_get_real   (mdw_ptr, 'Numerics', 'FreqSpaceCSS' , sr%css)
    call prop_get_real   (mdw_ptr, 'Numerics', 'RChHsTm01'    , sr%drel)
    call prop_get_real   (mdw_ptr, 'Numerics', 'RChMeanHs'    , sr%dh_abs)
    call prop_get_real   (mdw_ptr, 'Numerics', 'RChMeanTm01'  , sr%dt_abs)
    call prop_get_real   (mdw_ptr, 'Numerics', 'PercWet'      , sr%percwet)
    call prop_get_integer(mdw_ptr, 'Numerics', 'MaxIter'      , sr%itermx)
    !
    ! General output options
    !
    sr%itest               = 0
    sr%itrace              = 0
    sr%hotfile             = .false.
    sr%wavm_write_interval = 0.0
    sr%swwav               = .false.
    sr%deltcom             = 0.0
    sr%append_com          = .false.
    sr%output_points       = .false.
    sr%output_pnt_file     = .false.
    sr%pntfil              = ' '
    sr%curvefil            = ' '
    sr%swflux              = .true.
    !
    ! Standard output options
    !
    call prop_get_integer(mdw_ptr, 'Output', 'TestOutputLevel' , sr%itest)
    call prop_get_logical(mdw_ptr, 'Output', 'TraceCalls'      , flag)
    if (flag) sr%itrace = 1
    call prop_get_logical(mdw_ptr, 'Output', 'UseHotFile'      , sr%hotfile)
    call prop_get_real   (mdw_ptr, 'Output', 'MapWriteInterval', sr%wavm_write_interval)
    call prop_get_logical(mdw_ptr, 'Output', 'WriteCOM'        , sr%swwav)
    call prop_get_logical(mdw_ptr, 'Output', 'MassFluxToCOM'   , sr%swflux)
    call prop_get_real   (mdw_ptr, 'Output', 'COMWriteInterval', sr%deltcom)
    call prop_get_logical(mdw_ptr, 'Output', 'AppendCOM'       , sr%append_com)
    !
    ! determine the number of location files
    !
    nlocc = 0
    call tree_get_node_by_name( mdw_ptr, 'Output', out_ptr )    
    do jj = 1,size(out_ptr%child_nodes)
       tmp_ptr => out_ptr%child_nodes(jj)%node_ptr
       parname = tree_get_name( tmp_ptr )
       if (parname == 'locationfile') then
          nlocc = nlocc+1
       endif
    enddo
    if (nlocc > 0) then
       sr%output_points   = .true.
       sr%output_pnt_file = .true.
       allocate (sr%pntfilnam(nlocc), stat = istat)
       if (istat/=0) then
          write(*,*) 'SWAN_INPUT: memory alloc error (pntfilnam)'
          goto 999
       endif
    endif
    !
    ! read the location files
    !
    nlocc = 0
    call tree_get_node_by_name( mdw_ptr, 'Output', out_ptr )
    do jj = 1,size(out_ptr%child_nodes)
       tmp_ptr => out_ptr%child_nodes(jj)%node_ptr
       parname = tree_get_name( tmp_ptr )
       if (parname == 'locationfile') then
          nlocc = nlocc+1
          call prop_get_string(tmp_ptr, '*', 'locationfile' ,sr%pntfilnam(nlocc))
       endif
    enddo
    sr%nloc            = nlocc
    sr%output_table    = .false.
    sr%output_spec1d   = .false.
    sr%output_spec2d   = .false.
    if (sr%output_pnt_file) then
       call prop_get_logical(mdw_ptr, 'Output', 'WriteTable'  , sr%output_table)
       call prop_get_logical(mdw_ptr, 'Output', 'WriteSpec1D' , sr%output_spec1d)
       call prop_get_logical(mdw_ptr, 'Output', 'WriteSpec2D' , sr%output_spec2d)
    endif
    call prop_get_string (mdw_ptr, 'Output', 'CurveFile', sr%curvefil)
    if (sr%curvefil /= ' ') then
       !
       ! unknown number of output curves defined in a Tekal file
       ! use ncurv = -1 to flag this
       !
       sr%ncurv = -1
    endif
    !
    ! Additional keywords? Count the number of occurrences
    !
    n_outpars = 0
    call count_occurrences(mdw_ptr, 'output', 'additionaloutput', n_outpars)
    if (n_outpars  > 0) then
       !
       ! Allocate temporary array for additional output parameters
       !
       allocate (tmp_add_out_names(n_outpars), stat = istat)
       !
       ! Initialize array
       !
       tmp_add_out_names = ' '
        !
       ! The input can be read
       !
       par = 0
       !
       call tree_get_node_by_name(mdw_ptr, 'output', out_ptr)
       do j = 1, size(out_ptr%child_nodes)
          !
          ! Does out_ptr contain one or more children with name AdditionalOutput?
          !
          node_ptr => out_ptr%child_nodes(j)%node_ptr
          !
          parname = ''
          call prop_get_string(node_ptr, '*', 'AdditionalOutput', parname)
          if ( parname /= '') then
             par = par + 1
             !
             ! Read the additional output parameter
             !
             call tree_get_data_string(node_ptr, parname, success)
             do i = 1, par
                if (tmp_add_out_names(i) == parname) then
                   write(*,'(3a)') 'SWAN input: Additional output parameter ', trim(parname), ' has already been read'
                   par = par - 1
                   n_outpars = n_outpars - 1
                   exit
                endif
             enddo
             !
             if (tmp_add_out_names(par) == ' ') then
                tmp_add_out_names(par) = trim(parname)
             endif
             if (par == n_outpars) then
                exit
             endif
          endif
       enddo
       !
       ! Allocate array for additional output parameters
       !
       allocate (sr%add_out_names(n_outpars), stat = istat)
       !
       ! Fill the array with additional output names
       !
       sr%add_out_names(1:n_outpars) = tmp_add_out_names(1:n_outpars)
       !
       deallocate(tmp_add_out_names)
       !
    endif
    !
    !  Interval to keep the hotfile
    !
    sr%int2keephotfile = 0.0
    call prop_get_real(mdw_ptr, 'Output', 'Int2KeepHotfile', sr%int2keephotfile)
    !
    !  Determine the number of domains
    !
    ndomains = 0
    do i = 1, size(mdw_ptr%child_nodes)
       tmp_ptr => mdw_ptr%child_nodes(i)%node_ptr
       parname = tree_get_name( tmp_ptr )
       if ( parname == 'domain') ndomains = ndomains + 1
    enddo
    if (ndomains == 0) then
       write(*,*) 'SWAN_INPUT: no domains found!'
       goto 999
    endif
    sr%nnest = ndomains
    !
    istat = 0
    allocate (sr%dom(ndomains  ), stat = istat)
    !
    if (istat /= 0) then
       write(*,*) 'SWAN_INPUT: memory alloc error (ndomains)'
       goto 999
    endif
    !
    sr%dom(1)%qextnd           = 0
    sr%dom(1)%flowVelocityType = FVT_DEPTH_AVERAGED
    !
    ! Put general FLOW mapping flags in dom(1)
    !
    if (wavedata%mode /= stand_alone) then
       call prop_get_integer(mdw_ptr, 'General', 'FlowBedLevel'  , sr%dom(1)%qextnd(q_bath))
       call prop_get_integer(mdw_ptr, 'General', 'FlowWaterLevel', sr%dom(1)%qextnd(q_wl)  )
       call prop_get_integer(mdw_ptr, 'General', 'FlowVelocity'  , sr%dom(1)%qextnd(q_cur) )
       call prop_get_integer(mdw_ptr, 'General', 'FlowWind'      , sr%dom(1)%qextnd(q_wind))
       parname = ''
       call prop_get_string (mdw_ptr, 'General', 'FlowVelocityType', parname)
       call lowercase(parname, len(parname))
       select case (parname)
       case ('depth-averaged')
          sr%dom(1)%flowVelocityType = FVT_DEPTH_AVERAGED
       case ('surface-layer')
          sr%dom(1)%flowVelocityType = FVT_SURFACE_LAYER
       case ('wave-dependent')
          sr%dom(1)%flowVelocityType = FVT_WAVE_DEPENDENT
       case (' ')
          !
          ! Default value used
          !
       case default
          write(*,*) 'SWAN_INPUT: invalid option for [General], FlowVelocityType'
          goto 999
       end select
    endif
    do i = 1, ndomains
       dom => sr%dom(i)
       dom%curvibot         = -999
       dom%dirspace         = def_dirspace
       dom%ndir             = def_ndir
       dom%nfreq            = def_nfreq
       dom%nestnr           = -999
       dom%cgnum            = .true.
       dom%botfil           = ''
       dom%curlif           = ''
       dom%depfil           = ''
       dom%nesfil           = ''
       dom%vegfil           = ''
       dom%nesnam           = '--DUMMY--' !!dummy
       dom%mxb              = -999
       dom%myb              = -999
       dom%mxc              = -999
       dom%myc              = -999
       dom%freqmin          = def_freqmin
       dom%freqmax          = def_freqmax
       dom%startdir         = def_startdir
       dom%enddir           = def_enddir
       dom%veg_height       = -999.0
       dom%veg_diamtr       = -999.0
       dom%veg_nstems       = 1
       dom%veg_drag         = 1
       dom%qextnd(q_bath)   = sr%dom(1)%qextnd(q_bath)
       dom%qextnd(q_wl)     = sr%dom(1)%qextnd(q_wl)
       dom%qextnd(q_cur)    = sr%dom(1)%qextnd(q_cur)
       dom%qextnd(q_wind)   = sr%dom(1)%qextnd(q_wind)
       dom%flowVelocityType = sr%dom(1)%flowVelocityType
    enddo
    !
    domainnr = 0
    do i = 1, size(mdw_ptr%child_nodes)
       tmp_ptr => mdw_ptr%child_nodes(i)%node_ptr
       parname = tree_get_name( tmp_ptr )
       if ( parname /= 'domain') cycle
       domainnr = domainnr + 1
       dom => sr%dom(domainnr)
       !
       ! Read computational grid
       !
       call prop_get_string(tmp_ptr, '*', 'Grid', dom%curlif)
       call readgriddims(dom%curlif, dom%mxc, dom%myc)
       if (dom%curlif == '') then
          write(*,*) 'SWAN_INPUT: grid not found for domain', domainnr
          goto 999
       endif
       !
       ! poles? No, fences!
       !
       dom%mxc = dom%mxc - 1
       dom%myc = dom%myc - 1
       !
       ! Read bathymetry
       !
       dom%depfil   = ''
       dom%curvibot = 1
       call prop_get_string(tmp_ptr, '*', 'BedLevelGrid', dom%depfil)
       if (dom%depfil /= '') then
          call readgriddims(dom%depfil, dom%mxb, dom%myb)
          !
          ! poles? No, fences!
          !
          dom%mxb    = dom%mxb - 1
          dom%myb    = dom%myb - 1
       else
          dom%depfil = dom%curlif
          dom%mxb    = dom%mxc
          dom%myb    = dom%myc
       endif
       call prop_get_string(tmp_ptr, '*', 'BedLevel', dom%botfil)
       if (dom%botfil == '') then
          write(*,*) 'SWAN_INPUT: bathymetry not found for domain', domainnr
          goto 999
       endif
       !
       flag           = .false.
       dom%vegetation  = 0 
       call prop_get_logical(tmp_ptr, '*', 'Vegetation', flag)
       if (flag) dom%vegetation = 1
       if (dom%vegetation == 1) then
          call prop_get_real   (tmp_ptr, '*', 'VegHeight' , dom%veg_height)
          call prop_get_real   (tmp_ptr, '*', 'VegDiamtr' , dom%veg_diamtr)
          call prop_get_integer(tmp_ptr, '*', 'VegNstems' , dom%veg_nstems)
          call prop_get_real   (tmp_ptr, '*', 'VegDrag' ,   dom%veg_drag)
          !
          ! Read vegetation map
          !
          call prop_get_string(tmp_ptr, '*', 'VegetationMap', dom%vegfil)
          if (dom%vegfil == '') then
             write(*,*) 'SWAN_INPUT: vegetation map not found for domain ', domainnr
             goto 999
          endif
       endif       
       !
       ! Read directional space
       !
       call prop_get_integer(tmp_ptr, '*', 'NDir', dom%ndir)
       if (dom%ndir < 1) then
          write(*,*) 'SWAN_INPUT: invalid number of directions: ', dom%ndir
          goto 999
       endif
       parname = ''
       call prop_get_string(tmp_ptr, '*', 'DirSpace', parname)
       call lowercase(parname, len(parname))
       select case (parname)
       case ('circle')
          dom%dirspace = 1
       case ('sector')
          dom%dirspace = 2
       case default
          if (parname /= '' .or. dom%dirspace < 0) then
             write(*,*) 'SWAN_INPUT: unknown DirSpace: ', parname
             goto 999
          endif
       end select
       !
       if (dom%dirspace == 2) then
          call prop_get_real(tmp_ptr, '*', 'StartDir', dom%startdir)
          call prop_get_real(tmp_ptr, '*', 'EndDir'  , dom%enddir)
       endif
       !
       ! Read modelled frequency range
       !
       call prop_get_integer(tmp_ptr, '*', 'NFreq', dom%nfreq)
       if (dom%nfreq < 1) then
          write(*,*) 'SWAN_INPUT: invalid number of frequencies: ', dom%nfreq
          goto 999
       endif
       call prop_get_real(tmp_ptr, '*', 'FreqMin', dom%freqmin)
       call prop_get_real(tmp_ptr, '*', 'FreqMax', dom%freqmax)
       !
       ! Read in which domain this domain is nested
       !
       call prop_get_integer(tmp_ptr, '*', 'NestedInDomain', dom%nestnr)
       if (domainnr > 1 .and. &
          & (dom%nestnr<1 .or. dom%nestnr>=domainnr)) then
          write(*,*) 'SWAN_INPUT: domain', domainnr, ' not nested in a valid domain'
          goto 999
       endif
       dom%nesfil(1:4) = 'NEST'
       write(dom%nesfil(5:7),'(I3.3)') domainnr
       !
       ! Verify whether quantities should be mapped and optionally be extended
       ! for the current grid.
       !
       call prop_get_integer(tmp_ptr, '*', 'FlowBedLevel'  , dom%qextnd(q_bath))
       call prop_get_integer(tmp_ptr, '*', 'FlowWaterLevel', dom%qextnd(q_wl)  )
       call prop_get_integer(tmp_ptr, '*', 'FlowVelocity'  , dom%qextnd(q_cur) )
       call prop_get_integer(tmp_ptr, '*', 'FlowWind'      , dom%qextnd(q_wind))
       !
       if (dom%qextnd(q_bath)>0) sr%swmor   = .true.
       if (dom%qextnd(q_wl  )>0) sr%swwlt   = .true.
       if (dom%qextnd(q_cur )>0) sr%swuvt   = .true.
       if (dom%qextnd(q_wind)>0) sr%swwindt = .true.
       !
       if (sr%swuvt) then
          parname = ''
          call prop_get_string (tmp_ptr, '*', 'FlowVelocityType', parname)
          call lowercase(parname, len(parname))
          select case (parname)
          case ('depth-averaged')
             dom%flowVelocityType = FVT_DEPTH_AVERAGED
          case ('surface-layer')
             dom%flowVelocityType = FVT_SURFACE_LAYER
          case ('wave-dependent')
             dom%flowVelocityType = FVT_WAVE_DEPENDENT
          case (' ')
             !
             ! Default value used
             !
          case default
             write(*,'(a,i0,a)') 'SWAN_INPUT: invalid option for [Domain ', domainnr, '], FlowVelocityType'
             goto 999
          end select
          !
          ! echo to screen
          !
          select case(dom%flowVelocityType)
          case (FVT_DEPTH_AVERAGED)
             write(*,*) 'Domain ', domainnr, ' is using depth averaged flow velocity.'
          case (FVT_SURFACE_LAYER)
             write(*,*) 'Domain ', domainnr, ' is using surface layer flow velocity.'
          case (FVT_WAVE_DEPENDENT)
             write(*,*) 'Domain ', domainnr, ' is using wave dependent flow velocity.'
          case default
             ! nothing
          end select
       endif
       !
       ! Verify whether output to wavm-file should be written for this domain
       ! (default true)
       !
       call prop_get_logical(tmp_ptr, '*', 'Output', dom%cgnum)
       !
       ! Count number of meteofiles in current group Domain
       !
       dom%n_meteofiles_dom = 0
       do ii = 1, size(tmp_ptr%child_nodes)
          dom_ptr => tmp_ptr%child_nodes(ii)%node_ptr
          parname = tree_get_name( dom_ptr )
          if (parname == 'meteofile') then
             dom%n_meteofiles_dom = dom%n_meteofiles_dom + 1
          endif
       enddo
       !
       if (dom%n_meteofiles_dom  > 0) then
          !
          ! Allocate temporary array for meteofiles
          !
          allocate(tmp_meteofile(dom%n_meteofiles_dom), stat = istat)
          tmp_meteofile = ''
          !
          ! The input can be read
          !
          par = 0
          do j = 1, size(tmp_ptr%child_nodes)
             !
             ! Does tmp_ptr contain one or more children with name MeteoFile?
             !
             node_ptr => tmp_ptr%child_nodes(j)%node_ptr
             !
             parname = ''
             call prop_get_string(node_ptr, '*', 'MeteoFile', parname)
             if ( parname /= '') then
                par = par + 1
                !
                ! Read value for keyword meteofile
                !
                call tree_get_data_string(node_ptr, parname, success)
                !
                ! Check double occurrences
                !
                do ii = 1, par
                   if (tmp_meteofile(ii) == parname) then
                      write(*,'(a,a,a)') 'SWAN input: Group Domain: MeteoFile ', trim(parname), ' has already been read'
                      par = par - 1
                      dom%n_meteofiles_dom = dom%n_meteofiles_dom - 1
                      exit
                   endif
                enddo
                !
                if (tmp_meteofile(par) == '') then
                   !
                   ! Array location for MeteoFile empty, so store the read item in the temporary meteofile array
                   !
                   tmp_meteofile(par) = trim(parname)
                endif
                if (par == dom%n_meteofiles_dom) then
                   !
                   ! Correct number of meteofiles read
                   !
                   exit
                endif
             endif
          enddo
          !
          ! Allocate array for meteofiles in group Domain
          !
          allocate (dom%meteofile_dom(dom%n_meteofiles_dom), stat = istat)
          !
          ! Fill the array with meteofiles in group Domain
          !
          dom%meteofile_dom(1:dom%n_meteofiles_dom) = tmp_meteofile(1:dom%n_meteofiles_dom)
          sr%swwindt = .true.
          !
          deallocate(tmp_meteofile)
          !
       endif
       !
       ! Check whether also global meteofiles were provided.
       ! If so, the meteofile(s) specified in the DOMAIN group are used.
       !
       if     (sr%n_meteofiles_gen > 0 .and. dom%n_meteofiles_dom > 0) then
          write(*, '(a,i0)') 'SWAN_INPUT: Meteofiles specified in group Domain used instead of meteofiles specified in group General for domain ', domainnr
       elseif (sr%n_meteofiles_gen > 0 .and. dom%n_meteofiles_dom == 0) then
          !
          ! Meteofiles specified in group general will be used for this domain
          !
          dom%n_meteofiles_dom = sr%n_meteofiles_gen
          !
          ! Allocate array for meteofiles in group Domain
          !
          allocate (dom%meteofile_dom(dom%n_meteofiles_dom), stat = istat)
          !
          ! Fill the array with meteofiles in group Domain
          !
          dom%meteofile_dom = sr%meteofile_gen 
          sr%swwindt        = .true.
          write(*, '(a,i0)') 'SWAN_INPUT: Meteofiles specified in group General used for domain ', domainnr
       endif
       if (dom%n_meteofiles_dom > 0 .and. dom%qextnd(q_wind) > 0) then
          !
          ! User specified wind to be read from COM-file (from FLOW simulation),
          ! but user also specified wind via one or more meteofiles.
          ! The wind from the meteofiles will be used.
          ! These will cover the whole WAVE domain for sure.
          !
          write(*, '(a,i0)') 'SWAN_INPUT: Meteofiles specified in group Domain used instead of meteo input from FLOW for domain ', domainnr
          dom%qextnd(q_wind) = 0
       endif
       !       
    enddo
    !
    if (sr%swwindt) then
       !
       ! switch on varying wind (varwin) on curvilinear grid (curviwind)
       ! set exclusion value on the default one (excval)
       !
       sr%varwin     = .true.
       sr%curviwind  = .true.
       sr%excval     = -999.0
    endif
    !
    ! determine whether flow data is used
    !
    sr%useflowdata = sr%swmor .or. sr%swwlt .or. sr%swuvt .or. (sr%swwindt .and. dom%n_meteofiles_dom == 0) 
    !
    ! determine the number of boundaries
    !
    nbound = 0
    do i = 1, size(mdw_ptr%child_nodes)
       tmp_ptr => mdw_ptr%child_nodes(i)%node_ptr
       parname = tree_get_name( tmp_ptr )
       if ( parname == 'boundary') nbound = nbound + 1
    enddo
    sr%nbound = nbound
    !
    istat = 0
    allocate (sr%bnd(nbound  ), stat = istat)
    if (istat /= 0) then
       write(*,*) 'SWAN_INPUT: memory alloc error (nbound)'
       goto 999
    endif
    !
    do i = 1, nbound
       bnd => sr%bnd(i)
       !
       bnd%parread    = -999
       bnd%sshape     = -999
       bnd%periodtype = -999
       bnd%dsprtype   = -999
       bnd%bndtyp     = -999
       bnd%orient     = -999
       bnd%turn       = -999
       bnd%convar     = -999
       bnd%nsect      = -999
       bnd%bndcrd_mn  = -999
       bnd%bndcrd_xy  = -999.0
       bnd%gamma0     = 3.3
       bnd%sigfr      = -999.0
       bnd%name       = ' '
       sr%specfile    = ' '
       nullify(bnd%distance)
       nullify(bnd%waveheight)
       nullify(bnd%period)
       nullify(bnd%direction)
       nullify(bnd%dirspread)
    enddo
    !
    ! Optionally find these general quantities in the tables
    !
    if (sr%timedependent) then
       call gettable(sr%tseriesfile, 'General', 'WaveHeight', def_ts_hs, &
                   & 0     , errorstring)
       if (def_ts_hs(3) > 1) then
          write(*,*) 'SWAN_INPUT: too many WaveHeight entries in TSeriesFile'
          goto 999
       endif
       !
       call gettable(sr%tseriesfile, 'General', 'Period', def_ts_tp, &
                   & 0     , errorstring)
       if (def_ts_tp(3) > 1) then
          write(*,*) 'SWAN_INPUT: too many Period entries in TSeriesFile'
          goto 999
       endif
       !
       call gettable(sr%tseriesfile, 'General', 'Direction', def_ts_wd, &
                   & 0     , errorstring)
       if (def_ts_wd(3) > 1) then
          write(*,*) 'SWAN_INPUT: too many Direction entries in TSeriesFile'
          goto 999
       endif
       !
       call gettable(sr%tseriesfile, 'General', 'DirSpreading', def_ts_ds, &
                   & 0     , errorstring)
       if (def_ts_ds(3) > 1) then
          write(*,*) 'SWAN_INPUT: too many DirSpreading entries in TSeriesFile'
          goto 999
       endif
       !
    endif
    !
    boundnr = 0
    do i = 1, size(mdw_ptr%child_nodes)
       bnd_ptr => mdw_ptr%child_nodes(i)%node_ptr
       parname = tree_get_name( bnd_ptr )
       if ( parname /= 'boundary') cycle
       boundnr = boundnr + 1
       bnd => sr%bnd(boundnr)
       !
       call prop_get_string(bnd_ptr, '*', 'Name', bnd%name)
       !
       parname = ''
       call prop_get_string(bnd_ptr, '*', 'Definition', parname)
       call lowercase(parname,len(parname))
       select case (parname)
       case ('orientation')
          bnd%bndtyp = 1
          !
          parname = ''
          call prop_get_string(bnd_ptr, '*', 'Orientation' , parname)
          call lowercase(parname,len(parname))
          select case (parname)
          case ('n','north')
             bnd%orient = 1
          case ('nw','northwest')
             bnd%orient = 2
          case ('w','west')
             bnd%orient = 3
          case ('sw','southwest')
             bnd%orient = 4
          case ('s','south')
             bnd%orient = 5
          case ('se','southeast')
             bnd%orient = 6
          case ('e','east')
             bnd%orient = 7
          case ('ne','northeast')
             bnd%orient = 8
          case default
             write(*,*) 'SWAN_INPUT: missing or invalid boundary orientation'
             goto 999
          end select
          bnd%turn = 1
          !
          parname = ''
          call prop_get_string(bnd_ptr, '*', 'DistanceDir'   , parname)
          call lowercase(parname,len(parname))
          select case (parname)
          case ('clockwise')
             bnd%turn = 0
          case ('counter','counter-clockwise',' ')
             bnd%turn = 1
          case default
             write(*,*) 'SWAN_INPUT: invalid distance measurement direction'
             goto 999
          end select
          !
       case ('grid','grid-coordinates')
          bnd%bndtyp = 2
          !
          call prop_get_integer(bnd_ptr, '*', 'StartCoordM' , bnd%bndcrd_mn(1))
          call prop_get_integer(bnd_ptr, '*', 'StartCoordN' , bnd%bndcrd_mn(2))
          call prop_get_integer(bnd_ptr, '*', 'EndCoordM'   , bnd%bndcrd_mn(3))
          call prop_get_integer(bnd_ptr, '*', 'EndCoordN'   , bnd%bndcrd_mn(4))
          !
       case ('xy','xy-coordinates')
          bnd%bndtyp = 3
          !
          call prop_get_real(bnd_ptr, '*', 'StartCoordX' , bnd%bndcrd_xy(1))
          call prop_get_real(bnd_ptr, '*', 'StartCoordY' , bnd%bndcrd_xy(2))
          call prop_get_real(bnd_ptr, '*', 'EndCoordX'   , bnd%bndcrd_xy(3))
          call prop_get_real(bnd_ptr, '*', 'EndCoordY'   , bnd%bndcrd_xy(4))
          !
       case ('fromsp2file')
          bnd%bndtyp = 4
          !
          call prop_get_string(bnd_ptr, '*', 'OverallSpecfile' , sr%specfile)
          write(*,*) 'Boundary conditions from overall 2D spectra file'
          cycle
          !      
       case ('fromwwfile')
          bnd%bndtyp = 5
          !
          call prop_get_string(bnd_ptr, '*', 'WWspecfile' , sr%specfile)
          write(*,*) 'Boundary conditions from WAVEWATCH III spectra file'
          cycle
          !
       case default
          write(*,*) 'SWAN_INPUT: missing or invalid boundary orientation definition type'
          goto 999
       end select
       !
       parname = ''
       call prop_get_string(bnd_ptr, '*', 'SpectrumSpec'   , parname)
       call lowercase(parname,len(parname))
       select case (parname)
       case ('from file')
          bnd%parread = 1
          !
          bnd%sshape    = 1 ! jonswap
          bnd%periodtype= 1 ! peak
          bnd%dsprtype  = 1 ! power
       case ('parametric')
          bnd%parread = 2
          !
          parname = ''
          call prop_get_string(bnd_ptr, '*', 'SpShapeType'   , parname)
          call lowercase(parname,len(parname))
          select case (parname)
          case ('jonswap')
            bnd%sshape = 1
          case ('pm','pierson-moskowitz')
            bnd%sshape = 2
          case ('gauss')
            bnd%sshape = 3
          case default
             write(*,*) 'SWAN_INPUT: missing or invalid boundary spectrum shape type'
             goto 999
          end select
          !
          parname = ''
          call prop_get_string(bnd_ptr, '*', 'PeriodType'   , parname)
          call lowercase(parname,len(parname))
          select case (parname)
          case ('peak')
            bnd%periodtype = 1
          case ('mean')
            bnd%periodtype = 2
          case default
             write(*,*) 'SWAN_INPUT: missing or invalid boundary spectrum period type'
             goto 999
          end select
          !
          parname = ''
          call prop_get_string(bnd_ptr, '*', 'DirSpreadType'   , parname)
          call lowercase(parname,len(parname))
          select case (parname)
          case ('power')
            bnd%dsprtype = 1
          case ('degrees')
            bnd%dsprtype = 2
          case default
             write(*,*) 'SWAN_INPUT: missing or invalid boundary spectrum directional spreading type'
             goto 999
          end select
          !
       case default
          write(*,*) 'SWAN_INPUT: missing or invalid boundary spectrum specification type'
          goto 999
       end select
       !
       select case (bnd%sshape)
       case (1) ! jonswap
          call prop_get_real(bnd_ptr, '*', 'PeakEnhanceFac'   , bnd%gamma0)
          if (boundnr == 1) then
             ! use this gamm0 instead of the default value
             sr%gamma0 = bnd%gamma0
          endif
       case (3) ! gauss
          call prop_get_real(bnd_ptr, '*', 'GaussSpread'     , bnd%sigfr)
       end select
       !
       ! Determine the number of boundary sections
       !
       nsect = 0
       do j = 1,size(bnd_ptr%child_nodes)
          tmp_ptr => bnd_ptr%child_nodes(j)%node_ptr
          parname = tree_get_name( tmp_ptr )
          if ( parname == 'condspecatdist') nsect = nsect+1
       enddo
       bnd%nsect  = nsect
       bnd%convar = 2
       if (nsect==0) then
          !
          ! uniform condition
          !
          nsect      = nsect+1
          bnd%convar = 1
       endif
       !
       istat = 0
                     allocate (bnd%distance  (nsect ), stat = istat)
       if (istat==0) allocate (bnd%waveheight(nsect ), stat = istat)
       if (istat==0) allocate (bnd%period    (nsect ), stat = istat)
       if (istat==0) allocate (bnd%direction (nsect ), stat = istat)
       if (istat==0) allocate (bnd%dirspread (nsect ), stat = istat)
       if (istat==0) allocate (bnd%spectrum  (nsect ), stat = istat)
       if (istat/=0) then
          write(*,*) 'SWAN_INPUT: memory alloc error (nsect)'
          goto 999
       endif
       bnd%distance  = -999.0
       bnd%waveheight= -999.0
       bnd%period    = -999.0
       bnd%direction = -999.0
       bnd%dirspread = -999.0
       bnd%spectrum  = ' '
       !
       sectnr = 0
       if (bnd%nsect==0) then
          !
          ! uniform condition
          !
          sectnr = 1
       endif
       do j = 1,size(bnd_ptr%child_nodes)
          tmp_ptr => bnd_ptr%child_nodes(j)%node_ptr
          parname = tree_get_name( tmp_ptr )
          select case (parname)
          case ('condspecatdist')
             sectnr = sectnr+1
             call prop_get_real(tmp_ptr, '*', 'CondSpecAtDist', bnd%distance(sectnr))
          case ('waveheight')
             if (sectnr==0) then
                write(*,*) 'SWAN_INPUT: premature wave height specification at ',trim(bnd%name)
                goto 999
             endif
             call prop_get_real(tmp_ptr, '*', 'WaveHeight', bnd%waveheight(sectnr))
          case ('period')
             if (sectnr==0) then
                write(*,*) 'SWAN_INPUT: premature period specification at ',trim(bnd%name)
                goto 999
             endif
             call prop_get_real(tmp_ptr, '*', 'Period', bnd%period(sectnr))
          case ('direction')
             if (sectnr==0) then
                write(*,*) 'SWAN_INPUT: premature direction specification at ',trim(bnd%name)
                goto 999
             endif
             call prop_get_real(tmp_ptr, '*', 'Direction', bnd%direction(sectnr))
          case ('dirspreading')
             if (sectnr==0) then
                write(*,*) 'SWAN_INPUT: premature direction spreading specification at ',trim(bnd%name)
                goto 999
             endif
             call prop_get_real(tmp_ptr, '*', 'DirSpreading', bnd%dirspread(sectnr))
          case ('spectrum')
             if (sectnr==0) then
                write(*,*) 'SWAN_INPUT: premature spectrum file specification at ',trim(bnd%name)
                goto 999
             endif
             call prop_get_string(tmp_ptr, '*', 'Spectrum', bnd%spectrum(sectnr))
          end select
       enddo
       !
       ! Optionally find these general quantities in the tables
       !
       if (sr%timedependent) then
          call gettable(sr%tseriesfile, bnd%name, 'WaveHeight', bnd%ts_hs, &
                      & 0     , errorstring)
          if (bnd%ts_hs(1)<0) bnd%ts_hs = def_ts_hs
          if (bnd%ts_hs(3)>1 .and. bnd%ts_hs(3)/=bnd%nsect) then
             write(*,*) 'SWAN_INPUT: invalid number of WaveHeight entries in TSeriesFile'
             goto 999
          endif
          !
          call gettable(sr%tseriesfile, bnd%name, 'Period', bnd%ts_tp, &
                      & 0     , errorstring)
          if (bnd%ts_tp(1)<0) bnd%ts_tp = def_ts_tp
          if (bnd%ts_tp(3)>1 .and. bnd%ts_tp(3)/=bnd%nsect) then
             write(*,*) 'SWAN_INPUT: invalid number of Period entries in TSeriesFile'
             goto 999
          endif
          !
          call gettable(sr%tseriesfile, bnd%name, 'Direction', bnd%ts_wd, &
                      & 0     , errorstring)
          if (bnd%ts_wd(1)<0) bnd%ts_wd = def_ts_wd
          if (bnd%ts_wd(3)>1 .and. bnd%ts_wd(3)/=bnd%nsect) then
             write(*,*) 'SWAN_INPUT: invalid number of Direction entries in TSeriesFile'
             goto 999
          endif
          !
          call gettable(sr%tseriesfile, bnd%name, 'DirSpreading', bnd%ts_ds, &
                      & 0     , errorstring)
          if (bnd%ts_ds(1)<0) bnd%ts_ds = def_ts_ds
          if (bnd%ts_ds(3)>1 .and. bnd%ts_ds(3)/=bnd%nsect) then
             write(*,*) 'SWAN_INPUT: invalid number of DirSpreading entries in TSeriesFile'
             goto 999
          endif
          !
       endif
       !
    enddo
    !
    ! Determine the number of obstacles
    !
    if (obstfil /= ' ') then
       call tree_create_node( mdw_ptr, 'Obstacle Input', obst_ptr )
       call tree_put_data( obst_ptr, transfer(trim(obstfil),node_value), 'STRING' )
       call prop_file('ini',trim(obstfil),obst_ptr,istat)
       if (istat /= 0) then
          write(*,*) 'SWAN_INPUT: error reading obstacle file ''', trim(obstfil), ''''
          goto 999
       endif
       !
       call tree_get_node_by_name( obst_ptr, 'ObstacleFileInformation', tmp_ptr )
       call tree_get_node_by_name( tmp_ptr, 'PolylineFile', pol_ptr )
       if (.not.associated (pol_ptr)) then
          write(*,*) 'SWAN_INPUT: missing PolylineFile keyword in obstacle file'
          goto 999
       endif
       call tree_get_data_string(pol_ptr,polylinefile,flag)
       call prop_file('tekal',polylinefile,pol_ptr,istat)
       if (istat /= 0) then
          write(*,*) 'SWAN_INPUT: error reading obstacle polygon file ''', trim(polylinefile), ''''
          goto 999
       endif
       !
       nobst    = 0
       nobstpnt = 0
       do i = 1, size(obst_ptr%child_nodes)
          tmp_ptr => obst_ptr%child_nodes(i)%node_ptr
          parname = tree_get_name( tmp_ptr )
          if ( parname /= 'obstacle') cycle
          !
          nobst   = nobst + 1
          parname = ''
          call prop_get_string (tmp_ptr, '*', 'Name' , parname)
          call tree_get_node_by_name(pol_ptr, parname, tmp_ptr )
          if ( .not. associated(tmp_ptr) ) then
             write(*,*) 'SWAN_INPUT: obstacle polygon ''', trim(parname), ''' not found'
             goto 999
          endif
          !
          call tree_get_data_ptr( tmp_ptr, data_ptr, parname)
          nobstpnt = nobstpnt + transfer( data_ptr, nobstpnt )
       enddo
       !
    else
       nobst    = 0
       nobstpnt = 0
    endif
    sr%nobst = nobst
    sr%nscr  = nobstpnt
    !
    istat = 0
                  allocate (sr%trane     (nobst   ), stat = istat)
    if (istat==0) allocate (sr%f         (nobst   ), stat = istat)
    if (istat==0) allocate (sr%obet      (nobst   ), stat = istat)
    if (istat==0) allocate (sr%ogam      (nobst   ), stat = istat)
    if (istat==0) allocate (sr%reflection(nobst   ), stat = istat)
    if (istat==0) allocate (sr%refl_type (nobst   ), stat = istat)
    if (istat==0) allocate (sr%refl_coeff(nobst   ), stat = istat)
    if (istat==0) allocate (sr%nlin      (nobst   ), stat = istat)
    if (istat==0) allocate (sr%xpob      (nobstpnt), stat = istat)
    if (istat==0) allocate (sr%ypob      (nobstpnt), stat = istat)
    !
    if (istat/=0) then
       write(*,*) 'SWAN_INPUT: memory alloc error (nobst)'
       goto 999
    endif
    !
    if (nobst > 0) then
       obstnr  = 0
       obstpnt = 0
       do i = 1, size(obst_ptr%child_nodes)
          tmp_ptr => obst_ptr%child_nodes(i)%node_ptr
          parname = tree_get_name( tmp_ptr )
          if ( parname /= 'obstacle') cycle
          obstnr  = obstnr + 1
          !
          parname = ''
          call prop_get_string (tmp_ptr, '*', 'Type' , parname)
          call lowercase(parname,len(parname))
          select case (parname)
          case ('sheet')
             call prop_get_real(tmp_ptr, '*', 'TransmCoef'   , sr%trane(obstnr))
          case ('dam')
             sr%trane(obstnr) = 999.9
             call prop_get_real(tmp_ptr, '*', 'Height'       , sr%f(obstnr))
             call prop_get_real(tmp_ptr, '*', 'Alpha'        , sr%ogam(obstnr))
             call prop_get_real(tmp_ptr, '*', 'Beta'         , sr%obet(obstnr))
          end select
          !
          parname = ''
          call prop_get_string (tmp_ptr, '*', 'Reflections' , parname)
          call lowercase(parname,len(parname))
          select case (parname)
          case ('no')
             sr%reflection(obstnr) = 0
             sr%refl_type(obstnr) = 0
          case ('specular')
             sr%reflection(obstnr) = 1
             sr%refl_type(obstnr) = 1
          case ('diffuse')
             sr%reflection(obstnr) = 1
             sr%refl_type(obstnr) = 2
          end select
          !
          if (sr%refl_type(obstnr)>0) then
             call prop_get_real(tmp_ptr, '*', 'ReflecCoef'   , sr%refl_coeff(obstnr))
          endif
          !
          parname = ''
          call prop_get_string (tmp_ptr, '*', 'Name' , parname)
          call tree_get_node_by_name(pol_ptr, parname, tmp_ptr )
          if ( .not. associated(tmp_ptr) ) then
             write(*,*) 'SWAN_INPUT: obstacle polygon not found'
             goto 999
          endif
          !
          ! get x,y coordinates ...
          !
          call tree_get_data_ptr( tmp_ptr, data_ptr, parname)
          sr%nlin(obstnr) = transfer( data_ptr, nobstpnt )
          !
          do io = 1, sr%nlin(obstnr)
             obstpnt = obstpnt+1
             write (parname,'(a,i0)')'row_',io
             call tree_get_node_by_name( tmp_ptr, parname, node_ptr )
             call tree_get_data_ptr( node_ptr, data_ptr, parname )
             !
             xy = transfer( data_ptr, xy(1), 2 )
             sr%xpob(obstpnt) = xy(1)
             sr%ypob(obstpnt) = xy(2)
          enddo
       enddo
    endif
    !
    write(*,*) 'Done reading input'
    !
    return
999 continue
    stop
end subroutine read_keyw_mdw
!
!
!==============================================================================
subroutine read_swan_mdw(casl      ,wavedata  , &
                       & swmor     ,swwlt     ,swuvt     , &
                       & swwav     ,swuvi     ,corht     ,curvi     , &
                       & swbot     ,swflux    ,filcom    ,rgfout    ,prname    , &
                       & prnumb    ,title1    ,title2    ,title3    , &
                       & nnest     ,nttide    ,itest     ,itrace    , &
                       & zeta      ,ux0       ,uy0       ,css       ,cdd       , &
                       & grav      ,rho       , &
                       & timwav    ,wfil      ,nobst     ,nscr      , &
                       & xw        ,yw        ,alpw      ,mxw       ,myw       , &
                       & dxw       ,dyw       ,trane     ,f         ,ogam      , &
                       & obet      ,xpob      ,ypob      ,nlin      ,varwin    , &
                       & ncurv     ,ncrv      ,nclin     ,xpcu      ,ypcu      , &
                       & ncrp      ,filnam    ,error     ,inrhog    , &
                       & mxr       ,myr       ,ffil      , &
                       & maxsteps  ,maxobst   ,maxcurv   ,sr        )
   use read_grids
   !
   implicit none
!
! Global variables
!
    integer                                      :: maxsteps
    integer                                      :: maxobst
    integer                                      :: maxcurv
    integer                                      :: error
    integer                         ,intent(out) :: inrhog
    integer                         ,intent(out) :: itest
    integer                         ,intent(out) :: itrace
    integer                         ,intent(out) :: mxw
    integer                         ,intent(out) :: myw
    integer                         ,intent(out) :: mxr
    integer                         ,intent(out) :: myr
    integer                                      :: ncrp
    integer                         ,intent(out) :: ncrv
    integer                                      :: ncurv
    integer                                      :: nnest
    integer                                      :: nobst
    integer                         ,intent(out) :: nscr
    integer                                      :: nttide
    integer, dimension(maxcurv)                  :: nclin
    integer, dimension(maxobst)                  :: nlin
    logical                         ,intent(out) :: corht
    logical                         ,intent(out) :: curvi
    logical                         ,intent(out) :: swbot
    logical                         ,intent(out) :: swflux
    logical                         ,intent(out) :: swmor
    logical                         ,intent(out) :: swuvi
    logical                         ,intent(out) :: swuvt
    logical                         ,intent(out) :: swwav
    logical                         ,intent(out) :: swwlt
    logical                                      :: varwin
    real                            ,intent(out) :: alpw
    real                            ,intent(out) :: cdd
    real                            ,intent(out) :: css
    real                            ,intent(out) :: dxw
    real                            ,intent(out) :: dyw
    real                                         :: grav
    real                            ,intent(out) :: rho
    real                            ,intent(out) :: xw
    real                            ,intent(out) :: yw
    real, dimension(maxobst)        ,intent(out) :: f
    real, dimension(maxobst)        ,intent(out) :: obet
    real, dimension(maxobst)        ,intent(out) :: ogam
    real, dimension(maxsteps)       ,intent(out) :: zeta
    real, dimension(maxsteps)       ,intent(out) :: timwav
    real, dimension(maxobst)        ,intent(out) :: trane
    real, dimension(maxsteps)       ,intent(out) :: ux0
    real, dimension(maxsteps)       ,intent(out) :: uy0
    real, dimension(maxcurv)        ,intent(out) :: xpcu
    real, dimension(maxobst)        ,intent(out) :: xpob
    real, dimension(maxcurv)        ,intent(out) :: ypcu
    real, dimension(maxobst)        ,intent(out) :: ypob
    character(*)                                 :: casl
    character(4)                    ,intent(out) :: prnumb
    character(16)                   ,intent(out) :: prname
    character(*)                    ,intent(out) :: filcom
    character(37)                   ,intent(out) :: rgfout
    character(37)                                :: wfil
    character(37)                                :: ffil
    character(72)                   ,intent(out) :: title1
    character(72)                   ,intent(out) :: title2
    character(72)                   ,intent(out) :: title3
    character(*)                    ,intent(in)  :: filnam
    type(swan)                                   :: sr
    type(wave_data_type)                         :: wavedata
!
! Local variables
!
    integer           :: bndtyp
    integer           :: breaking
    integer           :: carn
    integer           :: cindx
    integer           :: compmode
    integer           :: convar
    integer           :: fshift
    integer           :: gridtype
    integer           :: hotfile
    integer           :: i
    integer           :: ierr
    integer           :: in
    integer           :: ind
    integer           :: ipfl
    integer           :: it
    integer           :: iuni
    integer           :: j
    integer           :: k
    integer           :: l
    integer           :: lunsm
    integer           :: nbound
    integer           :: nextnd
    integer           :: non_stationary ! read from mdw-file: 0: stationary, 1: non-stationary
    integer           :: npoints
    integer           :: nsect
    integer           :: obstyp
    integer           :: orient
    integer           :: parread
    integer           :: pnts
    integer           :: quad
    integer           :: rccnt
    integer           :: refdate
    integer           :: refrac
    integer           :: rindx
    integer           :: setup
    integer           :: spec1d
    integer           :: spec2d
    integer           :: swdiss
    integer           :: swmr
    integer           :: swou
    integer           :: swut
    integer           :: swwt
    integer           :: swwv
    integer           :: swwnd
    integer           :: table
    integer           :: triads
    integer           :: windgrowth
    integer           :: windtype
    integer, external :: new_lun
    integer, external :: skcomc
    logical           :: exists
    character(7)      :: vers
    character(20)     :: bndnam
    character(120)    :: line
    character(256)    :: message
    type(swan_bnd), pointer :: bnd
    type(swan_dom), pointer :: dom
!
!! executable statements -------------------------------------------------------
!
    error = 0
    rccnt = 0
    vers  = ' '
    iuni  = new_lun()
    message = filnam
    open (iuni, file = filnam, status = 'old', err = 1001)
    rewind (iuni, err = 1002)
    !
    !  General information
    !
    !  Switches
    !
    !     SWMOR   bottom from MORFO
    !     SWBOT   bottom/current from BOTTOM/CURREN file(s)
    !     SWWLT   water level from TRISU
    !     SWUVT   velocity from TRISU
    !     SWUVI   velocity from input (constant over field)
    !     SWWINDT wind field from TRISU
    !     SWWAV   wave data to WAVE
    !     SWDIS   wave forces from dissipation
    !     CORHT   correction HISWA wave height, period (CORRHT)
    !     SWOUT   store HISWA output
    !     CURVI   curvi linear FLOW grid
    !
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!------version number WAVE-GUI-------------------------------
    read (iuni,'(A)')  line
    i     = index (line,'version')
    read (line(i+8:),'(a)') vers
    !
    ! Checking on versionnumber is done on character base (as in WAVE-GUI)
    ! This can be disturbed when the strings contains trailing spaces
    ! Clean up vers, such that it has the format '1.11.11'
    ! Do not bother about wrongly placed dots or digits
    !
    vers = adjustl(vers)
    do j=1,len(vers)
       select case(vers(j:j))
       case(' ')
          if (j==2 .or. j==5) then
             vers(j:j) = '.'
          else
             vers(j:j) = '0'
          endif
       case('.')
          ! nothing
       case('0':'9')
          ! nothing
       case default
          write (*,'(a)') '*** ERROR: Unable to read WAVE-GUI version number'
          goto 1002
       end select
    enddo
    if (i==0 .or. llt(vers,'4.87.00')) then
       write (*,'(3a)') '*** ERROR: mdw_file created with WAVE-GUI version ', &
            & trim(vers), ' is not supported.'
       goto 1002
    endif
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!------SWAN version number------------------------------------
    if (lge(vers, '4.91.00')) then
       !
       ! SWAN version number is removed
       !
    else
       read (iuni,'(1X)')
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
    endif
!------General project data-----------------------------------
    read (iuni, *, err = 1002, end = 1009) prname
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
    read (iuni, *, err = 1002, end = 1009) prnumb
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
    read (iuni, *, err = 1002, end = 1009) title1
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
    read (iuni, *, err = 1002, end = 1009) title2
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
    read (iuni, *, err = 1002, end = 1009) title3
    !
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---Parameters from FLOW: bathymetry, waterlevel, current, wind---
    swmr  = 0
    swwt  = 0
    swut  = 0
    swwnd = 0
    if (lge(vers, '4.90.06')) then
       read (iuni, *, err = 1002, end = 1009) swmr, swwt, swut, swwnd
    else
       read (iuni, *, err = 1002, end = 1009) swmr, swwt, swut
    endif
    swbot      = .true.
    swmor      = .false.
    swwlt      = .false.
    swuvt      = .false.
    sr%swwindt = .false.
    swuvi      = .true.
    swflux     = .true.
    corht      = .false.
    curvi      = .true.
    if (swmr  == 1) swmor      = .true.
    if (swwt  == 1) swwlt      = .true.
    if (swut  == 1) swuvt      = .true.
    if (swwnd == 1) then
       sr%swwindt = .true.
       !
       ! switch on varying wind (varwin) on curvilinear grid (curviwind)
       ! set exclusion value on the default one (excval)
       !
       varwin       = .true.
       sr%curviwind = .true.
       sr%excval    = -999.0
    endif
    if (swmor .or. swwlt .or. swuvt .or. sr%swwindt) then
       !
       ! FLOW data is used
       !
       sr%useflowdata = .true.
    else
       sr%useflowdata = .false.
    endif
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---number of nested computations, including first one--------
    read (iuni, *, err = 1002, end = 1009) nnest
    mxr = 0
    myr = 0
    do in = 1, nnest
       dom => sr%dom(in)
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
!------filename comp. grid------------------------------------
       read (iuni, *, err =1002, end = 1009) dom%curlif
       call readgriddims(dom%curlif, dom%mxc, dom%myc)
       if (dom%mxc * dom%myc> mxr*myr) then
          mxr = dom%mxc
          myr = dom%myc
       endif
       !
       ! poles? no, fences!
       !
       dom%mxc = dom%mxc-1
       dom%myc = dom%myc-1
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
!------depth comp. gird-file name bottom grid----------------
       read (iuni, *, err =1002, end = 1009) dom%curvibot,line
       if (dom%curvibot==1) then
          dom%mxb    = dom%mxc
          dom%myb    = dom%myc
          dom%depfil = dom%curlif
       else
          if (swmor) then
             write (*,'(a)')    '*** ERROR: Bathymetry specified on rectilinear grid'
             write (*,'(11x,a,i5)')        'Line ',rccnt
             write (*,'(11x,a)')           'This option is only supported when ''Bathymetry from FLOW'' is not used'
             close(iuni)
             stop
          endif
          read(line,*, err =1002, end = 1009) dom%depfil
          call readgriddims(dom%depfil, dom%mxb, dom%myb)
          !
          ! poles? no, fences!
          !
          dom%mxb = dom%mxb-1
          dom%myb = dom%myb-1
       endif
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
!------filename depth file------------------------------------
       dom%botfil = ' '
       read (iuni, *, err =1002, end = 1009) dom%botfil
       if (dom%botfil == ' ') then
          write (*,'(3a)')   '*** ERROR: Bathymetry file not specified in file ''',trim(filnam),''','
          write (*,'(a,i5)') '           Line ',rccnt
          error = 4
          goto 1010
       endif
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
!------directional space(c/s)--numb. of dir.--start,end dir.--
       read (iuni, *, err = 1002, end = 1009) dom%dirspace, &
                    & dom%ndir, dom%startdir, dom%enddir
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
!------flow--fhigh--numb. of freq.--nesting grid number-output
       read (iuni, *, err = 1002, end =1009)  dom%freqmin, &
                    & dom%freqmax, dom%nfreq, dom%nestnr, swou
       dom%nesnam = ' '
       dom%nesnam(1:4) = 'NEST'
       write (dom%nesnam(5:7),'(I3.3)') in
       dom%nesfil      = dom%nesnam
       if (swou>0) then
          dom%cgnum = .true.
       else
          dom%cgnum = .false.
       endif
       !
       dom%qextnd = 0
    enddo
!---tidal information
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---number of tide steps ntide--reference date----------------
    if (lge(vers, '4.89.05')) then
       read (iuni, *, err = 1002, end = 1009) nttide, line
       !
       ! line contains reference date in the format yyyy-mm-dd
       ! Convert it to yyyymmdd and put it in wavetime
       !
       line      = adjustl(line)
       line(5:5) = line(6:6)
       line(6:6) = line(7:7)
       line(7:7) = line(9:9)
       line(8:8) = line(10:10)
       line(9:)  = ' '
       read (line, *, err = 1002, end = 1009) refdate
       call setrefdate(wavedata%time, refdate)
    else
       read (iuni, *, err = 1002, end = 1009) nttide
    endif
!---time--water level--ux--uy---------------------------------
    zeta   = 0.0
    ux0    = 0.0
    uy0    = 0.0
    nextnd = 0
    do it = 1, nttide
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
       read (iuni, *, err = 1002, end = 1009) &
                    & timwav(it), zeta(it), ux0(it), uy0(it)
       !
       ! Old way to switch on data extension (from FLOW to SWAN grid)
       ! If wl/ux/uy = -999.0, the value MUST be set to 0.0,
       ! because the way extension is performed has changed.
       !
       if (zeta(it)<-998.9 .and. zeta(it)>-999.1) then
          nextnd = nnest
          do in = 1, nnest
             sr%dom(in)%qextnd = 1
          enddo
          zeta(it)  = 0.0
       endif
       if (ux0(it)<-998.9 .and. ux0(it)>-999.1) then
          nextnd = nnest
          do in = 1, nnest
             sr%dom(in)%qextnd = 1
          enddo
          ux0(it)   = 0.0
       endif
       if (uy0(it)<-998.9 .and. uy0(it)>-999.1) then
          nextnd = nnest
          do in = 1, nnest
             sr%dom(in)%qextnd = 1
          enddo
          uy0(it)   = 0.0
       endif
    enddo
    !
    if (ux0(1) == 0.0 .and. uy0(1) == 0.0 ) then
       swuvi = .false.
    endif
    !
    if (wavedata%mode /= stand_alone .and. nttide > 1) then
       write(*,'(a)') '  Warning : number of tidal time points must be 1'
       write(*,'(a)') '            (when running online with Delft3D-FLOW)'
       write(*,'(a)') '            value is reset, time 0.0 is used'
       nttide = 1
       timwav(1) = 0.0
    endif
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---level--num grids to extend on--quantities to extend--
    if (lge(vers, '4.90.06')) then
       !
       ! Varying number of input values on the next line:
       ! - Read the line as character string
       ! - Read always present parameters from variable "line"
       ! - If additional parameters are present:
       !   Read always present parameters AND additional parameters from variable "line"
       !
       read (iuni, '(a)', err = 1002, end = 1009) line
       read (line, *, err = 1002, end = 1009) sr%wlevelcorr, nextnd
       if (nextnd > 0) then
          read (line, *, err = 1002, end = 1009) sr%wlevelcorr, nextnd, &
              & sr%dom(nnest)%qextnd(q_bath), &
              & sr%dom(nnest)%qextnd(q_wl)  , &
              & sr%dom(nnest)%qextnd(q_cur) , &
              & sr%dom(nnest)%qextnd(q_wind)
          do in = nnest-nextnd+1, nnest
             dom => sr%dom(in)
             dom%qextnd(q_bath) = sr%dom(nnest)%qextnd(q_bath)
             dom%qextnd(q_wl)   = sr%dom(nnest)%qextnd(q_wl)
             dom%qextnd(q_cur)  = sr%dom(nnest)%qextnd(q_cur)
             dom%qextnd(q_wind) = sr%dom(nnest)%qextnd(q_wind)
          enddo
       endif
    elseif (lge(vers, '4.90.00')) then
       read (iuni, *, err = 1002, end = 1009) sr%wlevelcorr, nextnd
       if (nextnd > 0) then
          do in = nnest-nextnd+1, nnest
             sr%dom(in)%qextnd = 1
          enddo
       endif
    else
       read (iuni, *, err = 1002, end = 1009) sr%wlevelcorr
    endif
    !
    ! By the code above, qextnd is set to
    !                  0 if it should not be extended
    !                  1 if it should be extended
    ! for the considered grid and quantity.
    !
    ! qextnd should be 0 if not used
    !                  1 if used and not extended
    !                  2 if used and extended
    ! for the considered grid and quantity.
    ! transfer swmor/swwlt/swuvt/swwindt to qextnd.
    !
    do in = 1, nnest
       dom => sr%dom(in)
       if (swmor) then
          dom%qextnd(q_bath) = dom%qextnd(q_bath) + 1
       else
          dom%qextnd(q_bath) = 0
       endif
       if (swwlt) then
          dom%qextnd(q_wl) = dom%qextnd(q_wl) + 1
       else
          dom%qextnd(q_wl) = 0
       endif
       if (swuvt) then
          dom%qextnd(q_cur) = dom%qextnd(q_cur) + 1
       else
          dom%qextnd(q_cur) = 0
       endif
       if (sr%swwindt) then
          dom%qextnd(q_wind) = dom%qextnd(q_wind) + 1
       else
          dom%qextnd(q_wind) = 0
       endif
    enddo
    !
    ! option to overrule per grid and quantity
    !
    call adjustinput(sr)
    !
    ! update global flags that determine usage
    !
    swmor = .false.
    swwlt = .false.
    swuvt = .false.
    sr%swwindt = .false.
    do in = 1, nnest
       dom => sr%dom(in)
       swmor = swmor .or. (dom%qextnd(q_bath)>0)
       swwlt = swwlt .or. (dom%qextnd(q_wl)>0)
       swuvt = swuvt .or. (dom%qextnd(q_cur)>0)
       sr%swwindt = sr%swwindt .or. (dom%qextnd(q_wind)>0)
    enddo
!---read boundary conditions commands-------------------------
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---number of boundaries--------------------------------------
    read (iuni, *, err = 1002, end = 1009) nbound
    rindx = 30 + 4*nnest
    cindx = 2
    !
    sr%gamma0    = 3.3 ! We need this even if the number of boundaries is 0!
    !
    do i = 1, nbound
       bnd => sr%bnd(i)
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
!------bnd name--par/read--bndtyp--condition------------------
       read (iuni, *, err = 1002, end = 1009) bndnam, parread,&
                                      &       bndtyp, convar
       bnd%name      = bndnam
       bnd%parread   = parread
       bnd%bndtyp    = bndtyp
       bnd%convar    = convar
       bnd%sshape    = 1
       bnd%periodtype= 1
       bnd%dsprtype  = 1
       bnd%gamma0    = 3.3
       !
       if (bndtyp==1) then
!         Side
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
!------------orientation--------------------------------------
          read (iuni, *, err = 1002, end = 1009) orient
          bnd%orient = orient
          bnd%turn   = 1
       elseif (bndtyp==2) then
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
!---------bndtyp=2 i,j-------------------------------------------
          read (iuni, *, err = 1002, end = 1009) &
                         & bnd%bndcrd_mn(1), bnd%bndcrd_mn(2),  &
                         & bnd%bndcrd_mn(3), bnd%bndcrd_mn(4)
          rindx = rindx + 4
       else
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
!---------bndtyp=3 x,y-------------------------------------------
          read (iuni, *, err = 1002, end = 1009) &
                         & bnd%bndcrd_xy(1), bnd%bndcrd_xy(2),  &
                         & bnd%bndcrd_xy(3), bnd%bndcrd_xy(4)
          rindx = rindx + 4
       endif
       if (parread==2) then
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
!---------shape--periodtype--dsprtype-----------------------------
          read (iuni, *, err = 1002, end = 1009) bnd%sshape, &
            &  bnd%periodtype, bnd%dsprtype ,bnd%gamma0,bnd%sigfr
          if (i == 1) then
             sr%gamma0 = bnd%gamma0
          endif
       endif
!      read sections
       error = rdsec(iuni, parread, convar, nsect, &
                   & bnd%distance ,bnd%waveheight, &
                   & bnd%period   ,bnd%direction , &
                   & bnd%dirspread, &
                   & bnd%spectrum , bnd%turn,   rccnt)
       if (error/=0) goto 1002
       bnd%nsect  = nsect
       rindx = rindx + 5*nsect
       cindx = cindx + nsect
    enddo
    !
    !  Obstacles
    !
    nscr  = 0
    nobst = 0
    ncrv  = 0
    ncrp  = 0
    ncurv = 0
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---number of obstacles----------------------------------------------
    !
    ! array trane must be initialized on 999.9
    ! see usage in write_swan_inp
    !
    trane      = 999.9
    sr%reflection = 0
    sr%refl_type  = 0
    sr%refl_coeff = 999.9
    read (iuni, *, err = 1002, end = 1009) nobst
    l     = 0
    if (nobst>0) then
       k     = 0
       do i = 1, nobst
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
!---------obstacle type----------------------------------------------
          read (iuni, *, err = 1002, end = 1009) obstyp
          if (obstyp==1) then
             rccnt = rccnt + skcomc(iuni)
             rccnt = rccnt + 1
!------------height--alfa--beta--(dam)--------------------------------------
             read (iuni, *, err = 1002, end = 1009) &
                         &                    f(i), ogam(i), obet(i)
          elseif (obstyp==2) then
             rccnt = rccnt + skcomc(iuni)
             rccnt = rccnt + 1
!------------transm. coefficient--(sheet)-----------------------------------
             read (iuni, *, err = 1002, end = 1009) trane(i)
          else
             write (*,*) 'Wrong obstacle type'
             goto 1002
          endif
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
!---------reflection---refl_type---refl_coeff-------------------------------
          if (lge(vers, '4.88.08')) then
             read (iuni, *, err = 1002, end = 1009) sr%reflection(i), &
                 & sr%refl_type(i), sr%refl_coeff(i)
             rccnt = rccnt + skcomc(iuni)
             rccnt = rccnt + 1
          endif
!---------numb. of corner pts----------------------------------------
          read (iuni, *, err = 1002, end = 1009) nlin(i)
          k       = k + nlin(i)
          do j = 1, nlin(i)
             l = l + 1
             rccnt = rccnt + skcomc(iuni)
             rccnt = rccnt + 1
!------------x- y-coordinates----------------------------------------
             read (iuni, *, err = 1002, end = 1009) xpob(l), ypob(l)
          enddo
       enddo
       nscr = l
    endif
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---grav.--rho--northdir--depmin-------------------------------------
    read (iuni, *, err = 1002, end = 1009) grav,rho,sr%northdir,sr%depmin
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---carnaut--setup--swdis--------------------------------------------
    read (iuni, *, err = 1002, end = 1009) carn, setup, swdiss
    sr%nautconv = carn==1
    sr%setup    = setup/=0
    sr%swdis    = swdiss
    !
    !  Wind, only when not using FLOW wind
    !
    sr%wvel = 0.0
    sr%wdir = 0.0
    if (.not. sr%swwindt) then
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
       !---wind type--------------------------------------------------------
       read (iuni, *, err =1002, end = 1009) windtype
       if (windtype == 1) then
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
          !------wind speed--wind direction--(constant)------------------------
          read (iuni, *, err = 1002, end = 1009) sr%wvel(1), sr%wdir(1)
          sr%wvel = sr%wvel(1)
          sr%wdir = sr%wdir(1)
          varwin = .false.
       elseif (windtype == 2) then
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
          !------wind grid--file name wind--(variable)-------------------------
          read (iuni, *, err = 1002, end = 1009) gridtype,wfil
          varwin = .true.
          if (gridtype==2) then
             sr%curviwind = .false.
             call small(wfil, 37)
             rccnt = rccnt + skcomc(iuni)
             rccnt = rccnt + 1
             !---------wind grid spec.--(x0,y0,alpha,nx,ny,dx,dy)-----------------
             read (iuni, *, err = 1002, end = 1009) &
                   & xw, yw, alpw, mxw, myw, dxw, dyw
          else
             sr%curviwind = .true.
          endif
       else
          write (*,'(3a)')   '*** ERROR: Wrong wind type in file ''',trim(filnam),''','
          write (*,'(a,i5)') '           Line ',rccnt
          goto 1002
       endif
    endif
    !
    !        *** read physics commands ***
    !
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---genmode---------------------------------------------------------
    read (iuni, *, err = 1002, end = 1009) sr%genmode
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---breaking--coefficients------------------------------------------
    read (iuni, *, err = 1002, end = 1009) breaking, sr%cfbr1, sr%cfbr2
    sr%breaking = breaking/=0
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---frictype--coefficient-------------------------------------------
    read (iuni, *, err = 1002, end = 1009) sr%frictype, line
    if (sr%frictype == 0) then
       sr%frcof = 0.0
    else
       read (line, *, err = 1002, end = 1009) sr%frcof
    endif
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---triads--coefficients--------------------------------------------
    read (iuni, *, err = 1002, end = 1009) triads, sr%cftriad1,sr%cftriad2
    sr%triads = triads==1
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---Diffraction--diffr_coeff--diffr_smsteps--diffr_adapt_propag-----
    if (lge(vers, '4.88.08')) then
       read (iuni, *, err = 1002, end = 1009) sr%diffraction, &
           & sr%diffr_coeff, sr%diffr_smsteps, sr%diffr_adapt_propag
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
    else
       sr%diffraction        = 0
       sr%diffr_smsteps      = 0
       sr%diffr_adapt_propag = 0
       sr%diffr_coeff        = 0.0
    endif
!---windgrowth--whitecap--quadruplets--refraction--fshift-----------
    read (iuni, *, err = 1002, end = 1009) windgrowth, sr%whitecap, quad, &
                &                          refrac, fshift
    sr%windgrowth  = windgrowth /= 0
    sr%quadruplets = quad == 1
    sr%refraction  = refrac == 1
    sr%fshift      = fshift /= 0
    if (sr%whitecap==2 .and. sr%genmode/=3) then
       write (*,'(2a,i0)') '*** ERROR: wcap=2 (WESTHuysen) can not be', &
            & ' combined with formulations of generation ',sr%genmode
       goto 1002
    endif
    !
    !        *** read numerical accuracy commands ***
    !
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---directional space--freq. space----------------------------------
    read (iuni, *, err = 1002, end = 1009) cdd,css
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---drel--dh_abs--dt_abs--percwet--itermx---------------------------
    read (iuni, *, err = 1002, end = 1009) &
          & sr%drel, sr%dh_abs, sr%dt_abs, sr%percwet, sr%itermx
    !
    !  Output at curves
    !
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---number of output curves-----------------------------------------
    read (iuni, *, err = 1002, end = 1009) ncurv
    l = 0
    if (ncurv>0) then
       k  = 0
       do i = 1, ncurv
          l     = l + 1
          ncrp  = ncrp + 1
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
!---------number of segments along curve--x- y-coordinate-----------
          read (iuni, *, err = 1002, end = 1009) nclin(l),          &
                      &  xpcu(l), ypcu(l)
          nclin(l) = nclin(l) + 1
          k        = k + nclin(l)
          do j = 2, nclin(l)
             l = l + 1
             rccnt = rccnt + skcomc(iuni)
             rccnt = rccnt + 1
             read (iuni, *, err = 1002, end = 1009) nclin(l),       &
                         &  xpcu(l), ypcu(l)
             ncrp = ncrp + nclin(l)
          enddo
       enddo
       ncrv = l
    endif
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
!---itest--itrace--compmode--hotfile--wavm_write_int--non-stationary---
    sr%modsim              = 0
    hotfile                = 0
    sr%wavm_write_interval = 0.0
    if (lge(vers, '4.90.06')) then
       !
       ! Varying number of input values on the next line:
       ! - Read the line as character string
       ! - Read always present parameters from variable "line"
       ! - If additional parameters are present:
       !   Read always present parameters AND additional parameters from variable "line"
       !
       read (iuni, '(a)', err = 1002, end = 1009) line
       read (line, *, err = 1002, end = 1009) itest, itrace, compmode, &
           & hotfile, sr%wavm_write_interval, non_stationary
       if (non_stationary == 1) then
!---------non-stationary: timeinterval--timestep-------------------------
          read (line, *, err = 1002, end = 1009) itest, itrace, compmode, &
              & hotfile, sr%wavm_write_interval, sr%modsim, &
              & sr%deltcom, sr%deltc
          !
          ! non-stationary switched on: modsim = 3
          !
          sr%modsim = 3
       else
          !
          ! Always use modsim = 2 for all stationary and quasi-stationary calculations
          !
          sr%modsim = 2
       endif
    elseif (lge(vers, '4.88.08')) then
       read (iuni, *, err = 1002, end = 1009) itest, itrace, compmode, hotfile
    else
       read (iuni, *, err = 1002, end = 1009) itest, itrace, compmode
    endif
    sr%hotfile = hotfile>0
    sr%compmode = compmode/=0
    !
    ! The modsim in file simulation_mode overrules the modsim read in mdw-file
    !
    inquire (file = 'simulation_mode', exist = exists)
    if (exists) then
       lunsm = new_lun()
       open (lunsm, file = 'simulation_mode')
       !
       ! modsim==2:    quasi-stationary run; time-varying input
       ! modsim==3:    non-stationary run with restart
       !
       read (lunsm, *,iostat=ierr) sr%modsim
       if (ierr /= 0) then
          write(*,*)'*** ERROR: Unable to read modsim from file ''simulation_mode'''
          close (lunsm)
          stop
       endif
       read (lunsm, *,iostat=ierr) sr%deltc       ! Time step in non-stat SWAN runs
       if (ierr /= 0 .and. sr%modsim == 3) then
          write(*,*)'*** ERROR: Unable to read deltc from file ''simulation_mode'''
          write(*,*)'           Necessary when modsim =3'
          close (lunsm)
          stop
       endif
       if (ierr == 0) then
          read (lunsm, *,iostat=ierr) sr%deltcom     ! Interval of communication FLOW-WAVE
          if (ierr /= 0 .and. sr%modsim == 3) then
             write(*,*)'*** ERROR: Unable to read deltcom from file ''simulation_mode'''
             write(*,*)'           Necessary when modsim =3'
             close (lunsm)
             stop
          endif
       endif
       close (lunsm)
    endif
!---flow grid-flow grid name---------------------------------------
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
    read (iuni, *, err = 1002, end = 1009) swwv,rgfout
    swwav = .false.
    if (swwv==1)  then
       if (wavedata%mode == stand_alone) then
          write(*,'(a)') '*** WARNING : Assuming com-file is present to add wave information.'
          swwav = .true.
       else
          swwav = .true.
       endif
    endif
!---output to locations--------------------------------------------
    rccnt = rccnt + skcomc(iuni)
    rccnt = rccnt + 1
    read (iuni, *, err = 1002, end = 1009) pnts
    sr%output_points = pnts==1
    if (sr%output_points) then
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
       read (iuni, *, err = 1002, end = 1009) ipfl
       sr%output_pnt_file = ipfl==1
       if (sr%output_pnt_file) then
!---------file name output locations------------------------------
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
          read (iuni, *, err = 1002, end = 1009) sr%pntfil
       else
!---------number of locations-------------------------------------
          rccnt = rccnt + skcomc(iuni)
          rccnt = rccnt + 1
          read (iuni, *, err = 1002, end = 1009) npoints
          do i = 1, npoints
!------------x y coordinate of location---------------------------
             rccnt = rccnt + skcomc(iuni)
             rccnt = rccnt + 1
             read (iuni, *, err = 1002, end = 1009) &
                  & sr%xyloc(1,i), sr%xyloc(2,i)
          enddo
       endif
!------table--spec1d--spec2d---------------------------------------
       rccnt = rccnt + skcomc(iuni)
       rccnt = rccnt + 1
       read (iuni, *, err = 1002, end = 1009) table, spec1d, spec2d
       sr%output_table = table==1
       sr%output_spec1d = spec1d==1
       sr%output_spec2d = spec2d==1
    endif
    !
    !  Communication file names; fill in dummy name if not required
    !
    !
    ! File md-wave is no longer accepted
    !
    ind=index(filnam,'.mdw')
    casl=filnam(1:ind-1)
    filcom      = ' '
    filcom(1:4) = 'com-'
    filcom(5:)  = casl
    inrhog      = 1
    sr%nbound   = nbound    !  number of boundaries
    sr%npoints  = npoints
    goto 1010
1002 continue
    write (*,'(a,a,a)') '*** ERROR: While reading file ''',trim(filnam),''','
    write (*,'(a,i5)')  '           Line ',rccnt
    error = 4
    !        read
    goto 1010
    !
 1009 continue
    write (*,'(a,a,a)') '*** ERROR: EOF While reading file ''',trim(filnam),''','
    write (*,'(a,i5)')  '           Line ',rccnt
    error = 6
    !        eof
    !
 1010 continue
    message = filnam
    close (iuni, err = 1011)
    goto 1012
    !
    !        close
 1011 continue
    error = 2
    write (*,'(a,a,a)')      '*** ERROR: While closing file ''',trim(message),''','
    write (*,'(a,i5,a,a,a)') '           Error on Line ',rccnt, ' in file ''',trim(filnam),'''.'
    goto 1012
    !
    !        open
 1001 continue
    write (*,'(a,a,a)') '*** ERROR: While opening file ''',trim(message),''','
    write (*,'(a,i5,a,a,a)')  '           Error on Line ',rccnt, ' in file ''',trim(filnam),'''.'
    error = 1
    !
 1012 continue
    if (error /= 0) stop
end subroutine read_swan_mdw
!
!
!==============================================================================
subroutine write_swan_input (sr, itide, outcnt, inest, wavedata)
    !
    implicit none
    !
    integer                           :: itide
    integer                           :: inest
    integer                           :: outcnt
    real                              :: wdir
    real                              :: wvel
    character(37)                     :: curlif
    type(swan)                        :: sr
    type(wave_data_type)              :: wavedata
    !
    curlif = sr%dom(inest)%curlif(1:37)
    wvel   = sr%wvel(itide)
    wdir   = sr%wdir(itide)
    !
    call write_swan_inp (wavedata, outcnt, &
                    & itide        ,sr%nttide ,inest      ,sr%nnest  ,sr%swuvt  , &
                    & sr%swuvi     ,sr%prname ,sr%prnumb  ,sr%title1 ,sr%title2 , &
                    & sr%title3    ,sr%itest  ,sr%itrace  , &
                    & sr%inrhog    ,wvel      ,wdir       , &
                    & sr%wlevelcorr,sr%grav   ,sr%rho     ,sr%nobst  ,sr%nscr   , &
                    & sr%wfil      ,sr%ffil   ,sr%xw      , &
                    & sr%yw        ,sr%alpw   ,sr%mxw     ,sr%myw    ,sr%dxw    , &
                    & sr%dyw       ,sr%trane  ,sr%f       , &
                    & sr%ogam      ,sr%obet   ,sr%xpob    ,sr%ypob   ,sr%nlin   , &
                    & sr%varwin    ,sr%varfri ,sr%ncurv   ,sr%ncrv   ,sr%nclin  , &
                    & sr%xpcu      ,sr%ypcu   ,curlif     ,sr%casl   , &
                    & sr%cdd       ,sr%css    ,sr%sferic  ,sr     )
end subroutine write_swan_input
!
!
!==============================================================================
subroutine write_swan_inp (wavedata, outcnt, &
                & itide     ,nttide    ,inest     ,nnest     ,swuvt     , &
                & swuvi     ,prname    ,prnumb    ,title1    ,title2    , &
                & title3    ,itest     ,itrace    , &
                & inrhog    ,wvel      ,wdir      , &
                & wlevelcorr,grav      ,rho       ,nobst     ,nscr      , &
                & wfil      ,ffil      ,xw        , &
                & yw        ,alpw      ,mxw       ,myw       ,dxw       , &
                & dyw       ,trane     ,f         , &
                & ogam      ,obet      ,xpob      ,ypob      ,nlin      , &
                & varwin    ,varfri    ,ncurv     ,ncrv      ,nclin     , &
                & xpcu      ,ypcu      ,curlif    ,casl      , &
                & cdd       ,css       ,sferic    ,sr     )
   use properties
   use read_grids
   use wave_data
   !
   implicit none
!
! Global variables
!
    integer                        , intent(in)  :: inest
    integer                        , intent(in)  :: inrhog
    integer                        , intent(in)  :: itest
    integer                        , intent(in)  :: itide
    integer                        , intent(in)  :: itrace
    integer                        , intent(in)  :: mxw
    integer                        , intent(in)  :: myw
    integer                        , intent(in)  :: ncrv
    integer                        , intent(in)  :: ncurv
    integer                        , intent(in)  :: nnest
    integer                        , intent(in)  :: nobst
    integer                        , intent(in)  :: nscr
    integer                        , intent(in)  :: nttide
    integer                        , intent(in)  :: outcnt
    integer      , dimension(ncrv) , intent(in)  :: nclin
    integer      , dimension(nobst), intent(in)  :: nlin
    logical                        , intent(in)  :: sferic
    logical                        , intent(in)  :: swuvi
    logical                        , intent(in)  :: swuvt
    logical                        , intent(in)  :: varfri
    logical                        , intent(in)  :: varwin
    real                           , intent(in)  :: alpw
    real                           , intent(in)  :: cdd
    real                           , intent(in)  :: css
    real                           , intent(in)  :: dxw
    real                           , intent(in)  :: dyw
    real                           , intent(in)  :: grav
    real                           , intent(in)  :: rho
    real                           , intent(in)  :: wdir
    real                           , intent(in)  :: wlevelcorr
    real                           , intent(in)  :: wvel
    real                           , intent(in)  :: xw
    real                           , intent(in)  :: yw
    real         , dimension(ncrv) , intent(in)  :: xpcu
    real         , dimension(ncrv) , intent(in)  :: ypcu
    real         , dimension(nobst), intent(in)  :: f
    real         , dimension(nobst), intent(in)  :: obet
    real         , dimension(nobst), intent(in)  :: ogam
    real         , dimension(nobst), intent(in)  :: trane
    real         , dimension(nscr) , intent(in)  :: xpob
    real         , dimension(nscr) , intent(in)  :: ypob
    character(16)                  , intent(in)  :: prname
    character(*)                   , intent(in)  :: casl
    character(37)                  , intent(in)  :: curlif
    character(37)                  , intent(in)  :: ffil
    character(37)                                :: wfil
    character(4)                   , intent(in)  :: prnumb
    character(72)                  , intent(in)  :: title1
    character(72)                  , intent(in)  :: title2
    character(72)                  , intent(in)  :: title3
    type(swan)                                   :: sr
    type(wave_data_type)                         :: wavedata
!
! Local variables
!
    integer                     :: bound
    integer                     :: cindx
    integer                     :: convar
    integer                     :: cs
    integer                     :: dsprtype
    integer                     :: i
    integer                     :: ind
    integer                     :: j
    integer                     :: jendcrv
    integer                     :: k
    integer                     :: kst
    integer                     :: l
    integer                     :: lc
    integer                     :: lunhot
    integer                     :: luninp
    integer                     :: mdc1
    integer                     :: msc
    integer                     :: mxfr
    integer                     :: myfr
    integer                     :: n
    integer                     :: nb
    integer                     :: npoints
    integer                     :: nsect
    integer                     :: orient
    integer                     :: parread
    integer                     :: periodtype
    integer                     :: rindx
    integer                     :: sect
    integer                     :: shape
    integer                     :: loc    
    integer, external           :: new_lun
    logical                     :: exists
    logical                     :: frame
    real                        :: alpb
    real                        :: alpfr
    real                        :: dir1
    real                        :: dir2
    real                        :: dxb
    real                        :: dyb
    real                        :: fhigh
    real                        :: flow
    real                        :: timsec
    real                        :: xlenfr
    real                        :: xpfr
    real                        :: xpb
    real                        :: ypb
    real                        :: ylenfr
    real                        :: ypfr
    real                        :: tal
    real                        :: tap
    character(6)                :: oldhot
    character(7), dimension(20) :: varnam1
    character(7), dimension(9)  :: varnam2
    character(8)                :: casl_short
    character(15)               :: tbegc
    character(15)               :: tendc
    character(15), external     :: datetime_to_string
    character(37)               :: botfil
    character(37)               :: curfil
    character(37)               :: mudfil
    character(37)               :: vegfil
    character(60)               :: lijn
    character(79)               :: line
    character(79)               :: pointname
    character(256)              :: fname
    type(swan_bnd), pointer     :: bnd
    type(swan_dom), pointer     :: dom
    !
    ! Do not add more variables to varnam1
    !
    data varnam1/'HSIGN  ', 'DIR    ', 'TM01   ', 'DEPTH ', 'VELOC ',     &
         &       'TRANSP ', 'DSPR   ', 'DISSIP ', 'LEAK  ', 'QB    ',     &
         &       'XP     ', 'YP     ', 'DIST   ', 'UBOT  ', 'STEEPW',     &
         &       'WLENGTH', 'FORCES ', 'RTP    ', 'PDIR  ', 'WIND  '      /
    data varnam2/'TPS    ', 'TM02   ', 'TMM10  ', 'DHSIGN', 'DRTM01',     &
         &       'SETUP  ', 'DISSURF', 'DISWCAP', 'DISBOT'                /
!
!! executable statements -------------------------------------------------------
!
    dom => sr%dom(inest)
    !
    !     *** additional swan arrays ***
    !
    botfil = 'BOTNOW'
    curfil = 'CURNOW'
    mudfil = 'MUDNOW'
    vegfil = 'VEGNOW'
    !
    if (dom%qextnd(q_wind)>0 .or. dom%n_meteofiles_dom > 0) wfil = 'WNDNOW'
    nb     = sr%nbound
    !
    mxfr       = 0 !swani(11)
    myfr       = 0 !swani(12)
    npoints    = sr%npoints
    !
    msc  = dom%nfreq
    mdc1 = dom%ndir
    cs   = dom%dirspace
    !
    xpfr     = 0.0 !swanr(14)
    ypfr     = 0.0 !swanr(15)
    alpfr    = 0.0 !swanr(16)
    xlenfr   = 0.0 !swanr(17)
    ylenfr   = 0.0 !swanr(18)
    !
    flow  = dom%freqmin
    fhigh = dom%freqmax
    dir1  = dom%startdir
    dir2  = dom%enddir
    !
    ! *** Swan file is written *****************************************
    !
    !     General project data of SWAN file
    !
    lc = len_trim(casl)
    casl_short = casl(1:8)
    if (lc > 8) then
       write(*,'(5a)') '*** MESSAGE: ''',trim(casl),''' is truncated to ''',trim(casl_short),''' in SWAN input file'
       lc = len_trim(casl_short)
    endif
    luninp = new_lun()
    open (luninp, file = 'swan.inp')
    line(1:79) = ' '
    line(1:72) =                                           &
     & '$***************************** HEADING ************&
     &*********************'
    write (luninp, '(1X,A)') line
    line(1:79)  = ' '
    line(1:2)   = '$ '
    write (luninp, '(1X,A)') line
    line(1:9)   = 'PROJECT  '
    i           = 10
    line(i:i)   = ''''''
    line(11:26) = prname
    line(27:30) = '''  '''
    line(31:34) = prnumb
    i           = 35
    line(i:i)   = ''''''
    line(36:79) = ' '
    write (luninp, '(1X,A)') line(1:35)
    i           = 1
    line(i:i)   = ''''''
    i           = 74
    line(i:i)   = ''''''
    line(2:73)  = title1
    write (luninp, '(5X,A)') line(1:74)
    line(2:73)  = title2
    write (luninp, '(5X,A)') line(1:74)
    line(2:73)  = title3
    write (luninp, '(5X,A)') line(1:74)
    line(1:79)  = ' '
    line(1:2)   = '$ '
    write (luninp, '(1X,A)') line
    line(1:79)  = ' '
    line(1:72)  =                                           &
     & '$***************************** MODEL INPUT ********&
     &*********************'
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    !
    !     Coefficient settings
    !
    line(1:22)  = 'SET   LEVEL =         '
    line(23:47) = 'NOR =           DEPMIN = '
    write (line(15:21), '(F6.2)') wlevelcorr
    write (line(29:34), '(F6.2)') sr%northdir
    write (line(48:53), '(F6.2)') sr%depmin
    line(54:55) = ' _'
    write (luninp, '(1X,A)') line
    if (sr%modsim > 0) then
       write(line,'(a,i0,a)') '       MAXMES = 1000   MAXERR = ', sr%maxerr, ' _'
       write (luninp, '(1X,A)') line
    endif
    line(1:79)  = ' '
    line(7:22)  = 'GRAV =          '
    line(23:47) = 'RHO =           INRHOG = '
    write (line(15:21), '(F6.2)') grav
    write (line(29:36), '(F8.2)') rho
    write (line(48:53), '(  I6)') inrhog
    line(54:55) = ' _'
    write (luninp, '(1X,A)') line
    line(1:79)  = ' '
    if (sr%nautconv) then
       line(7:11) = 'NAUT '
    else
       line(7:11) = 'CART '
    endif
    write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
    line(1:79) = ' '
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    !
    ! hotfile= .true.: use hotfile
    ! modsim = 2     : quasi-stationary
    ! modsim = 3     : non-stationary
    !
    if (sr%modsim <= 1) then
       line(1:79) = ' '
       line(1:10) = 'MODE STAT '
       write (luninp, '(1X,A)') line
    elseif (sr%modsim > 1) then
       line(1:79) = ' '
       line(1:10) = 'MODE NONST'
       write (luninp, '(1X,A)') line
    else
    endif
    if (sferic) then
       line(1:79) = ' '
       line(1:11) = 'COORD SPHE '
       write (luninp, '(1X,A)') line
    endif
    !
    !     *** activate Setup in SWAN ***
    !
    if (sr%setup) then
       line(1:79) = ' '
       line(1:5) = 'SETUP'
       write (luninp, '(1X,A)') line
    endif
    !
    !     Definition computation grid(s). Note: these definitions are
    !     also used in preparing NEST files.
    !
    !     ***                                                 ***
    !     ***  additional commands for CURVI-LINEAR grid      ***
    !     ***                                                 ***
    !
    line(1:79) = ' '
    line(1:6)  = 'CGRID '
    line(7:11) = 'CURV '
    write (line(12:21), '(2(I4,1X))') dom%mxc, dom%myc
    line(31:48) = 'EXCEPT  0.0    0.0'
    line(57:58) = ' _'
    write (luninp, '(1X,A)') line
    line(1:79)  = ' '
    if (cs==1) then
       line(7:22) = 'CIR             '
    elseif (cs==2) then
       line(7:11) = 'SEC  '
       write (line(12:32), '(F10.2,1X,F10.2)') dir1, dir2
    else
    endif
    write (line(33:37), '(I4,1X)') mdc1
    write (line(38:59), '(2(F10.2,1X))') flow, fhigh
    write (line(60:64), '(I4,1X)') msc
    write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
    line(1:79) = ' '
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    !
    !     READING of coordinates CURVILINEAR computational grid
    !
    line(1:13) = 'READ COOR 1. '
    ind = index(curlif, ' ')
    i = 14
    line(i:i) = ''''''
    line(15:14 + ind) = curlif
    line(14 + ind:14 + ind) = ''''''
    line(15 + ind:16 + ind) = ' _'
    line(17 + ind:79) = ' '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    line(1:)   = ' 4   0   1 FREE'
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
    line(1:79) = ' '
    !
    !     Definition of bottom grid(s)
    !
    lijn = 'INPGRID _'
    write (luninp, '(1X,A)') lijn
    if (dom%curvibot==1) then
       line(1:18) = 'BOTTOM CURV 0. 0. '
       write (line(19:28), '(2(I4,1X))') dom%mxb, dom%myb
       write (luninp, '(1X,A)') line
    else
       fname = dom%depfil
       call readregulargrid(fname, sferic, xpb, ypb, alpb, &
                          & dom%mxb, dom%myb, dxb, dyb)
       !
       ! poles? no, fences!
       !
       dom%mxb = dom%mxb - 1
       dom%myb = dom%myb - 1
       line(1:11) = 'BOTTOM REG '
       write (line(12:74),                                   &
       &       '(2(F10.2,4X),F6.1,1X,2(I4,1X),2(F8.2,1X))')  &
       &       xpb, ypb, alpb,          &
       &       dom%mxb, dom%myb, dxb, dyb
       write (luninp, '(1X,A)') line
       botfil = dom%botfil(1:37)
    endif
    line(1:79) = ' '
    !
    !     File-name bottom depth  (use temporary file)
    !
    line(1:18) = 'READINP BOTTOM 1.0'
    !     Write (line(16:21),'(F5.1,1X)') fac
    ind = index(botfil, ' ')
    i = 22
    line(i:i) = ''''''
    i = i+1
    line(i:) = trim(botfil)
    i = i+(ind-1)
    line(i:i) = ''''''
    i = i+1
    if (dom%curvibot==1) then
       line(i:) = ' 4'
    else
       line(i:) = ' 3'
    endif
    i = i+2
    line(i:) = ' 0 FREE'
    write (luninp, '(1X,A)') line(1:79)
    line(1:79) = ' '
    !
    !     File-name current field (temporary file)
    !
    if (dom%qextnd(q_cur)>0 .or. swuvi) then
       lijn = 'INPGRID _'
       line(1:18) = 'CURREN CURV 0. 0. '
       write (line(19:28), '(2(I4,1X))')    dom%mxc, dom%myc
       write (luninp, '(1X,A)') lijn
       write (luninp, '(1X,A)') line(1:79)
       line(1:79) = ' '
       !
       !        *** read current grid ***
       !
       line(1:21)  = 'READ CUR FAC= 1.    _'
       line(22:79) = ' '
       write (luninp, '(1X,A)') line(1:79)
       line(1:79)  = ' '
       ind         = index(curfil, ' ')
       i           = 1
       line(i:i)   = ''''''
       line(2:)    = curfil
       line(ind + 1:ind + 1)   = ''''''
       line(ind + 4:ind + 10)  = 'IDLA=4 '
       line(ind + 11:ind + 14) = 'FREE'
       line(ind + 15:79)       = ' '
       write (luninp, '(1X,A)') line(1:79)
       line(1:79)  = ' '
    endif
!-----------------------------------------------------------------------
    line(1:79) = ' '
    !
    !     Fluid Mud
    !
    if (wavedata%mode == flow_mud_online) then
       lijn = 'INPGRID _'
       line(1:18) = 'MUDL CURV 0. 0. '
       write (line(19:28), '(2(I4,1X))')    dom%mxc, dom%myc
       write (luninp, '(1X,A)') lijn
       write (luninp, '(1X,A)') line(1:79)
       line(1:79) = ' '
       !
       !     File-name mud depth  (use temporary file)
       !
       line(1:18) = 'READINP MUDL 1.0'
       ind = index(mudfil, ' ')
       i = 22
       line(i:i) = ''''''
       i = i+1
       line(i:) = trim(mudfil)
       i = i+(ind-1)
       line(i:i) = ''''''
       i = i+1
       line(i:) = ' 4'
       i = i+2
       line(i:) = ' 0 FREE'
       write (luninp, '(1X,A)') line(1:79)
    endif
    !
    !     Vegetation map
    !
    line(1:2) = '$ '
    write (luninp, '(1X,A)') line
    if (dom%vegetation == 1) then
       lijn = 'INPGRID _'
       line(1:19) = 'NPLANTS CURV 0. 0. '
       write (line(20:29), '(2(I4,1X))')    dom%mxc, dom%myc
       write (luninp, '(1X,A)') lijn
       write (luninp, '(1X,A)') line(1:79)
       line(1:79) = ' '
       !
       !     File-name vegetation map 
       !
       line  = 'READINP NPLANTS 1.0 ''' // trim(vegfil) // ''' 4 0 FREE'
       write (luninp, '(1X,A)') line(1:79)
    endif
!-----------------------------------------------------------------------
    line(1:79) = ' '
    line(1:2) = '$ '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    !
    !     diffraction
    !
    if (sr%diffraction == 1) then
       line(1:7) = 'DIFFRAC'
       write (line( 9: 9), '(I1)'   ) sr%diffraction
       write (line(11:20), '(F10.5)') sr%diffr_coeff
       write (line(22:25), '(I4)'   ) sr%diffr_smsteps
       write (line(27:27), '(I1)'   ) sr%diffr_adapt_propag
       write (luninp, '(1X,A)') line(1:79)
       line(1:79)  = ' '
    endif
!-----------------------------------------------------------------------
    line(1:2) = '$ '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    !
    !     definition of grid for wind field
    !
    if (dom%qextnd(q_wind)>0 .or. dom%n_meteofiles_dom > 0) then
       !        *** definition of grid ***
       !
       if (.not.sr%curviwind) then
          lijn       = 'INPGRID _'
          line(1:79) = ' '
          line(1:7)  = 'WIND   '
          write (line(8:35),  '(2(F10.2,4X))') xw, yw
          write (line(36:43), '(  F7.1 ,1X) ') alpw
          write (line(44:53), '(2(I4   ,1X))') mxw, myw
          write (line(54:71), '(2(F8.2 ,1X))') dxw, dyw
          write (luninp, '(1X,A)') lijn
          write (luninp, '(1X,A)') line(1:79)
          line(1:79)  = ' '
       else
          lijn       = 'INPGRID _'
          line(1:79) = ' '
          line(1:18) = 'WIND CURV 0. 0.   '
          write (line(19:28), '(2(I4,1X))') dom%mxc, dom%myc
          if (sr%excval> - 998.99 .or. sr%excval< - 999.01) then
             line(29:37) = ' EXCVAL '
             write (line(38:49), '(F12.4)') sr%excval
             line(50:79) = ' '
          else
             line(29:79) = ' '
          endif
          write (luninp, '(1X,A)') lijn
          write (luninp, '(1X,A)') line(1:79)
          line(1:79) = ' '
       endif
       !
       !        *** read wind grid ***
       !
       line(1:20)  = 'READ WIN FAC= 1.   _'
       line(21:79) = ' '
       write (luninp, '(1X,A)') line(1:79)
       line(1:79)  = ' '
       ind         = index(wfil, ' ')
       i           = 1
       line(i:i)   = ''''''
       line(2:)    = wfil
       line(ind + 1:ind + 1)   = ''''''
       line(ind + 4:ind + 10)  = 'IDLA=4 '
       line(ind + 11:ind + 14) = 'FREE'
       line(ind + 15:79)       = ' '
       write (luninp, '(1X,A)') line(1:79)
       line(1:79)              = ' '
    endif
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    !
    !     Uniform wind velocity and direction
    !
    if (.not.varwin .and. dom%n_meteofiles_dom == 0) then
       if (sr%genmode==0) then
          line(1:79) = ' '
          write (luninp, '(1X,A)') line
       elseif (abs(wvel)>0.) then
          line(1:10)  = 'WIND  VEL='
          write (line(11:20), '(F10.2)') wvel
          line(21:25) = ' DIR='
          write (line(26:35), '(F10.2)') wdir
          line(36:79) = ' '
          write (luninp, '(1X,A)') line
       else
       endif
    endif
!-----------------------------------------------------------------------
    line(1:79) = ' '
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    !
    !     definition of grid for friction field
    !
    if (varfri) then
       lijn       = 'INPGRID _'
       line(1:79) = ' '
       line(1:18) = 'FRIC CURV 0. 0.   '
       write (line(19:28), '(2(I4,1X))') dom%mxc, dom%myc
       if (sr%excval> - 998.99 .or. sr%excval< - 999.01) then
          line(29:37) = ' EXCVAL '
          write (line(38:49), '(F12.4)') sr%excval
          line(50:79) = ' '
       else
          line(29:79) = ' '
       endif
       write (luninp, '(1X,A)') lijn
       write (luninp, '(1X,A)') line(1:79)
       line(1:79) = ' '
       line(1:20)  = 'READ FRI FAC= 1.   _'
       line(21:79) = ' '
       write (luninp, '(1X,A)') line(1:79)
       ind         = index(ffil, ' ')
       i                       = 1
       line(i:i)               = ''''''
       line(2:)                = ffil
       line(ind + 1:ind + 1)   = ''''''
       line(ind + 4:ind + 10)  = 'IDLA=4 '
       line(ind + 11:ind + 14) = 'FREE'
       line(ind + 15:79)       = ' '
       write (luninp, '(1X,A)') line(1:79)
       line(1:79)              = ' '
    endif
!-----------------------------------------------------------------------
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
    !
    !     Input wave parameters:
    !     Note that different sides can be chosen (this in contrast with
    !     HISWA in which the boundary conditions are allways described
    !     at x=0
    !
    !
    !     boundary condition:       par/read
    !     also for nested runs      side/segment
    !                                     xy/ij
    !
    rindx = 30 + 4*nnest
    cindx = 2
    if (inest==1) then
       do bound = 1, nb
          bnd => sr%bnd(bound)
          if (bnd%bndtyp==4) then
            line        = ' '
            line( 1:10) = 'BOUN NEST '
            line(11:11) = ''''''
            ind = index(sr%specfile, ' ') - 1
            line(12:12 + ind) = sr%specfile
            line(12+ind:12+ind) = ''''''
            line(12+ind+1:12+ind+7) = ' CLOSED'
            write(luninp, '(1X,A)') line
            cycle
          elseif (bnd%bndtyp==5) then
            line        = ' '
            line( 1:11) = 'BOUN WWIII '
            line(12:12) = ''''''
            ind = index(sr%specfile, ' ') - 1
            line(13:13 + ind) = sr%specfile
            line(13+ind:13+ind) = ''''''
            line(13+ind+1:13+ind+5) = ' OPEN'
            write(luninp, '(1X,A)') line
            cycle
          endif
          line(1:79) = ' '
          parread = bnd%parread
          !              User specified conditions
          shape     = bnd%sshape
          periodtype= bnd%periodtype
          dsprtype  = bnd%dsprtype
          line(1:)  = 'BOUN SHAPE'
          if (shape==1) then
             write (line(12:), '(A, 1X, F6.2)') 'JONSWAP', bnd%gamma0
          elseif (shape==2) then
             line(12:) = 'PM'
          elseif (shape==3) then
             write (line(12:), '(A, 1X, F6.2)') 'GAUSS', bnd%sigfr
          else
          endif
          if (periodtype==1) then
             line(27:) = 'PEAK'
          else
             line(27:) = 'MEAN'
          endif
          if (dsprtype==1) then
             line(32:) = 'DSPR POWER'
          else
             line(32:) = 'DSPR DEGR'
          endif
          if (.not.(shape==1 .and. bnd%gamma0==0.0)) then
             ! something was actually defined
             write (luninp, '(1X,A)') line
          endif
          !
          line(1:79) = ' '
          line(1:)   = 'BOUN'
          nsect      = bnd%nsect
          convar     = bnd%convar
          if (bnd%bndtyp==1) then
             !              Side
             line(6:) = 'SIDE'
             orient   = bnd%orient
             if (orient==1) then
                line(11:) = 'N'
             elseif (orient==2) then
                line(11:) = 'NW'
             elseif (orient==3) then
                line(11:) = 'W'
             elseif (orient==4) then
                line(11:) = 'SW'
             elseif (orient==5) then
                line(11:) = 'S'
             elseif (orient==6) then
                line(11:) = 'SE'
             elseif (orient==7) then
                line(11:) = 'E'
             elseif (orient==8) then
                line(11:) = 'NE'
             else
             endif
             if (bnd%turn==1) then
                line(14:) = 'CCW'
             else
                line(14:) = 'CLOCKW'
             endif
             line(22:)  = '_'
             write (luninp, '(1X,A)') line
          else
             !
             ! Segment
             !
             line(6:) = 'SEGM'
             if (bnd%bndtyp == 3) then
                line(11:) = 'XY'
                write (line(14:), '(4(F12.3,2X))')         &
                     & bnd%bndcrd_xy(1), bnd%bndcrd_xy(2), &
                     & bnd%bndcrd_xy(3), bnd%bndcrd_xy(4)
             elseif (bnd%bndtyp == 2) then
                line(11:) = 'IJ'
                write (line(14:), '(4I10)')                          &
                     & bnd%bndcrd_mn(1), bnd%bndcrd_mn(2), &
                     & bnd%bndcrd_mn(3), bnd%bndcrd_mn(4)
             endif
             line(72:)  = '&'
             write (luninp, '(1X,A)') line
             rindx = rindx + 4
          endif
          line(1:79) = ' '
          if (convar==1) then
             !                 Constant conditions
             line(21:) = 'CON'
             if (parread==2) then
                !                    User specified conditions
                write (line(26:), '(A,4F8.2)') 'PAR ',                 &
                                & bnd%waveheight(1), bnd%period(1)   , &
                                & bnd%direction (1), bnd%dirspread(1)
             else
                !                    Read conditions from file
                ind = index(bnd%spectrum(1), ' ') - 1
                line(26:30)         = 'FILE '
                i                   = 31
                line(i:i)           = ''''''
                line(i + 1:i + ind) = bnd%spectrum(1)
                i                   = i + ind
                line(i + 1:i + 1)   = ''''''
                line(i + 3:i + 4)   = ' 1'
             endif
             write (luninp, '(1X,A)') line
             rindx = rindx + 5
             cindx = cindx + 1
          else
             !                 Variable conditions
             line(21:) = 'VAR'
             if (parread==2) then
                !                    User specified conditions
                line(25:) = 'PAR'
                do sect = 1, nsect
                   write (line(29:), '(5F9.2)') bnd%distance(sect) , &
                        & bnd%waveheight(sect), bnd%period(sect)   , &
                        & bnd%direction (sect), bnd%dirspread(sect)
                   if (sect<nsect) then
                      line(75:) = '&'
                   endif
                   rindx      = rindx + 5
                   write (luninp, '(1X,A)') line
                   line(1:79) = ' '
                enddo
                cindx = cindx + nsect
             else
                !                    Read conditions from file
                line(25:29) = 'FILE '
                do sect = 1, nsect
                   write (line(30:38), '(F9.2)') bnd%distance(sect)
                   line(39:39)         = ' '
                   i                   = 40
                   line(i:i)           = ''''''
                   ind = index(bnd%spectrum(sect), ' ') - 1
                   line(i + 1:i + ind) = bnd%spectrum(sect)
                   i                   = i + ind
                   line(i + 1:i + 1)   = ''''''
                   line(i + 3:i + 4)   = ' 1'
                   if (sect<nsect) then
                      line(79:) = '&'
                   endif
                   rindx               = rindx + 5
                   cindx               = cindx + 1
                   write (luninp, '(1X,A)') line
                   line(1:79)          = ' '
                enddo
             endif
          endif
       enddo
    else
       line        = ' '
       line( 1:10) = 'BOUN NEST '
       line(11:11) = ''''''
       line(12:15) = 'NEST'
       write(line(16:18),'(I3.3)') inest
       line(19:19) = ''''''
       line(20:26) = ' CLOSED'
       write(luninp, '(1X,A)') line
    endif
!-----------------------------------------------------------------------
    line(1:79) = ' '
    !
    ! hotfile= true: use hotfile
    !
    if (sr%hotfile) then
       !
       ! define the name of the hotfile to be used
       !
       write (fname,'(a,i0,2a)') 'hot_', inest, '_', trim(sr%usehottime)
       !
       ! use it when it exists
       !
       inquire (file = trim(fname), exist = exists)
       if (exists) then
          line  = 'INIT HOTS ''' // trim(fname) // ''''
          write (luninp, '(1X,A)') line
          write(*,'(2a)') '  Using SWAN hotstart file: ',trim(fname)
       endif
    endif
    !
    !     Physics activated in SWAN (default options)
    !
!-----------------------------------------------------------------------
    line(1:79) = ' '
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    if (sr%genmode == 1) then
       line(1:8) = 'GEN1 '
    elseif (sr%genmode == 2) then
       line(1:8) = 'GEN2 '
    elseif (sr%genmode == 3) then
       if (sr%whitecap == 2) then
          line = 'GEN3 WESTH'
       else
          line(1:8) = 'GEN3 '
       endif
    else
    endif
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    if (.not.sr%breaking) then
       line(1:9)   = 'OFF BREAK'
       line(10:79) = ' '
       write (luninp, '(1X,A)') line
       line(1:79)  = ' '
    else
       line(1:9)   = 'BREAK CON'
       write (line(15:30), '(2(F6.2,2X))') sr%cfbr1, sr%cfbr2
       write (luninp, '(1X,A)') line
       line(1:79)  = ' '
    endif
    if (sr%frictype==1) then
       line(1:9) = 'FRIC JON '
       write (line(15:21), '(F6.4,1X)') sr%frcof
    elseif (sr%frictype==2) then
       line(1:9) = 'FRIC COLL'
       write (line(15:21), '(F6.4,1X)') sr%frcof
    elseif (sr%frictype==3) then
       line(1:9) = 'FRIC MAD '
       write (line(15:21), '(F6.4,1X)') sr%frcof
    else
    endif
    if (sr%frictype/=0) then
       write (luninp, '(1X,A)') line
       line(1:79) = ' '
    endif
    if (wavedata%mode == flow_mud_online) then
       !
       ! In the future, (some of) the following parameters must be read from the mud-comfile
       !
       !write (luninp, '(1x,a)') 'MUD alpha=1. rhom=1300. nu=0.0027'
       write (luninp, '(1x,a,f8.2,a,f8.6)') 'MUD alpha=1. rhom=', sr%rhomud, &
            & ' nu=', sr%viscmud
    endif
    if (sr%triads) then
       line(1:6) = 'TRIAD '
       write (line(15:27), '(F6.4,1X,F6.4)') sr%cftriad1, sr%cftriad2
       line(31:40)= ' 0.2  0.01'
       write (luninp, '(1X,A)') line
       line(1:79) = ' '
    endif
    if (.not.sr%windgrowth) then
       line(1:10)  = 'OFF WINDG '
       line(11:79) = ' '
       write (luninp, '(1X,A)') line
       line(1:79)  = ' '
    endif
    if (.not.sr%quadruplets) then
       line(1:10)  = 'OFF QUAD  '
       line(11:79) = ' '
       write (luninp, '(1X,A)') line
       line(1:79)  = ' '
    endif
    if (sr%whitecap==0) then
       line(1:10)  = 'OFF WCAP  '
       line(11:79) = ' '
       write (luninp, '(1X,A)') line
    !else
    !   line(1:20)  = 'WCAP   CSM   4   2  '
    !   write (luninp, '(1X,A)') line
    endif
    line(1:79)  = ' '
    line(1:10)  = 'LIM  10 1 '
    write (luninp, '(1X,A)') line
    line(1:79)  = ' '
    if (.not.sr%refraction) then
       line(1:10)  = 'OFF REFRAC'
       line(11:79) = ' '
       write (luninp, '(1X,A)') line
       line(1:79)  = ' '
    endif
    if (.not.sr%fshift) then
       line(1:10)  = 'OFF FSHIFT'
       line(11:79) = ' '
       write (luninp, '(1X,A)') line
       line(1:79)  = ' '
    endif
    if (dom%vegetation == 1) then
       line(1:10) = 'VEGETATION'
       write (line(15:), '(F6.2,1X,F6.4,1X,I4,1X,F6.4)') dom%veg_height, dom%veg_diamtr, dom%veg_nstems, dom%veg_drag
       write (luninp, '(1X,A)') line
       line(1:79) = ' '
    endif
    if (sr%modsim == 3) then
       write (luninp, '(1X,A)') 'PROP BSBT'
    endif
    !
    !     Numerical variables
    !
    line(1:47) = 'NUM DIR cdd=        SIGIM css=                 '
    write (line(14:19), '(F6.2)') cdd
    write (line(32:37), '(F6.2)') css
    line(48:79) = ' '
    write (luninp, '(1X,A)') line(1:79)
    line(1:79)  = ' '
    line(1:2)   = '$ '
    write (luninp, '(1X,A)') line
    line(1:79)  = ' '
    line(1:10)  = 'NUM ACCUR '
    if ( sr%modsim /= 3 ) then
        write (line(15:), '(F8.3,1X,F8.3,1X,F8.3,1X,F8.3,1X,I4)') &
        & sr%drel, sr%dh_abs, sr%dt_abs, sr%percwet, sr%itermx
    else
        write (line(15:), '(F8.3,1X,F8.3,1X,F8.3,1X,F8.3,1X,A,1X,I4)') &
        & sr%drel, sr%dh_abs, sr%dt_abs, sr%percwet, &
        & 'NONSTAT', sr%itermx
    endif
    write (luninp, '(1X,A)') line(1:79)
!-----------------------------------------------------------------------
    line(1:79)  = ' '
    !
    !     Obstacles
    !
    if (nobst>0) then
       l = 0
       do i = 1, nobst
          line(1:79) = ' '
          if (trane(i)<999.0) then
             line(1:11) = 'OBST TRANS '
             write (line(12:17), '(F6.3)') trane(i)
          else
             line(1:9)  = 'OBST DAM '
             write (line(10:16), '(F6.2,1X)') f(i)
             write (line(17:23), '(F6.2,1X)') ogam(i)
             write (line(24:30), '(F6.2,1X)') obet(i)
          endif
          if (sr%reflection(i) == 1) then
             write (line(31:44), '(a,F8.6)') ' REFL ',sr%refl_coeff(i)
             if (sr%refl_type(i) == 1) then
                write (line(45:50), '(a)') ' RSPEC'
             elseif (sr%refl_type(i) == 2) then
                !
                ! With SWAN version 40.51A, 'RDIFF 1 1 1' is replaced with 'RDIFF 1'
                ! Not backwards compatible!
                !
                write (line(45:52), '(a)') ' RDIFF 1'
             else
             !
             ! Wrong type indicator
             !
             endif
          endif
          line(57:62) = ' LIN _'
          write (luninp, '(1X,A)') line
          line(1:79)  = ' '
          do j = 1, nlin(i)
             line(1:79) = ' '
             l = l + 1
             write (line(13:70), '(2(E25.17,4X))') xpob(l), ypob(l)
             if (j/=nlin(i)) line(72:72) = '_'
             write (luninp, '(1X,A)') line
             line(1:79) = ' '
          enddo
       enddo
    endif
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
    line(1:79) = ' '
    line(1:72) = '$***************************** OUTPUT REQUEST **************************'
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    line(1:2)  = '$ '
    write (luninp, '(1X,A)') line
    do i=1, size(varnam1)
       write (luninp, '(1X,3A)') 'QUANTITY ',varnam1(i), ' excv=-999.0'
    enddo
    do i=1, size(varnam2)
       write (luninp, '(1X,3A)') 'QUANTITY ',varnam2(i), ' excv=-999.0'
    enddo
    if (allocated(sr%add_out_names)) then
       do i=1, size(sr%add_out_names)
          write (luninp, '(1X,3A)') 'QUANTITY ',sr%add_out_names(i), ' excv=-999.0'
       enddo
    endif
    write (luninp, '(1X,A)') '$ '
    line(1:79) = ' '
    !
    !     *** output definitions ***
    !
    !     definition of nested grid
    !
    if (nnest>1) then
       if (inest<nnest) then
          do kst = 2, nnest
             if (sr%dom(kst)%nestnr==inest) then
                line(1:7)             = 'POINTS '
                i                     = 8
                line(i:i)             = ''''''
                fname(1:5)            = 'NGRID'
                write(fname(6:8),'(I3.3)') kst
                line( 9:16)           = fname(1:8)
                i                     = 17
                line(i:i)             = ''''''
                line(18:23)           = ' FILE '
                i                     = 24
                line(i:i)             = ''''''
                line(25:31)           = 'SWANIN_'
                line(32:39)           = fname(1:8)
                i                     = 40
                line(i:i)             = ''''''
                line(41:79)           = ' '
                write (luninp, '(1X,A)') line
                line(1:79) = ' '
                line(1:6)  = 'SPEC _'
                line(7:79) = ' '
                write (luninp, '(1X,A)') line
                line(1:79) = ' '
                i          = 1
                line(i:i)  = ''''''
                line(2:9)  = fname(1:8)
                i          = 10
                line(i:i)  = ''''''
                line(12:22)= 'SPEC2D  ABS'
                i                = 24
                line(i:i)        = ''''''
                line(25:31)      = sr%dom(kst)%nesfil(1:7)
                i                = 32
                line(i:i)        = ''''''
                write (luninp, '(1X,A)') line
             endif
          enddo
       endif
    endif
    !
    ! Table output
    ! The length of a line in the SWAN output is limited to 360 characters.
    ! Splitting the output solves this problem.
    !
    line(1:6)   = 'TABLE '
    i           = 7
    line(i:i)   = ''''''
    line(8:15)  = 'COMPGRID'
    i           = 16
    line(i:i)   = ''''''
    line(17:24) = ' NOHEAD '
    i           = 25
    line(i:i)   = ''''''
    line(26:33) = 'SWANOUT1'
    i           = 34
    line(i:i)   = ''''''
    line(35:37) = '  _'
    line(38:79) = ' '
    write (luninp, '(1X,A)') line
    line(1:79)  = ' '
    write (luninp, '(6(2X,A),''_'')') varnam1
    line(1:79)  = ' '
    line(1:2)   = '$ '
    write (luninp, '(1X,A)') line
    !
    line(1:6)   = 'TABLE '
    i           = 7
    line(i:i)   = ''''''
    line(8:15)  = 'COMPGRID'
    i           = 16
    line(i:i)   = ''''''
    line(17:24) = ' NOHEAD '
    i           = 25
    line(i:i)   = ''''''
    line(26:33) = 'SWANOUT2'
    i           = 34
    line(i:i)   = ''''''
    line(35:37) = '  _'
    line(38:79) = ' '
    write (luninp, '(1X,A)') line
    line(1:79)  = ' '
    write (luninp, '(6(2X,A),''_'')') varnam2
    line(1:79)  = ' '
    line(1:2)   = '$ '
    write (luninp, '(1X,A)') line
    !
    if (allocated(sr%add_out_names)) then
       line(1:6)   = 'TABLE '
       i           = 7
       line(i:i)   = ''''''
       line(8:15)  = 'COMPGRID'
       i           = 16
       line(i:i)   = ''''''
       line(17:24) = ' NOHEAD '
       i           = 25
       line(i:i)   = ''''''
       line(26:33) = 'SWANOUT3'
       i           = 34
       line(i:i)   = ''''''
       line(35:37) = '  _'
       line(38:79) = ' '
       write (luninp, '(1X,A)') line
       line(1:79)  = ' '
       write (luninp, '(6(2X,A7),''_'')') sr%add_out_names
       line(1:79)  = ' '
       line(1:2)   = '$ '
       write (luninp, '(1X,A)') line
    endif
!-----------------------------------------------------------------------
    line(1:79)  = ' '
    !
    !     Curves
    !
    if (ncurv > 0) then
       k = 0
       do i = 1, ncurv
          line(1:79) = ' '
          line(1:7)  = 'CURVE  '
          j          = 8
          line(j:j)  = ''''''
          line(9:11) = 'cur'
          write (line(12:13), '(I2.2)') i
          write (line(14:15), '(I2.2)') inest
          j          = 16
          line(j:j)  = ''''''
          k          = k + 1
          write (line(21:48), '(2(F10.2,4X))') xpcu(k), ypcu(k)
          line(49:49) = '_'
          write (luninp, '(1X,A)') line
          line(1:79)  = ' '
          jendcrv     = nclin(k)
          do j = 2, jendcrv
             k = k + 1
             line(1:79) = ' '
             write (line(11:15), '(I5)') nclin(k)
             write (line(21:48), '(2(F10.2,4X))') xpcu(k), ypcu(k)
             !              Modification
             if (j/=jendcrv) line(50:50) = '_'
             write (luninp, '(1X,A)') line
             line(1:79) = ' '
          enddo
          line(1:2)  = '$ '
          write (luninp, '(1X,A)') line
          line(1:79) = ' '
          line(1:7)  = 'TABLE  '
          j          = 8
          line(j:j)  = ''''''
          line(9:11) = 'cur'
          write (line(12:13), '(I2.2)') i
          write (line(14:15), '(I2.2)') inest
          j          = 16
          line(j:j)   = ''''''
          line(21:26) = 'NOHEAD'
          line(31:31)   = ''''''
          line(32:39) = 'SWANOUT_'
          line(40:47) = line(9:16)
          line(51:51) = '_'
          write (luninp, '(1X,A)') line
          line(1:79) = ' '
          write (luninp, '(4(2X,A),A)') varnam1(11), varnam1(12), varnam1(13),     &
                                      & varnam1(4), ' _'
          write (luninp, '(5(2X,A),A)') varnam1(1), varnam1(3), varnam1(2),        &
                                      & varnam1(7), varnam1(8), ' _'
          write (luninp, '(2(2X,A))') varnam1(16), varnam1(5)
          line(1:79) = ' '
       enddo
    elseif (ncurv == -1) then
       !
       ! Output curves specified in a Tekal file
       ! Handle in a separate subroutine
       !
       write(*,'(1X,A)') ' Output curves are specified in a polyline file'
       call outputCurvesFromFile
    endif
    line(1:2) = '$ '
    write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
    !
    !     Output locations
    !
    line(1:79) = ' '
    !
    ! loop over the location files
    !
    if (sr%output_points) then
       do loc = 1, sr%nloc
          pointname                = ' '
          lc = len_trim(sr%pntfilnam(loc))
          if (lc>8) then 
             pointname(1:) = sr%pntfilnam(loc)(1:8)
          else
             pointname(1:)  = sr%pntfilnam(loc)
          endif
          line(1:7)       = 'POINTS '
          i               = 8
          line(i:i)       = ''''''
          line(i+1:)      = pointname
          i               = i+1+8
          line(i:i)       = ''''''
          line(i+1:)     = ' _'
          write (luninp, '(1X,A)') line
          line(1:79)      = ' '
          if (sr%output_pnt_file) then
             line(1:5) = 'FILE '
             i         = 6
             line(i:i) = ''''''
             line(7:)  = sr%pntfilnam(loc)
             ind       = index(sr%pntfilnam(loc), ' ')
             if (ind==0) ind = 8
             i         = ind - 1 + 7
             line(i:i) = ''''''
             write (luninp, '(1X,A)') line
             line(1:79) = ' '
          else
             do n = 1, npoints
                write (line(1:26),  '(E25.17,1X)') sr%xyloc(1,n)
                write (line(27:52), '(E25.17,1X)') sr%xyloc(2,n)
                if (n<npoints) then
                   line(54:55) = ' _'
                endif
                write (luninp, '(1X,A)') line
                line(1:79)     = ' '
             enddo
          endif
          line(1:2) = '$ '
          write (luninp, '(1X,A)') line
          line(1:79) = ' '
          if (sr%output_table) then
             line(1:6)  = 'TABLE '
             i          = 7
             line(i:i)  = ''''''
             line(i+1:) = pointname
             i          = i+1+8
             line(i:i)  = ''''''
             line(i+1:) = ' '
             line(i+2:) = 'HEAD '
             i          = i+7
             line(i:i)  = ''''''
             line(i+1:) = sr%pntfilnam(loc)
             i          = i+1+lc
             if (nnest>1) then
                line(i:) = 'n'
                write (line(i+1:), '(I1)') inest
                i = i+2
             endif
             if (nttide>1 .or. wavedata%mode /= stand_alone) then
                line(i:) = 't'
                if (nttide > 1) then
                    write (line(i+1:), '(I6.6)') 100000*inest + itide
                else  ! wavedata%mode /= stand_alone
                   write (line(i+1:), '(I3.3)') outcnt
                endif
                i = i+7
             endif
             line(i:)    = '.tab'
             i           = i+4
             line(i:i)   = ''''''
             line(i+1:) = ' XP YP DEP HS DIR RTP TM01 _'
             write (luninp, '(1X,A)') line
             line(1:79)  = ' '
             line(37:56) = 'DSPR UBOT WIND VEL  '
             write (luninp, '(1X,A)') line
          endif
          line(1:79) = ' '
          if (sr%output_spec1d) then
             line(1:6)  = 'SPEC  '
             i          = 7
             line(i:i)  = ''''''
             line(i+1:) = pointname
             i          = i+1+8
             line(i:i)  = ''''''
             line(i+1:) = ' '
             line(i+2:) = 'SPEC1D '
             i          = i+9
             line(i:i)  = ''''''
             line(i+1:) = sr%pntfilnam(loc)
             i          = i+1+lc
             if (nnest>1) then
                line(i:) = 'n'
                write (line(i+1:), '(I1)') inest
                i = i+2
             endif
             !
             ! Running online with Delft3D-FLOW: itide contains the output counter 
             !
             if (nttide>1 .or. wavedata%mode /= stand_alone) then
                line(i:) = 't'
                if (nttide > 1) then
                   write (line(i+1:), '(I3.3)') itide
                else  ! wavedata%mode /= stand_alone
                   write (line(i+1:), '(I3.3)') outcnt
                endif
                i = i+4
             endif
             line(i:) = '.sp1'
             i         = i+4
             line(i:i) = ''''''
             write (luninp, '(1X,A)') line
             line(1:79) = ' '
          endif
          if (sr%output_spec2d) then
             line(1:6) = 'SPEC  '
             i         = 7
             line(i:i) = ''''''
             line(8:10 + lc) = pointname
             i         = i+1+8
             line(i:i) = ''''''
             line(i+1:) = ' '
             line(i+2:) = 'SPEC2D '
             i         = i+9
             line(i:i) = ''''''
             line(i+1:) = sr%pntfilnam(loc)
             i         = i+1+8
             if (nnest>1) then
                line(i:) = 'n'
                write (line(i+1:), '(I1)') inest
                i = i+2
             endif
             !
             ! Running online with Delft3D-FLOW: itide contains the output counter 
             !
             if (nttide>1 .or. wavedata%mode /= stand_alone) then
                line(i:) = 't'
                if (nttide > 1) then
                   write (line(i+1:), '(I3.3)') itide
                else  ! wavedata%mode /= stand_alone
                   write (line(i+1:), '(I3.3)') outcnt
                endif
                i = i+4
             endif
             line(i:)  = '.sp2'
             i         = i+4
             line(i:i) = ''''''
             write (luninp, '(1X,A)') line
             line(1:79) = ' '
          endif
      enddo
    endif
    line(1:2) = '$ '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    ! frame not set anywhere!
    frame = .false.
    if (frame) then
       line(1:5) = 'FRAME'
       line(6:6) = ' '
       i = 7
       line(i:i) = ''''''
       line(8:14) = 'clframe'
       i = 15
       line(i:i) = ''''''
       line(16:16) = ' '
       write (line(17:25), '(F8.2 ,1X)') xpfr
       write (line(26:34), '(F8.2 ,1X)') ypfr
       write (line(35:43), '(F8.2 ,1X)') alpfr
       write (line(44:54), '(F10.2,1X)') xlenfr
       write (line(55:65), '(F10.2,1X)') ylenfr
       line(66:67) = '_ '
       write (luninp, '(1X,A)') line
       line(1:79) = ' '
       write (line(1:9), '(I4,1X,I4)') mxfr, myfr
       write (luninp, '(1X,A)') line
       line(1:79) = ' '
    endif
    line(1:79) = ' '
    line(1:2) = '$ '
    write (luninp, '(1X,A)') line
!-----------------------------------------------------------------------
    !
    !     Compute and test parameters
    !
    line(1:79) = ' '
    line(1:35) = 'TEST  ITEST=      ITRACE=          '
    write (line(14:16), '(I3)') itest
    write (line(27:29), '(I3)') itrace
    line(36:79) = ' '
    write (luninp, '(1X,A)') line
    line(1:79) = ' '
    !
    ! Default: put current time in writehottime
    ! writehottime will be overwritten by tendc when quasi-/non-stationary
    !
    sr%writehottime = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec)
    !
    ! 
    if (.not.sr%compmode) then
       line(1:1) = '$'
       line(2:79) = ' '
    else
       !
       ! modsim = 2   : quasi-stationary
       ! modsim = 3   : non-stationary
       !
       line(1:79) = ' '
       if (sr%modsim <= 1) then
          !
          ! stationary
          !
          line(1:7) = 'COMPUTE'
       elseif (sr%modsim == 2) then
          tendc = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec)
          write (line,'(A,1X,A)') 'COMPUTE STAT  ',tendc
          sr%writehottime = tendc
       elseif (sr%modsim == 3) then
          !
          ! non-stationary
          !
          ! starttime
          !
          tbegc = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec)
          !
          ! endtime
          !
          tendc = datetime_to_string(wavedata%time%refdate, wavedata%time%timsec + sr%deltcom * 60.0)
          !
          ! built line
          !
          line(1:79)  = ' '
          line(1:16)  = 'COMPUTE NONSTAT '
          write (line(17:31), '(a)')    tbegc
          write (line(33:40), '(f8.2)') sr%deltc
          line(41:44) = ' MIN'
          write (line(46:61), '(a)')    tendc
          sr%writehottime = tendc
       else
       endif
    endif
    write (luninp, '(1X,A)') line
    !
    ! hotstart:
    ! hotfile= true: use hotfile
    ! modsim = 2   : quasi-stationary
    ! modsim = 3   : non-stationary
    !
    if (sr%hotfile) then
       !
       ! line to ensure that SWAN is going to produce a hotfile
       !
       write (fname,'(a,i0,2a)') 'hot_', inest, '_', trim(sr%writehottime)
       line  = 'HOTF ''' // trim(fname) // ''''
       write (luninp, '(1X,A)') line
    endif
    !
    line(1:79)  = ' '
    line(1:4)   = 'STOP'
    line(5:79)  = ' '
    write (luninp, '(1X,A)') line
    line(1:79)  = ' '
    close (luninp)
!
!
!
contains
!
!
!===============================================================================
subroutine outputCurvesFromFile()
    use precision_basics
    integer                     :: i,j,istat
    real(sp)             , dimension(1:2) :: inputvals
    character(1), pointer, dimension(:)   :: data_ptr
    character(30)               :: node_type
    character(30)               :: parname
    character(80)               :: curname
    character(80)               :: line
    type(tree_data)   , pointer :: cur_ptr
    type(tree_data)   , pointer :: pol_ptr
    type(tree_data)   , pointer :: tmp_ptr

    nullify(pol_ptr)
    call tree_create('Delft3D-WAVE output curves', pol_ptr)
    istat = 0
    call prop_file('tekal',trim(sr%curvefil),pol_ptr,istat)
    if (istat /= 0) then
       select case (istat)
       case(1)
          write(*,*) '*** ERROR File: '//trim(sr%curvefil)//' not found'
       case(3)
          write(*,*) '*** ERROR Premature EOF in file: '//trim(sr%curvefil)
       case default
          write(*,*) '*** ERROR Read error from file: '//trim(sr%curvefil)
       endselect
       stop
    endif
    !
    ! if no line exists in the polyline file
    !
    if(.not. associated(pol_ptr%child_nodes) ) then
        write(*,'(1X,A)') ' Error! 0 output curve is specified in the polyline file!'
        stop
    endif
    do i = 1,size(pol_ptr%child_nodes)
       cur_ptr => pol_ptr%child_nodes(i)%node_ptr
       curname = tree_get_name(cur_ptr)
       write(luninp,'(1x,3a)') 'CURVE  ''', trim(curname), '''  _'
       do j = 1,size(cur_ptr%child_nodes)
          tmp_ptr => cur_ptr%child_nodes(j)%node_ptr

          write (parname,'(a,i0)') 'row_', j
          ! call tree_get_node_by_name( polygon_ptr, parname, node_ptr )
          call tree_get_data_ptr( tmp_ptr, data_ptr, node_type )
          !
          ! inputvals is of type real(fp)
          ! the data to be retrieved is in real(sp)
          ! call transfer with a real(sp) constant as second parameter
          !
          inputvals = transfer( data_ptr, 0., 2 )
          write(line,'(18x,2(f10.2,4x))') inputvals(1), inputvals(2)
          if (j /= 1) then
             line(14:14) = '1'
          endif
          if (j < size(cur_ptr%child_nodes)) then
             write(line,'(2a)') trim(line), '    _'
          endif
          write(luninp,'(a)') line
       enddo
       write(luninp,'(1x,a)') '$ '
       write(luninp,'(1x,5a)') 'TABLE  ''', trim(curname), '''    NOHEAD    ''SWANOUT_', trim(curname), '''   _'
       write(luninp, '(4(2X,A),A)') varnam1(11), varnam1(12), varnam1(13),     &
                                  & varnam1(4), ' _'
       write(luninp, '(5(2X,A),A)') varnam1(1), varnam1(3), varnam1(2),        &
                                  & varnam1(7), varnam1(8), ' _'
       write(luninp, '(2(2X,A))') varnam1(16), varnam1(5)
    enddo
end subroutine outputCurvesFromFile


end subroutine write_swan_inp
!
!
!===============================================================================
subroutine adjustinput(sr)
    use properties
    implicit none
    !
    type(swan)                  :: sr
    !
    character(256)              :: filnam
    character(256)              :: parname
    integer                     :: i
    integer                     :: in
    integer                     :: istat
    type(tree_data)   , pointer :: domain_ptr
    type(tree_data)   , pointer :: input_tree
    type(swan_dom)    , pointer :: dom
    !
    filnam = TRIM(sr%filnam) // '.opt'
    !
    ! Create input tree
    !
    call tree_create  ( "Delft3D-WAVE input", input_tree )
    istat = 0
    call prop_file('ini',trim(filnam),input_tree,istat)
    if (istat /= 0) return
    !
    sr%append_com = .false.
    call prop_get_logical(input_tree, '*', 'AppendCOM'  , sr%append_com)
    call prop_get_logical(input_tree, '*', 'checkVersionNumber'  , sr%checkVersionNumber)
    if (.not.sr%checkVersionNumber) then
       write(*,'(a)') '*** MESSAGE: The check on the SWAN version number is disabled'
    endif
    !
    do i = 1,size(input_tree%child_nodes)
       !
       ! Does input_tree contain a child with name 'domain'?
       !
       domain_ptr => input_tree%child_nodes(i)%node_ptr
       parname = tree_get_name( domain_ptr )
       if ( parname /= 'domain') cycle
       parname = ''
       call prop_get_string(domain_ptr, '*', 'Grid', parname)
       !
       do in = 1, sr%nnest
          if (sr%dom(in)%curlif == parname) exit
       enddo
       if (in > sr%nnest) cycle
       dom => sr%dom(in)
       !
       call prop_get_integer(domain_ptr, '*', 'FlowBedLevel'  , dom%qextnd(q_bath))
       call prop_get_integer(domain_ptr, '*', 'FlowWaterLevel', dom%qextnd(q_wl)  )
       call prop_get_integer(domain_ptr, '*', 'FlowVelocity'  , dom%qextnd(q_cur) )
       call prop_get_integer(domain_ptr, '*', 'FlowWind'      , dom%qextnd(q_wind))
       !
    enddo
    !
end subroutine adjustinput

end module swan_input
