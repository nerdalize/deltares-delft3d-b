module rdparm_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision              ! single and double precision
use timers
use fileinfo               ! file information for all input/output files
!
!  module procedure(s)
!
use openfl_mod             ! explicit interface
use strip_mod              ! explicit interface
use stop_exit_mod          ! explicit interface
!
implicit none              ! force explicit typing
!
contains
      subroutine rdparm ( lun1   , lnam1   , lun2   , title   , subst  ,    &
                          nopart , rough   , drand  , wveloa  , wdira  ,    &
                          itstrt , itstop  , idelt  , icwsta  , icwsto ,    &
                          icwste , iyear   , imonth , iofset  , ipset  ,    &
                          recovr , window  , xwaste , ywaste  , zwaste ,    &
                          radius , iwtime  , amassd , ictime  , ictmax ,    &
                          decay  , amassc  , idtime , iddtim  , stickdf,    &
                          modtyp , pblay   , wparm  , nocons  , ioptrad,    &
                          const  , nosubs  , nowind , iwndtm  , nolay  ,    &
                          ndprt  , ftime   , stoch  , nmstat  , xstat  ,    &
                          ystat  , ihstep  , ihstrt , ihstop  , substi ,    &
                          kwaste , itrack  , vsfour , ivtime  , ntrack ,    &
                          tcktot , nplot   , mapsub , uscal   , notrak ,    &
                          ldiffz , ldiffh  , lcorr  , ipc     , nosubc ,    &
                          alpha  , ihdel   , subst2 , isubud  , finud  ,    &
                          iftime , ifopt   , nosud  , isfud   , iutime ,    &
                          nmap   , mmap    , finnh4 , finno3  , nmdyer ,    &
                          nmconr , nfract  , lsettl , taucs   , tauce  ,    &
                          chezy  , noslay  , mstick , nstick  , ioptdv ,    &
                          cdisp  , filvers , irfac  , anfac   , linear ,    &
                          ini_opt, ini_file, rhow   , nodye   , nocont ,    &
                          noudef , nosta   , iptset , ivtset  , idtset)
!
!
!     READING PART INPUT FILE (*.inp)
!         (initially)
!
!
!     system administration : r.j. vos (since 1 -1 -94)
!
!
!     created               : february 1990, by l. postma
!
!
!     updated               : december 2000, by leo postma
!                               * added 5th component to vsfour (minimum
!                                 settling velocity)
!                               * added settling velocity depending on
!                                 power of the concentration (anfac)
!                               * added refined plotgrid for concentration
!                                 dependent settling velocity (power<>0.0)
!                                 irfac = 1 means no refinement
!                           : january 1991, by andre hendriks, added zwaste,
!                                extended drand and added oil parameters
!                           : february 1991, by a. markus, added wwaste
!                           : march 1991, by a. markus, added second name
!                                deleted drand extensions, oil param.
!                           : july  1992, by r.j. vos
!                                deleted wparm extensions
!                                added decay for 8 subst.
!                                filename set to char*255
!                                number of particles per load required
!                                wparm used as percentage of the total
!                                number of particles (per load)
!                           : january 1993, by r.j. vos
!                                history input
!                           : may   1993, by m. zeeuw
!                              - ipset now controlled on ascending order
!                              - ipset now controlled on precise matching
!                                with output of map output. this is needed
!                                within cms while delpar rounds of non
!                                matching time to the nearest possible,
!                                while cms administrates the intended times
!                                in the database. whenever one tries to
!                                visiualize this data the commandgenerator
!                                can't find exact data, and generates the
!                                first data found what so ever.
!                           : june  1993, by m. zeeuw
!                              - ictime now controlled on precise matching
!                                with delpar time serie. this is needed
!                                while the combination of part08, part09
!                                and part13 can't integrate correctly, when
!                                the latter isn't the case.
!                           : may 1994, by r.j. vos
!                                start and stop time for time histories required
!                           : june 1994, by r.j. vos
!                                steering parameter for psf introduced (slim)
!                           : june 1995, by r.j. vos
!                                psf has its own time points and recovery rates
!                           : 3d version 1996
!                                determines particle track number
!                                read layer thickness
!                           : july 1996, allow for particle tracking plot
!                           : july 1996, rjvos, red tide model
!                             modtyp =3 is red tide model, 5 substances at least
!                             modtyp =4 is oil model
!                             also user defined releases are given
!                             mapsub relates substances to positions
!                             concentration array like delwaq, two layer model gets 2 subs.
!                             alpha added to scale vertical diffusivity
!                           : sept.1996: adapted initial. substi for partplot
!                           : sept.1996, v3.20
!                             idelt is now read form input, interpolation is
!                             allowed....(like delwaq -2 option)
!                           : okt .1996, v3.20
!                             for tpg use only 10 characters for substance names
!                           : july 1997, v3.30
!                             - thickness is now calculated..
!                             - ud release follows directly from delwaq files
!                             - zoom window size may be varied
!                             - two options for psf (may run faster)
!                           : aug 1997, v3.30, file option ud rel for filename
!                             added names for releases (cont+dye)
!                             added filenames for red tide model
!                             added string hyd file ui delft3d (not used by delpar)
!                             added substance number for 2nd subs.temp.
!                           : sept97, check on zero icwste and ihstep
!                           : v3.4 nov 97, oil included (modtyp=4)
!                                          uses openfl
!                             dec 97 changed format 2338 for settling velocities
!                           : 12 march 1998: lnam1 must be char * (*) and not char*40 for delft3d !!
!                             (found for obayashi)
!                           : v3.43 with sedimentation/erosion, + correc. declarations of logicals
!                           : v3.50 sticking material added (sept 1998)
!                                   set defaults for chezy and rhow
!                           : v3.60 added 3d temperature model (modtyp = 5)
!                                   added constant vertical dispersion
!                                   added error messages for parameter reading
!                                   list of error messages was completed on 6/4/99
!                                   oil now always has sticking substance, other substances might also be sticky
!                           : v3.60 acceptance tests: changed f9.2 into
!                                   f11.2 for (x,y) co-ordinates
!                           : v3.60 correction for oil spill model done
!                                   for parameter zwaste
!
!
!     note                  : - within delpar, this routine is invocated
!                               only once, so no value should be saved.
!                               suppress compile variables and/or
!                               options  for enabling static memory in
!                               stead of dynamically memory. also
!                               dynamic memory allocation is assumed
!                               default for this routine.
!                             - for settling (lsettl=.true.) an extra layer is created
!                               for the bed, the number of layers for output is then noslay
!                             - the 3d temeparature model is done with partwq as well
!
!
!     logical unit numbers  : lun1   - stripped user input file
!                             lun2   - output log file
!                             lun3   - user input file
!
!
!     subroutines called    : strip  - strips the user input file
!                             stop_exit
!                             openfl
!
!
!     parameters            :
!
!     name    kind     length     funct.  description
!     ====    ====     ======     ======  ===========
!     alpha   real        1       output  scale factor for vertcial diffusivity
!     amassc  real  nocont*ictmax output  masses at the breaks / second
!                     *nosubx
!     amassd  real  nosubx*nodye output  masses of dye released
!     anfac   real   1   output  v*c**anfac is settling velocity
!     cdisp   real        1       output  constant vertical diffusivity constant
!     chezy   real        1       output  chezy-coefficient
!     chydfl  char       80       local   character-string for hyd file for ui delft3d
!     const   real      nocons    output  user-defined constants
!     decay   real     ictmax     output  decay value at the breaks
!     drand   real      norand    output  random step parameters
!     finnh4  char*256    1       in/out  filenames of NH4 file red tide model
!     finno3  char*256    1       in/out  filenames of NO3 file red tide model
!     finud   char*256  noudef    in/out  filenames of ud delwaq files
!     ftime   real  ictmax*nocont output  time matrix for wasteloads (1/s)
!     ictime  integer  ictmac     output  breakpoint time values
!     ictmax  integer     1       in/out  maximum number of breakpoints
!     icwsta  integer     1       output  map file start time
!     icwste  integer     1       output  map file time step
!     icwsto  integer     1       output  map file stop time
!     iddtim  integer     1       output  delwaq time delay
!     idelt   integer     1       output  simulation time step inputfile
!     idtime  integer  idtmax     output  breakpoint times, decays
!     iftime  integer   noudef    in/out  ud rel. reading times from files
!     ifopt   integer   noudef    in/out  file option ud release
!     idtmax  integer     1       in/out  maximum nr of breaks decays
!     ihdel   integer     1       input   time step hydro.dyn files
!     ihstep  integer     1       output  time-step on history file
!     ihstop  integer     1       output  stop time-histories
!     ihstrt  integer     1       input   start time-histories
!     imonth  integer     1       output  month offset to calendar
!     iofset  integer     1       output  seconds offset to calendar
!     ioptdv  integer     1       output  vertical diffusion option
!     ioptpf  integer     1       output  psf zoom window option
!     ipc     integer     1       input   choice numerical scheme
!     ipoil   integer     1       output  oil-indicator mass/m^2 or mass/m^3
!     ipset   integer  iptmax     output  times for a plot file
!     irfac   integer     1       output  refinement factor plotgrid for
!                                         anfac <> 0.0
!     isfud   integer   nosubs    in/out  index array for subst. from files ud rel.
!     isubud  integer   nosubs    in/out  index array for substances for user defined releases
!     itrack  integer     1       output  substance for particle track
!     itstop  integer     1       output  simulation stop
!     itstrt  integer     1       input   simulation start
!     iutime  integer  noudef     output  ud release times for delpar
!     ivtime  integer  ivtmax     output  times of the settling velocities
!     ivtmax  integer     1       output  number breakpoints settling velocities
!     iwndtm  integer  nowinx     output  time on of wind changes
!     iwtime  integer  nodye     output  times of the instantaneous releases
!     iyear   integer     1       output  year offset to calendar
!     lcorr   logical     1       output  switch for predcorrector scheme
!     ldiffh  logical     1       output  switch for hor. diffusion
!     ldiffz  logical     1       output  switch for vert.diffusion
!     lsettl  logical     1       output  switch for sedimentation erosion
!     lnam1   char*256    1       input   file name of lun1
!     lun1    integer     1       input   unit number 1
!     lun2    integer     1       input   unit number 2
!     kwaste  real  nocont+nodye output  k values of the releases
!     linear  integer   nocont    input   0 = block interpolated loads
!                                         1 = linear interpolated loads
!     mapsub  integer  nosubs*2   output  map for substance numbers
!     mmap    integer     1       in/out  y dimension of zoom grid
!     modtyp  integer     1       output  model-run-type
!     mstick  integer   nosubs    output  array that tells if a substance i is sticking
!                                         if it sticks then mstick(i) = j with j > 0
!                                         the number j gives the source for this sticking substance
!                                         (for j = 0 substance i does not stick)
!                                         (for j < 0 substance i itself is sticking)
!     ndprt   real  nocont+nodye output  number of particles per
!                                         load (instantaneous and continuous)
!     nfract  integer     1       output  no of oil fractions
!     nfracmx  integer    1       local   max nr of oil fractions
!     njumph  integer     1       output  frequency for history file
!     nmap    integer     1       in/out  x dimension of zoom grid
!     nmdyer  char*20  nostmx     output  names of instantaneous releases
!     nmconr  char*20  nostmx     output  names of cont. releases
!     nmstat  char*20  nostmx     output  names of mon. stations
!     nocont  integer     1       in/out  max nr of continuous points
!     nocons  integer     1       in/out  number of constants
!     nodye  integer     1       in/out  max nr of instantaneous release points
!     nolay   real        1       in/out  number of layers for water phase
!     noslay  real        1       in/out  number of layers for water phase and bed
!                                         for lsettl = .true. noslay = nolay + 1
!     nopam   integer     1       output  number of parameters
!     nopart  integer     1       in/out  number of particles
!     nosta   integer     1       output  number of monitoring stations
!     nostmx  integer     1       output  max. number of mon. stations
!     nosubs  integer     1       output  number of substances
!     nosubc  integer     1       output  leading dimension conc.array
!     nosud   integer    noudef   in/out  number of subst. on file for ud release
!                                         only required when ifopt=1
!     notrak  integer     1       input   nr of particle tracks
!     noudef  integer     1       input   nr of user defined releases
!     nowind  integer     1       output  number of wind variabilities
!     nplot   integer   notrak    output  seq. ordered particle numbers for tracks
!     nstick  integer   nosubs    output  number of sticking substances
!     ntrack  integer     1       output  substance for number of particles per bin
!     oiltyp  char.20     1       output  sort of oil
!     pblay   real        1       in/out  relative thickness lower layer
!     radius  real  nocont+nodye output  radius of the load
!     recovr  real     iptmax     output  recovery rate for each picture
!     rough   real        1       output  roughness length
!     rhow    real        1       output  density of water in g/l (= kg/m3)
!     stoch   real  nosubx*nocont output  stoechiometry matrix per load
!     slim    real  nosubx*nocont output  steering parameter psf's
      integer(ip), intent(  out) :: stickdf         ! sticking at drying flats
!     subst   char*20   nosubs    output  substances' names plo file
!     substi  char*20   nosubs    output  substances' names input file
!     tauce   real        1       output   critical shear stress for erosion (pa)
!     taucs   real        1       output   critical shear stress for sedimentation (pa)
!     tcktot  real       nolay    in/out  layer thickness,set to 1 for modtyp = 2 or 2d model
!     title   char*40     4       output  simulation title
!     uscal   real      noudef    output  scale values ud releases
!     vsfour  real  4*ivtmax*nosubs output  fourier components at breakpoints
!     wdira   real      nowind    output  wind direction degree from north
!     window  real        4       output  plot window
!     wveloa  real      nowind    output  wind velocity  m/s
!     xstat   real      nostmx    output  x values of mon. stations
!     ystat   real      nostmx    output  y values of mon. stations
!     xwaste  real      nodye    output  x values of the points
!     ywaste  real      nodye    output  y values of the points
!     zwaste  real      nodye    output  relative z values of the points
!     ----    ----     ------     ------  -----------
!     ascal   real      nocont    local   work array for scale values
!     car     char*      npos     local   help buffer
!     dr1     real        1       local   random step parameter 1
!     dr2     real        1       local   random step parameter 2
!     i       integer     1       local   index pointer
!     i1      integer     1       local   index pointer 1
!     i2      integer     1       local   index pointer 2
!     ic      integer     1       local   continuous load index pointer
!     icount  integer     1       local   help counter variable
!     ictset  integer     1       local   time points cont's
!     id      integer     1       local   instantaneous load index pointer
!     idtset  integer     1       local   time points instantaneous's
!     ierr    integer     1       local   simulation stop
!     ih      integer     1       local   help var. hour
!     im      integer     1       local   help var. minute
!     ir      integer     1       local   help variable
!     is      integer     1       local   help var. seconds
!     isub    integer     1       local   index pointer substances
!     it      integer     1       local   help variable
!     itime   integer     1       local   time
!     itst2   integer     1       local   help var. for time
!     ivdif   integer     1       local   help var. vert. diffusivity
!     iwopt   integer     1       local   number of wind variations
!     j       integer     1       local   index pointer
!     k       integer     1       local   index pointer
!     ki      integer     1       local   help variable
!     ks      integer     1       local   help variable
!     kt      integer     1       local   help variable
!     noc     integer     1       local   nr of continuous points
!     nocont  integer     1       local   number of continuous releases
!     nodye   integer     1       local   nr of instantaneous release points
!     notime  integer     1       local   number of plot times
!     np      integer     1       local   number of particles
!     wparm   real  nocont+nodye local   scale for no. particles per load
!                     +noudef
!     xw1f    real        1       local   x window 1
!     xw2f    real        1       local   x window 2
!     yw1f    real        1       local   y window 1
!     yw2f    real        1       local   y window 2

!     parameters
!
      integer , parameter   :: npos   =200,  lun3   = 97
!
!     declarations, as in global dat pointer module
!
      character(len=   *), pointer, dimension(:) :: subst2
      character(len=   *), pointer, dimension(:) :: nmstat
      character(len=   *), pointer, dimension(:) :: subst
      character(len=   *), pointer, dimension(:) :: nmdyer
      character(len=   *), pointer, dimension(:) :: nmconr
      character(len=   *), pointer, dimension(:) :: title
      character(len=   *), pointer, dimension(:) :: substi
!      
      character(len=   *) :: lnam1
      character(len=npos) :: car

!
!     parameters
!     file version of input file; required to process input files
!     related to different part versions. (to handle extra input items)
!     input files are identified by the label on the first input
!     line. (formerly this was a fixed, rather obsolete label)
!
      character(len=   *) :: filvers
      character(len= 256) :: chydfl
      character(len=   *) :: ini_file
!
!     dimensioning
!
      real   (sp), pointer, dimension(:) :: const , drand  , radius, recovr , &
                                            wdira , window , wparm , wveloa, xstat  , &
                                            xwaste, ystat  , ywaste, zwaste, tcktot , &
                                            uscal
!
      real   (sp), pointer, dimension(:,:)     :: amassd, decay
      real   (sp), pointer, dimension(:,:)     :: ftime
      real   (sp), pointer, dimension(:,:)     :: stoch
      real   (sp), pointer, dimension(:,:,:)   :: amassc
      real   (sp), pointer, dimension(:,:,:)   :: vsfour

      integer(ip), pointer, dimension(:)       :: idtime, ipset , iwndtm, iwtime, ndprt  ,  &
                                                  kwaste, ivtime, mapsub, nplot , mstick
      integer(ip), pointer, dimension(:)       :: ioptrad
      integer(ip), pointer, dimension(:)       :: linear    ,ictmax
      integer(ip), pointer, dimension(:,:)     :: ictime

!
!.. special for user defined releases version 3.30
!
      integer(ip), pointer, dimension(:)       :: iftime, ifopt , nosud, isubud, isfud
      integer(ip), pointer, dimension(:)       :: iutime

      character(len=*), pointer, dimension(:)  ::  finud
      character(len=*)                         ::  finnh4, finno3
!
!..   added for verson 3.43, partially corrections !!
!
      logical :: lcorr, ldiffh , ldiffz , lsettl
      logical :: oil  , oil2dh , oil3d

      character(len=10),dimension(0:1) :: option

      integer(ip), allocatable, dimension(:,:) :: dhms
      real(sp)   , allocatable, dimension(:  ) :: ascal
!
      real(sp) ::  eps = 1.e-4
!
!     local scalars
!
      integer(ip) :: i      , i1      , ic      , icwsta  , icwste  , icwsto
      integer(ip) :: id     , iddtim  , idelt   , idtset  , ierr    , ibrk
      integer(ip) :: ifrac  , ifract  , ih      , ihdel   , ihstep  , ihstop , ihstrt
      integer(ip) :: ilay   , im      , imonth  , ini_opt , int     , iofset , iopsed
      integer(ip) :: iopt   , ioptdd  , ioptdv  , ipc     , ipos    , lenstr
      integer(ip) :: iq     , ir      , irfac   , is      , isub    , it     , itime
      integer(ip) :: itrack , itst2   , itstop  , itstrt  , iptset
      integer(ip) :: ivtset , iwopt   , iyear   , j       , jsub    , k      , ks
      integer(ip) :: ln     , lun1    , lun2    , lunin   , max
      integer(ip) :: mmap   , mod     , modtyp  , nfract  , ninpol
      integer(ip) :: nmap   , noc     , nocons  , nocont  , nodac
      integer(ip) :: nodye  , nolay   , nopart  , noslay  , nosta
      integer(ip) :: nosubc , nosube  , nosubs  , notime  , notrak
      integer(ip) :: noudef , nowind  , np      , nstick  , ntrack , nvsfour
      real   (sp) :: alpha  , anfac   , angle   , c1      , c2
      real   (sp) :: cdisp  , chezy   , density , dr1     , dr2
      real   (sp) :: dxwin  , dywin   , evemul  , fstick  , hmin
      real   (sp) :: rhow   , rough   , sqrt    , tauce   , taucs
      real   (sp) :: viscos , volfrac , pblay
      real   (sp) :: xw1f   , xw2f    , yw1f    , yw2f
!
      integer(ip) nfcons             ! parameters per oil fraction
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "rdparm", ithndl )
!
!     initialize error counter
!
      ierr   = 0
!
      option(0)='block'
      option(1)='linear'
!
!     open input file
!
      open(lun3, file = lnam1)
!
!     open input work-file
!
      open(lun1, file = 'particle.wrk')
!
!     strip the comments
!
      call strip (lun3  , lnam1 , lun1  , lun2  ,   &
                  npos  , ';'   , car   )
!
!     close input file
!
      close(lun3)
!
!     print one rule in logging-file
!
      write(lun2 , *)
!
!======================================================================
!
!     read the file version for input file
!     (replaces obsolete file label)
!
      read (lun1 , * , err=4001) filvers
      if (filvers(1:19)=='delpar_version_3.60') then
         filvers(1:19)='v3.60.00'
      endif

      if (filvers(2:19) < '3.66.00') then
         write(*   ,'(2x,a  )') ' Obsolete input file '
         write(*   ,'(2x,a,a)') ' Version found             : ',filvers(1:19)
         write(*   ,'(2x,a,a)') ' Lowest version requested  : ','V3.66.00'
         write(lun2,'(2x,a  )') ' Obsolete input file '
         write(lun2,'(2x,a  )') ' Version found             : ',filvers(1:19)
         write(lun2,'(2x,a  )') ' Lowest version requested  : ','V3.66.00'
         stop
      endif
!
!======================================================================
!
!     read the titles
!
      read (lun1 ,    *,err=4002) title(1), title(2), title(3), title(4)
!
      write(lun2 , 2000) title(1)
      write(*    , 2000) title(1)
!
!======================================================================
!
!     read the name of the hyd file for ui delft3d
!
      read (lun1 ,    *,err=4003) chydfl
!
      write(lun2 , 1997) chydfl
      write(*    , 1997) chydfl
!
!======================================================================
!
!     read modeltype , and number of tracks,
!     extra number of output substances and option for sed/erosion
!
      nosube = 0
      read (lun1,  *, err =11 ) modtyp, notrak, nosube, iopsed
      goto 12
11    continue
      write(*,*) ' Error when reading the model type '
      write(*,*) ' Is this version 3.50?'
      call stop_exit(1)
12    continue
!
      if(iopsed==0) then
         lsettl = .false.
         write(lun2,*) ' Sedimentation-erosion processes are inactive'
      else
         lsettl = .true.
         write(lun2,*) ' Sedimentation-erosion processes are enabled'
      endif
!======================================================================
!
!     adapt variables for number of substances and layer-thickness
!
      mapsub = 0 ! whole array assignment
      write(*,*) ' Number of layers            : ', nolay

      if (notrak==0) then
         write(lun2,'(a)')  &
              '  Particle tracks not written to tracking file'
      else
         write(lun2,'(a)')      &
              '   Particle tracks written to tracking file'
      endif
      oil    = modtyp==4
      oil2dh = oil .and. nolay == 1
      oil3d  = oil .and. nolay  > 1
!
!======================================================================
!
!    read numerical parameters
!
      read(lun1,*,err=4007) ipc, idelt
      if(idelt <= 0) idelt = ihdel
      write(lun2,*) '   '
      write(lun2,*) ' Numerical scheme            : ',ipc
      write(lun2,*) '   '
      write(lun2,*) ' Time step for hydrodynamics : ',ihdel,' seconds '
      write(lun2,*) ' Time step for part. tracking: ',idelt,' seconds '
      write(lun2,*) '   '
      write(*   ,*) '   '
      write(*   ,*) ' Numerical scheme            : ',ipc
      write(*   ,*) '   '
      write(*   ,*) ' Time step for hydrodynamics : ',ihdel,' seconds '
      write(*   ,*) ' Time step for part. tracking: ',idelt,' seconds '
      write(*   ,*) '   '
      if(ihdel <idelt) then
         write(lun2,2023)
         call stop_exit(1)
      endif
      ninpol = ihdel/idelt
      if(ihdel /= (ninpol*idelt)) then
         write(lun2,2022)
         write(*   ,2022)
         call stop_exit(1)
      endif
      if(ipc <= 1) then
         lcorr = .false.
         write(lun2,*) ' Predictor corrector scheme is not used '
      else
         lcorr = .true.
         write(lun2,*) ' Predictor corrector scheme is used '
      endif
!
!======================================================================
!
!    read vertical diffusivity parameters
!
      read(lun1,*,err=4008) ioptdv, alpha, cdisp
      if(ioptdv==1.and.alpha==(1.0)) then
       write(lun2,*) '   '
       write(lun2,*)   &
         ' Vertical diffusion from depth-averaged algebraic model'
       write(lun2,*) '   '
       ldiffz = .true.
      elseif(ioptdv==1.and.alpha > (0.0)) then
       write(lun2,*) '   '
       write(lun2,*)   &
         ' Vertical diffusion from depth-averaged algebraic model'
       write(lun2,*)   &
         ' Scale factor for diffusion : ',alpha
       write(lun2,*) '   '
       ldiffz = .true.
      elseif(ioptdv==0.and.alpha > (0.0)) then
       write(lun2,*) '   '
       write(lun2,*) ' Vertical diff. :',cdisp,' m2/s'
       write(lun2,*) ' Scale factor   : ',alpha
       write(lun2,*) '   '
       ldiffz = .true.
      elseif(alpha==(0.0)) then
       write(lun2,*) '   '
       write(lun2,*) ' Vertical diffusivity  is zero  '
       write(lun2,*) '   '
       ldiffz = .false.
      elseif(ioptdv <= 0.or.ioptdv > 1) then
       write(lun2,*) ' Error: this vert.diff. option is not valid'
       write(*   ,*) ' Error: this vert.diff. option is not valid'
       call stop_exit(1)
      endif
      write(lun2,*) '   '
!
!3d.. layer thickness 3d models
!
      if(nolay > 1.and.modtyp /= 2) then
         write(lun2,3115)
         do  27 ilay = 1, nolay
!
!    tcktot is made in subroutine rdlgri with deepest segment
!
            write(lun2,3120) ilay,tcktot(ilay)
  27     continue
      else
         tcktot(1) = 1.0
      endif
!======================================================================
!
!  read substance names
!
      if (oil) then
         read (lun1 ,    *,err=4009) nosubs, nfract
      else
         read (lun1 ,    *,err=4009) nosubs
      endif
!
      read (lun1 ,    *,err=4009) (substi(i), i = 1, nosubs)
      write(*   ,*     ) ' Modelled substances : '
      write(lun2,'(//a)') ' Modelled substances : '
      do 281 i = 1, nosubs
         write(*   ,'(12x,a20)') substi(i)
         write(lun2,'(12x,a20)') substi(i)
281   continue
!
!.. modtyp = 1 or 2 refers to 2d hydrodynamics
!
      if     (modtyp == 1) then
!
        write(*,*) ' You are using the tracer model '
        pblay  = 0.0
        nosubc = nosubs
      elseif (modtyp == 2) then
!
!.. temperature model for 2-layer option.........
!.. heat diffsuion with finite volume method.....
!.. between two particle models..................
!
        write(*,*) ' You are using the two-layer temperature model'
        nolay = 2
        nosubc= nolay*nosubs
!
!.. substance number 1 must be temperature
!.. substance 2 is temperature for bottom layer
!
        mapsub(1) = 1
        mapsub(2) = 2
      elseif (modtyp == 3) then
!
!.. red tide  module.............................
!
        write(*,*) ' You are using the red tide model '
        nosubc = nosubs
      elseif (modtyp == 4) then
!
!.. oil module...................................
!
        nosubc = nosubs
        if(nosubs <3) then
           write(lun2,*) ' For oil module at least 3 substances '
           write(lun2,*) '(floating, dispersed and sticking oil)'
           write(*   ,*) ' For oil module at least 3 substances '
           write(*   ,*) '(floating, dispersed and sticking oil)'
           ierr = ierr + 1
        endif
!
        if(nfract*3 > nosubs) then
           write(lun2,*) ' For oil module at least 3 subst per fraction'
           write(*   ,*) ' For oil module at least 3 subst per fraction'
           ierr = ierr + 1
        endif
!
        do isub = 1, nfract*3
           mapsub(isub) = isub
        enddo
        write(*,'(//)')
        write(*,*) ' You are using the oil model '
        write(*,'(//)')
        write(*,*) ' Number of oil fractions     : ', nfract
!
      elseif     (modtyp == 5) then
!
!.. temperature module for 2d or 3d without 2 layer option.............
!
        pblay  = 0.0
        nosubc = nosubs
        if(nolay==1) then
           write(*,*) ' You are using the 2d temperature model '
        else
           write(*,*) ' You are using the 3d temperature model '
        endif
!.. substance number 1 must be temperature
        mapsub(1) = 1
      else
!
!       modeltype not supported
!
        write(lun2 , 2015) modtyp
        nolay  = 1
        ierr = ierr + 1
      endif
!
!======================================================================
!
!  detect particle tracks and detect sticking substances
!
!  sticking substances are those substances that stick to
!  land, most often this is oil
!  if substance (i) can stick then mstick(i) > 0
!  the number gives the source for this sticking substance
!  i.e. mstick(6) = 1, then substance 1 is inactive and substance 6
!                      is the origin (eg. floating oil) and is active
!..note: inactive (or stikcing) substances must have mstick(i) < 0
!
      itrack = 0
      ntrack = 0
      nstick = 0
!
      nplot  = 0 ! whole array assignment
      mstick = 0 ! whole array assignment
!
      do 29 isub = 1, nosubs
!
!.. substances that are sticking
!
         if(isub > (3*nfract).and.     &
            substi(isub)(1:6)=='stick_') then
!
!.. substances that are sticking and that are not oil
!.. look for the source of this material
!.. 14 characters (all that is left) are used hereto
!
            do 28 jsub = 1, nosubs
               if(substi(jsub)(1:14)==substi(isub)(7:20)) then
                  mstick(jsub) = isub
!.. sticking one itself must ne negativer!
                  nstick = nstick + 1
                  mstick(isub) = -nstick
                  goto 288
               endif
28          continue
!
            write(*   ,*) ' Error: sticking substance has no source '
            write(lun2,*) ' Error: sticking substance has no source '
            call stop_exit(1)
!
288         continue
!
         endif
29    continue
!
!.. insert substances that are sticking and are oil
!
      do 289 ifract = 1, nfract
!.. floating oil sticks
         isub = (ifract-1)*3 + 1
         jsub = 3*ifract
         mstick((ifract-1)*3 + 1) = 3*ifract
!.. dispersed oil sticks
         isub = (ifract-1)*3 + 2
         jsub = 3*ifract
         mstick((ifract-1)*3 + 2) = 3*ifract
!.. sticking substance itself may not stick
!   (it woulld be converted back to floating or dispersed)
         nstick = nstick + 1
         mstick(3*ifract)         = -nstick
289   continue
!
      write(lun2, 3132) nstick
      do 290 isub = 1, nosubs
         write(lun2,3133) isub, mstick(isub)
290   continue
!
!======================================================================
!
      i = 0
      do 62 ilay = 1, nolay
         do 52 isub = 1, nosubs
            i = i + 1
            subst(i)    = '                    '
            subst(i)    = substi(isub)
            if(nolay > 1) then
               ln = min(lenstr(subst(i),20)+1,18)
               write(subst(i)(ln:),'(a,i2.2)') '_',ilay
            endif
52      continue
62    continue
!
!     for 3d datasets (required by gpp)
!
      substi(nosubs+1) = 'localdepth          '
!
      i = 0
      do 61 ilay = 1, nolay
         do 51 isub = 1, nosubs+1
            i = i + 1
            if(modtyp==2) then
               ipos = (isub-1)*nolay + ilay
               subst2(ipos) = substi(isub)
            elseif(ilay==1) then
               subst2(isub) = substi(isub)
            endif
51       continue
61    continue
!
!.. get substance numbers for red tide model
!
      if(modtyp==3) then
!
        do 71 i = 1, nosubs
!.. red-q is non-dimensional cellular n content in red tide
             if(substi(i)(1:5)=='RED-Q') mapsub(1) = i
!.. red-n is nitrogen concentration in red tide
             if(substi(i)(1:5)=='RED-N') mapsub(2) = i
!.. red-c is concentration nitrogen-red tide in water column
             if(substi(i)(1:5)=='RED-C') mapsub(3) = i
!.. NH4 is ammonium
             if(substi(i)(1:3)=='NH4') mapsub(4) = i
!.. NO3 is nitrate
             if(substi(i)(1:3)=='NO3') mapsub(5) = i
!.. must follow..
             if(substi(i)(1:8)=='NH4_FILE') then
                mapsub(4) = -i
                read(lun1,*,err=4011)  finnh4
             endif
             if(substi(i)(1:8)=='NO3_FILE') then
                mapsub(5) = -i
                read(lun1,*,err=4012)  finno3
             endif
71      continue
!
        write(lun2,*) ' Red tide model, substance indices: '
        write(lun2,*) ' q-red tide is substance number ',mapsub(1)
        write(lun2,*) ' n-red tide is substance number ',mapsub(2)
        write(lun2,*) ' c-red tide is substance number ',mapsub(3)
        write(lun2,*) ' NH4 is substance number ',mapsub(4)
        write(lun2,*) ' NO3 is substance number ',mapsub(5)
        if(mapsub(4) <0) then
           write(lun2,*) ' Name of NH4 conc file = ',finnh4
        endif
        if(mapsub(5) <0) then
           write(lun2,*) ' Name of NO3 conc file = ',finno3
        endif
      endif
!
      if     (modtyp == 1) then
!
         write(lun2 , 2001) 'Tracer model'
!
      elseif (modtyp == 2) then
!
         write (lun2 , 2001)   &
              'Temperature model: stratified 2-layer model'
!
         do 91 i1 = 1, nosubs*nolay
            if(i1 <= nolay) then
               write (lun2 , 2010) 'Substance - upper layer:',  &
               subst(i1)( 1:10) , subst(i1)(11:20)
            else
               write (lun2 , 2010) 'Substance - lower layer:',  &
               subst(i1)( 1:10) , subst(i1)(11:20)
            endif
 91      continue
         write (lun2 , 2011) pblay
!
      elseif (modtyp == 3) then
!
        write (lun2 , 2002)  &
        'Red tide model - five substances 3d model'
!
      elseif (modtyp == 4) then
!
        write (lun2 , 2002)  &
        'Oil model - dispersion and evaporation included'
      endif
!
!     added for 3d datasets
!
      subst2(nosubs+1) = 'localdepth'
!
!======================================================================
!
!     read the model parameters
!     number of particles
!
!     note : - npmax must be set to the requested number + 0.5% of that
!              number because due to round-off errors while adding new
!              particles, less then 0.5% of the total number of particles
!              are added surplus; so check with -0.5%
!
      read (lun1,    *,err = 4013) np
      write(lun2, 2020) np
      write(*   , 2020) np
      nopart = np
!
!     roughness
!
      read (lun1 ,    *,err = 4014) rough
      write(lun2 , 2040) rough
!
!     random walk
!
      read (lun1 ,    *,err = 4015) dr1 , dr2
      write(lun2 , 2050) dr1 , dr2
      drand(2) = dr2/2.0
      if(dr1 > (0.0)) then
         ldiffh= .true.
         drand(1) = 2.0*sqrt(idelt*dr1)
      else
         ldiffh= .false.
         drand(1) = 0.0
      endif
!
!     wind fetch [%]
!
      read (lun1 ,    *,err = 4016) drand(3)
      write(lun2 , 2060) drand(3)
!
!     water density
!
      read (lun1 ,    *,err = 40161) rhow
      write(lun2 , 2065) rhow
!
!     option for wind
!     constant wind velocity and direction
!
      read (lun1 ,    *,err = 4017) iwopt
      if     (iwopt  < 2) then
!
!       error, number of wind variations
!
        write(lun2 , 2070) nowind, iwopt
      else
!
!       wind varies with the time
!
        nowind = iwopt
        do 100, i = 1, nowind
          read (lun1 ,   *,err=4018) id, ih, im, is, wveloa(i), wdira(i)
          iwndtm(i) = id * 86400 + ih * 3600 + im * 60 + is
  100   continue
      endif
      write(lun2 , '(//)')
!
!======================================================================
!
!       read number of constants that will follow
!
      read (lun1 , *,err=4019) noc

      if (.not. oil) then

!        for other models than oil
         do i = 1, noc
            read (lun1 , *,err=4020) const(i)
         enddo
      else
!
!       read oil parameters per oil fraction
!
!        1. evaporation fraction per day
!        2. option for oil dispersion
!        3. dispersion rate/day
!        4. sticky probability [0,1]
!        5. volatile fraction [0,1]
!        6. emulsification parameter c1
!        7. maximum water content c2  [0,1]
!        8. evaporation fraction at which emulsification starts
!        9. density
!       10. kinematic viscosity
!       11. stick at drying flats if 1.0       optional
!       12. minimum thickness of oil layer
!
         nfcons = 10
         do ifrac = 1, nfract
            read (lun1,*,err=4020) &
                      const((ifrac-1)*nfcons + 1)  ! evaporation
            read (lun1,*,err=4020) iopt            ! dispersion option
            backspace lun1 ! read input line again
            if (iopt == 0) then
               read (lun1,*,err=4020)  &
                      const((ifrac-1)*nfcons + 2), & ! dispersion option
                      const((ifrac-1)*nfcons + 3)    ! dispersion rate/day
            else
               ! dispersion option
               read (lun1,*,err=4020) const((ifrac-1)*nfcons + 2)
               const((ifrac-1)*nfcons + 3) = 0.0 ! not used
            endif

            do i = 4, nfcons
               read (lun1,*,err=4020) const((ifrac-1)*nfcons + i)
            enddo
         enddo

         nocons = nfract * nfcons ! nfcons params per oil fraction
!
!        read global oil parameters,
!           - minimum thickness oil layer
!           - deflection angle
!
         nocons = nocons + 1
         read (lun1,*,err=4020) const(nocons) ! hmin
         if (nolay > 1) then
            nocons = nocons + 1
            read (lun1,*,err=4020) const(nocons) ! deflection angle
         endif
!      
         stickdf = 0.0
         if (filvers(2:19) >= '3.73.00') then
            nocons = nocons + 1
            read (lun1,*,err=4020) stickdf   ! sticking at drying flats
         endif
!
!        adjust number of constants
!
         nocons = noc

!        extra parameters added from part v3.66.00
!        *) per oil fraction
!           - volatile fraction (volfrac)
!           - emulsification fraction (c1)
!           - maximum water content (c2)
!           - fraction evap. at which emulsification starts (evemul)
!        *) global oil parameter
!           - minimum thickness oil layer (hmin)
!
!
!        write constants per oil fraction
!
         do ifrac = 1, nfract
            write(lun2,'(2x,a,a)') 'Fraction ',substi(1+3*(ifrac-1))
            write(lun2,'(8x,a,es15.7)')  &
           'Evaporation per day                              : ',  &
                      const((ifrac-1)*nfcons+1)
            write(lun2,'(8x,a,es15.7)') &
           'Evaporation per time step                        : ',  &
                        const ((ifrac-1)*nfcons + 1)*idelt/86400.0

            ioptdd = int(const ((ifrac-1)*nfcons + 2))
            if(ioptdd==1) then
               write(lun2,'(8x,a,es15.7)')  &
              'Dispersion C0 from Delvigne/Sweeney formula'
            else
               write(lun2,'(8x,a,es15.7)')  &
              'Dispersion rate (per day)                        : ',  &
                         const((ifrac-1)*nfcons+3)
            endif

            fstick = const ((ifrac-1)*nfcons+4)
            write(lun2,'(8x,a,f10.3)')  &
           'Stickyness probability [0-1]                     : ',fstick

            volfrac = const ((ifrac-1)*nfcons+5)
            write(lun2,'(8x,a,f10.3)')  &
           'Volatile fraction                                : ',volfrac

            c1      = const ((ifrac-1)*nfcons+6)
            write(lun2,'(8x,a,e10.3)')  &
           'Emulsification parameter                         : ',c1

            c2      = const ((ifrac-1)*nfcons+7)
            write(lun2,'(8x,a,f10.3)')  &
           'Maximum water content                            : ',c2

            evemul  = const ((ifrac-1)*nfcons+8)
            write(lun2,'(8x,a,f10.3)')  &
           'Fraction evaporated to start emulsification      : ',evemul

            density = const((ifrac-1)*nfcons+9)
            write(lun2,'(8x,a,es15.7)')  &
           'Oil density (kg/m3)                              : ', &
            density

            viscos = const((ifrac-1)*nfcons+10)
            write(lun2,'(8x,a,es15.7)')  &
           'Kinematic viscosity (cSt)                        : ',  &
            viscos

         enddo
!
!        global oil parameters
!
         write(lun2,'(2x,a)') ' Global oil parameters'
         if (nolay == 1) then
            hmin = const(nfract*nfcons+1)
            write(lun2,'(8x,a,e10.3)')   &
           'Minimum thickness oil layer                      : ',hmin
         else if (nolay > 1) then
            hmin = const(nfract*nfcons+1)
            angle= const(nfract*nfcons+2)
!
            write(lun2,'(8x,a,e10.3)')   &
           'Minimum thickness oil layer                      : ',hmin
            write(lun2,'(8x,a,f10.3)')   &
           'Deflection angle (Coriolis - 3D only)            : ',angle
         endif
         if ( stickdf .eq. 0 ) then
            write(lun2,'(8x,a)') 'Oil is not sticking at drying flats'
         else
            write(lun2,'(8x,a)') 'Oil is sticking at drying flats'
         endif
      endif
!
      nocons = noc
!
!======================================================================
!
!     read the simulation timers
!     simulation start time
!
      read (lun1 ,    *,err=4021) id, ih, im, is
      write(lun2 , 2090) id, ih, im, is
      itst2 = id * 86400 + ih * 3600 + im * 60 + is
      if (itst2  /=  itstrt) then
        write(lun2 , 2100) itstrt / 86400 , mod(itstrt, 86400)/3600,  &
                            mod(itstrt, 3600) / 60 , mod(itstrt, 60)
        ierr = ierr + 1
      endif
!
!     simulation stop  time
!
      read (lun1 ,    *,err=4022) id,ih,im,is
      write(lun2 , 2110) id,ih,im,is
      itstop = id * 86400 + ih * 3600 + im * 60 + is
      if (mod(itstop - itstrt, idelt)  /=  0) then
        write(lun2 , 2120) idelt
        ierr = ierr + 1
      endif
!
!     delwaq delay  time (obsolete)
!
      read (lun1 ,    *,err=4023) id,ih,im,is
!     write(lun2 , 2122) id,ih,im,is
      iddtim = id*86400 + ih*3600 + im*60 + is
      iddtim = max (0 , iddtim)
!
      iddtim = 0
      if (iddtim  >  0) then
        ir = itstop - itstrt - iddtim
        write(lun2 , 2125) ir/86400 , mod(ir,86400)/3600 ,  &
                              mod(ir,3600)/60 , mod(ir,60)
      endif
!
!     check wind-variations
!
      write(lun2 , 2071)
      do 120 i = 1, nowind
!
        if     (i  < 2) then
!
!         first time must be equal to starttime
!
          if (iwndtm(i)  /=  itstrt       ) then
            write(lun2 , 2072)
          endif
        elseif (i == nowind) then
!
!         first time must be equal to stoptime
!
          if (iwndtm(i)  /=  itstop       ) then
            write(lun2 , 2075)
          endif
        else
!
!         actual time must be later then then this time but one
!
          if (iwndtm(i)  <=  iwndtm(i - 1)) then
            write(lun2 , 2073)
          endif
!
        endif
!
        itime = iwndtm(i)
        id = itime/86400
        ih = mod(itime, 86400) / 3600
        im = mod(itime,  3600) / 60
        is = mod(itime,    60)
        write(lun2 , 2074) id, ih, im, is, wveloa(i), wdira(i)
!
  120 continue
!
!     map file start time
!
      read (lun1 ,    *,err=4024) id,ih,im,is
      write(lun2 , 2130) id,ih,im,is
      icwsta = id * 86400 + ih * 3600 + im * 60 + is
      if (icwsta  < itstrt) then
        write(lun2 , 2140)
        ierr = ierr + 1
      endif
!
!     start time of map-file must be equal to start time of simulation
!     for delwaq take over , otherwise not required
!
      if (iddtim  /=  0 .and. icwsta  /=  itstrt) then
        write(lun2 , 2145)
        ierr = ierr + 1
      endif
!
!     map file stop  time
!
      read (lun1 ,    *,err=4025) id,ih,im,is
      write(lun2 , 2150) id,ih,im,is
      icwsto = id*86400 + ih*3600 + im*60 + is
      if (icwsto  < icwsta) then
        write(lun2 , 2160)
        ierr = ierr + 1
      endif
      if (iddtim  /=  0 .and. icwsto  /=  itstop) then
        write(lun2 , 2165)
        ierr = ierr + 1
      endif
!
!     map file step  time
!
      read (lun1 ,    *,err=4026) id,ih,im,is
      write(lun2 , 2170) id,ih,im,is
      icwste = id*86400 + ih*3600 + im*60 + is
      if (icwste  <=  0) then
         write(lun2, *) ' Error: time step mapfile must be positive '
         write(*   , *) ' Error: time step mapfile must be positive '
         ierr = ierr + 1
      endif
      if (iddtim  /=  0) write(lun2 , 2175)
!
!.. changes for version 3.00 : time histories like timings map file in input
!
!     time-history file start time
!
      read (lun1 ,    *,err=4027) id,ih,im,is
      write(lun2 , 2131) id,ih,im,is
      ihstrt = id * 86400 + ih * 3600 + im * 60 + is
      if (ihstrt  < itstrt) then
        write(lun2 , 2166)
        ierr = ierr + 1
      endif
!
!     time-history file stop  time
!
      read (lun1 ,    *,err=4028) id,ih,im,is
      write(lun2 , 2151) id,ih,im,is
      ihstop = id*86400 + ih*3600 + im*60 + is
      if (ihstop  < ihstrt) then
        write(lun2 , 2167)
        ierr = ierr + 1
      endif
!
!     step time on time-history file
!
      read (lun1 ,    *,err=4029) id,ih,im,is
      write(lun2 , 2171) id,ih,im,is
      ihstep = id*86400 + ih*3600 + im*60 + is
      if (ihstep  <=  0) then
         write(lun2, *) ' Error: time step hisfile must be positive '
         write(*   , *) ' Error: time step hisfile must be positive '
         ierr = ierr + 1
      endif
!
!.. end of changes version 3.00
!
!     offset to real timings
!
      read (lun1 ,    *,err=4030) iyear,imonth,id,ih,im,is
      write(lun2 , 2180) iyear,imonth,id,ih,im,is
      iofset = id*86400 + ih*3600 + im*60 + is
!
!     write t0-string to title string
!     required by gpp for time offset on his and map file
!
      write(title(4),'(a3,1x,i4.4,5(1x,i2.2) )')  &
             'T0:',iyear,imonth,id,ih,im,is
!
!======================================================================
!
!     initial condition for oil (from v3.66)
!
!     ini_opt = 0 : cold start (no particles ar t=0)
!             = 1 : warm start
!                   oil patches to be read from  ini-file
!
      if (oil) then
         read (lun1,*,err=6001) ini_opt
         if (ini_opt == 1 ) read (lun1,*,err=6002) ini_file
      endif
!
!     read the number of monitoring stations
!     their names and their (x,y) coordinates
!
      read (lun1,    *,err=4031) nosta
      if (ihstep  <=  0 .and. nosta  >  0) then
        write(lun2 , 2024)
        ierr = ierr+1
      endif
      if (nosta  >  0) then
        write(lun2 , 2026)
        do 5 i = 1, nosta
          read (lun1,   *,err=4032) nmstat(i),xstat(i),ystat(i)
          write(lun2,2027) nmstat(i),xstat(i),ystat(i)
    5   continue
      endif
!======================================================================
!
!     read the plot characteristics
!     number of plot times
!
      read (lun1 ,    *,err=4033) notime
      if (notime > 0) write(lun2 , 2190)
      iptset = notime
!
!     plot times
!
      do 10 i = 1 , notime
        read (lun1 ,    *,err=4034)      id , ih , im , is , recovr(i)
        write(lun2 , 2210) id , ih , im , is , recovr(i)
        ipset (i) = id*86400 + ih*3600 + im*60 + is
        if (ipset (i)  < itstrt .or. ipset (i)  >  itstop) then
          write(lun2 , 2220)
          ierr = ierr + 1
        endif
!
!       test ascending order
!
        if (i  >  1) then
          if (ipset (i)  <=  ipset (i - 1)) then
            write(lun2 , 2222)
            ierr = ierr + 1
          endif
        endif
!
   10 continue
!
!     test precise matching of map output
!
      if ( mod(icwste,idelt)  > 0 )  then
           write(lun2 , 2224)
           ierr = ierr + 1
      endif
!
!     x-window of the plot
!
      read (lun1 ,    *,err=4038) xw1f , xw2f
      if (xw1f  >  xw2f) then
!
!       ascending order; swap
!
        xw1f = xw1f + xw2f
        xw2f = xw1f - xw2f
        xw1f = xw1f - xw2f
      endif
      write(lun2 , 2229)
      write(lun2 , 2230) xw1f , xw2f
!
!     y-window of the plot
!
      read (lun1 ,    *,err=4038) yw1f , yw2f
      if (yw1f  >  yw2f) then
!
!       ascending order; swap
!
        yw1f = yw1f + yw2f
        yw2f = yw1f - yw2f
        yw1f = yw1f - yw2f
      endif
      write(lun2 , 2240) yw1f , yw2f
      window(1) = xw1f
      window(2) = xw2f
      window(3) = yw1f
      window(4) = yw2f

      read (lun1 ,   *) mmap, nmap
      nmap=max(nmap,3)
      mmap=max(mmap,3)
      write(lun2, 2241 ) mmap  ,nmap
!
!======================================================================
!
!     instantaneous release chracteristics
!     number of instantaneous releases
!
      read (lun1 ,    *,err=4039) nodye
      if (nodye > 0) write(lun2 , 2250)
!
!     for each instantaneous release
!
      do 20 i = 1 , nodye
!
!       name of the instantaneous release (version 3.30)
!
        read (lun1 ,    *,err=4040) nmdyer(i)
        write(lun2 , 2258) nmdyer(i)
!
!       time of the instantaneous release
!
        read (lun1 ,    *,err=4041)   id,ih,im,is
        write(lun2 , 2260) id,ih,im,is
        iwtime(i) = id*86400 + ih*3600 + im*60 + is
        if (iwtime(i)  < itstrt .or. iwtime(i)  >  itstop) then
          write(lun2 , 2270)
          ierr = ierr + 1
        endif
!
!       location of the instantaneous release
!
        if (.not.oil) then
           if (nolay==1) then
              read (lun1,*,err=4042) xwaste(i), ywaste(i), zwaste(i)
              kwaste(i)=1
           else
              read (lun1,*,err=4042) xwaste(i), ywaste(i), kwaste(i)
              zwaste(i)=0.0
           endif
        else
           read (lun1,*,err=4042) xwaste(i), ywaste(i)
           zwaste(i)=0.0
           kwaste(i)=1
        endif
!
!       option for release radius (and release radius)
!
        read (lun1 ,    *,err=4043) iopt
        if (iopt == 0) then
           backspace lun1
           read (lun1 ,    *,err=4043) ioptrad(i), radius(i)
        else
           ioptrad(i) = iopt
           radius (i) = 0
        endif
!
!       percentage of particles used
!
        read (lun1 ,    *,err=4043) wparm(i)
        ndprt(i) = int(wparm(i)*nopart/100.0 + 0.5)

        if (nolay==1) then
           write(lun2 , 2280) xwaste(i), ywaste(i), zwaste(i)
           write(lun2 , 2282) radius(i), wparm(i)
        else
           write(lun2 , 2281) xwaste(i), ywaste(i), kwaste(i)
           write(lun2 , 2282) radius(i), wparm(i)
        endif
!
!       mass of the instantaneous release
!
        read (lun1 ,  *,err=4044) (amassd(k,i),k=1,nosubs)
        write(lun2 , 2289)
        do k=1,nosubs
           write(lun2,2290) substi(k),amassd(k,i)
        enddo
   20 continue
!======================================================================
!
!     continuous release chracteristics
!     number of continuous releases
!
      read (lun1 ,    *,err=4045) nocont
      if (nocont > 0) write(lun2 , 2300) nocont
!
!     for each continuous release
!
      allocate(ascal(nocont))
      do 30 ic = 1 , nocont
!
!       name of the continuous release (version 3.30)
!
        read (lun1 ,    *,err=4046) nmconr(ic)
        write(lun2 , 2259) nmconr(ic)
!
!       location of the continuous release
!
        if (.not.oil) then
           if (nolay==1) then
              read (lun1,*,err=4047)  &
                   xwaste(ic+nodye), ywaste(ic+nodye), zwaste(ic+nodye)
              kwaste(ic+nodye)=1
           else
              read (lun1,*,err=4047)  &
                   xwaste(ic+nodye), ywaste(ic+nodye), kwaste(ic+nodye)
              zwaste(ic+nodye)=0.0
           endif
        else
           read (lun1,*,err=4047) xwaste(ic+nodye), ywaste(ic+nodye)
           zwaste(ic+nodye)=0.0
           kwaste(ic+nodye)=1
        endif
!
!       radius and scale (% of particles)
!
        read (lun1,*,err=4043) radius(ic+nodye), wparm(ic+nodye)
        ndprt(ic+nodye) = int(wparm(ic+nodye)*nopart/100.0 + 0.5)

        if (nolay==1) then
           write(lun2 , 2280)  &
                 xwaste(ic+nodye), ywaste(ic+nodye), zwaste(ic+nodye)
           write(lun2 , 2282)  &
                 radius(ic+nodye), wparm(ic+nodye)
        else
           write(lun2 , 2281)  &
                 xwaste(ic+nodye), ywaste(ic+nodye), kwaste(ic+nodye)
           write(lun2 , 2282)  &
                 radius(ic+nodye), wparm(ic+nodye)
        endif
!
!       scale factors (ascal) for each load
!
        read (lun1 ,  *,err=4049) ascal (ic)
        write(lun2,'(/8x,a,f10.2)') 'Scale factor : ',ascal (ic)
!
!       interpolation options for continuous releases
!
        read (lun1,*,err=5049) linear(ic)
        if (linear(ic) == 0) then
           write(lun2,'(/8x,a)') 'Block interpolation for load table'
        else
           write(lun2,'(/8x,a)') 'Linear interpolation for load table'
        endif
!
!       and stoechiometry matrix (stoch) for each load
!
        write(lun2,2329)  nmconr(ic)
        read (lun1,*,err=4050) (stoch (k,ic), k = 1, nosubs)
        do k=1,nosubs
           write(lun2,2290) substi(k),stoch(k,ic)
        enddo
!
!       read time points cont's
!
        read (lun1 ,    *,err=4051) ictmax(ic)
!
!       for each time step
!
        allocate (dhms(size(ictime,2),4))
        do 50 i = 1 , ictmax(ic)
!
!         read the rate of mass-release (unity of mass/sec)
!         for each continuous load
!         for this breakpoint (loads first, then times)
!
          read (lun1,*,err=4052) id,ih,im,is,ftime(ic,i)
          dhms(i,1)=id
          dhms(i,2)=ih
          dhms(i,3)=im
          dhms(i,4)=is
!
          ictime(ic,i) = id*86400 + ih*3600 + im*60 + is
          if (i == 1 .and. ictime(ic,i)  /=  itstrt) then
            write(lun2 , 2360)
            ierr = ierr + 1
          endif
   50   continue
!
!       make up mass by multiplication with scale factors
!
        do ibrk = 1 , ictmax(ic)
        do isub = 1 , nosubs
           amassc(ic, isub, ibrk) = ascal(ic) * stoch(isub,ic) * ftime(ic,ibrk)
        enddo
        enddo
!
!
        write(lun2,'(8x,a)') &
         ' Load table (after scaling)'
        write(lun2,'(/12x,a,a)')  ' Station ',nmconr(ic)
        do isub=1,nosubs
          write(lun2,'(15x,10a)') substi(isub)
          do ibrk = 1 , ictmax(ic)
            write(lun2,2352) (dhms(ibrk,j),j=1,4), amassc(ic, isub, ibrk)
          enddo
        enddo
        deallocate (dhms)
!
        if (ictmax(ic)  >  0) then
!
!       check stop-time continuous loads
!
        if (ictime(ic,ictmax(ic))  /=  itstop) then
          write(lun2 , 2381)
          ierr = ierr + 1
        endif
      endif
!

   30 continue
      deallocate(ascal)
      nodac = nodye + nocont
!======================================================================
!
!       user defined releases
!
      read (lun1 ,    *,err=4053) noudef
      write(lun2 , 2301) noudef
!
!     for each release
!
      do 55 i = 1 , noudef
!
!       percentage of particles of the release
!
        read (lun1, *,err=4054)  wparm(i+nodac), uscal(i)
        ndprt(i+nodac) = int(wparm(i+nodac)*nopart/100.0 + 0.5)
        write(lun2,2316) i, ndprt(i+nodac), wparm(i+nodac), uscal(i)
!
!       scalefactors ud releases (to scale files)
!
!       read time for delpar release, and substance number
!
        read (lun1 ,    *,err=4055) id, ih, im, is, isubud(i)
        iutime(i) = id*86400 + ih*3600 + im*60 + is
        write(lun2,2378)
        write(lun2,2379) id,ih,im,is,i,isubud(i)
!
!       read option no. for file type
!       0 = mapfile delwaq
!       1 = restart file delwaq
!       2 = particle coordinates file
!
        read (lun1,*,err=4056) ifopt(i)
!
!       read filename delwaq or particle coordinates file
!
        read (lun1 ,    *,err=4057) finud(i)
        write(lun2 , 2317) finud(i), ifopt(i)
!
        if(ifopt(i) /= 2) then
!
!       read time for delwaq, no., no. of subst. and subst. number
!
           read (lun1,*,err=4058) id,ih,im,is,nosud(i),isfud(i)
           write(lun2, 2376)
           write(lun2, 2377) id, ih, im, is, &
              ifopt(i), nosud(i), isubud(i), isfud(i)
           iftime(i) = id*86400 + ih*3600 + im*60 + is
        else
           lunin = 51
           call openfl(lunin, finud(i), ftype(2), 0  )
           read(lunin,err=5001)
           read(lunin,err=5002) iftime(i)
           write(lun2, 2389) iftime(i)
           close(lunin)
        endif
!
        if(iftime(i) <itstrt.or.iftime(i) > itstop) then
           write(lun2,2176) i
        endif
!
   55 continue
!
!======================================================================
!
!     read time points decays
!
      read (lun1 ,    *,err=4059) idtset

!
      allocate (dhms(size(decay,2),4))
      do 60 i = 1 , idtset
!
!       read the values
!
        it = ( i - 1 ) * nosubs
        read (lun1,*,err=4060) id,ih,im,is,(decay(isub,i),isub=1,nosubs)
        dhms(i,1)=id
        dhms(i,2)=ih
        dhms(i,3)=im
        dhms(i,4)=is
!
        idtime(i) = id*86400 + ih*3600 + im*60 + is
        if (i == 1 .and. idtime(i)  /=  itstrt) then
          write(lun2 , 2360)
          ierr = ierr + 1
        endif
        if (i  >  1) then
          if (idtime(i)  <=  idtime(i-1)) then
            write(lun2 , 2370)
            ierr = ierr + 1
          endif
        endif
   60 continue
!
      write(lun2,'(1x,a)') ' Decay rates'
      do is=1,nosubs
        write(lun2,'(6x,a)') substi(is)
        do i = 1 , idtset
           write(lun2,2374) (dhms(i,j),j=1,4),decay(is,i)
        enddo
      enddo
      deallocate (dhms)
!
!
      if (idtset  >  0) then
!
!       check stop-time instantaneous loads
!
        if (idtime(idtset)  /=  itstop) then
          write(lun2 , 2380)
          ierr = ierr + 1
        endif
      endif
!======================================================================
!
      write(lun2 , 2338)
!     read power for vz = w * c ** n concentration dependent settling
!     and refinement factor for the plotgrid
!
      nvsfour = 6
!
!     data first added in v3.63.00, so only read starting
!     from this version
!
      read (lun1 , * , err=4064 ) anfac
      read (lun1 , * , err=4065 ) irfac
      irfac = max(1,irfac)
      write ( lun2 , 2339 ) anfac,irfac

!     read time points settling velocities
!
      read (lun1 ,    *,err=4061) ivtset
!
!3d
      do 80 isub = 1, nosubs
         write(lun2,'(4x,a)') substi(isub)
         write(lun2 , 2373)
         do 75 i = 1, ivtset
!
!           read the values
!
            ks = ( i - 1 ) * nosubs + isub
!
!           in v3.63.00 two extra settling tables were added i.e.
!           for vmin and vmax. so, total of 6 tables
!
            read (lun1,  *,err=4062  ) id,ih,im,is, &
                   (vsfour(iq,isub,i),iq=1,nvsfour)
!
            write(lun2, 2375) id,ih,im,is, &
                   (vsfour(iq,isub,i),iq=1,nvsfour)
!
            if(isub==1) then
               ivtime(i) = id*86400 + ih*3600 + im*60 + is
               if (i == 1 .and. ivtime(i)  /=  itstrt) then
                 write(lun2 , 2360)
                 ierr = ierr + 1
               endif
               if (i  >  1) then
                  if (ivtime(i)  <=  ivtime(i-1)) then
                     write(lun2 , 2370)
                     ierr = ierr + 1
                  endif
               endif
            endif
   75    continue
   80 continue
!
      if (ivtset  >  0) then
!
!       check stop-time settling velocities
!
        if (ivtime(ivtset)  /=  itstop) then
          write(lun2 , 2382)
          ierr = ierr + 1
        endif
      endif
!
!
!======================================================================
!
!       critical shear stress for sedimentation and erosion
!       chezy and the density of water in g/l
!
      noslay = nolay
! 10/9/98: set defaults for chezy
      chezy = 50.0
!
      if(lsettl) then
         read (lun1,*,err=4063) taucs, tauce, chezy
         write(lun2,3125) taucs, tauce, chezy
      endif
!
!..  for sedimentation-erosion an extra layer is created
!..  for output routines part12, part13 and parths
!..  for plot routine part13 also extra subsyances names are created
!
      if(lsettl) then
         noslay = nolay + 1
         i = nolay*nosubs
         write(lun2,*) ' Substances defined in bed layer (sed/erosion): '
         do 83 ilay = nolay+1, noslay
            do 84 isub = 1, nosubs
               i = i + 1
               subst(i)    = substi(isub)
               ln = min(lenstr(subst(i),20)+1,18)
               write(subst(i)(ln:),'(a,i2.2)') '_',ilay
               write(lun2,'(6x,a)') subst(i)
84          continue
83       continue
      endif
!
!======================================================================
!
!     close input work-file
!
   70 close (lun1)
!
!     check on the total number of particles:
!
      np = 0
      do 90, i = 1 , nodye  + nocont + noudef
        np = np + ndprt(i)
90    continue
!
      if (np  /=  nopart) then
       nopart = np
       write(lun2, 3100) nopart
      endif
!
!     check on valid zoom window for his, plo and psf file
!
      dxwin = abs(window(2)-window(1))
      dywin = abs(window(4)-window(3))
      if ( dxwin <=  eps  .or. dywin <=  eps ) then
          write(lun2,2390)
         ierr = ierr + 1
      endif
!
!     stop when errors occured dureing reading
!
      if (ierr  /=  0) then
        call stop_exit(1)
        stop ' Errors found on part input file; check print file '
      endif
!
!     end of subroutine
!
      if ( timon ) call timstop ( ithndl )
      return
!
!     formats
!
!     informative formats
!
 1997 format(//,2x,'Applied hydrodynamics file (hyd-file): ',a/)
 2000 format(   2x,'Simulation: ',/(14x,a40))
 2001 format(/,2x,'Model type: ',/,14x,a,i6                          )
 2002 format(/,2x,'Model type: ',/,14x,a                             )
 2010 format(14x,2a,a10,' - ',a10                                      )
 2011 format(14x,'Relative height of division between layers: ',f10.3  )
 2020 format(/'  Number of particles to be used     :',i11             )
 2026 format(/'  Monitoring stations : ',/,  &
              '        location name               x           y    '  )
 2027 format(8x,a20,2(2x,f11.2)                                        )
 2040 format(/'  Roughness length in meters         :',f11.4           )
 2050 format( '  Horizontal displacement parameters :',2f11.4          )
 2060 format( '  Wind influence coefficient [%]     :',f11.4           )
 2065 format( '  Density of water [kg/m**3]         :',f11.4           )
 2071 format(/'  Table for wind:'/       &
             7x,'Time (dd hh mm ss)',15x,'Wind speed',  &
             13x,'Wind direction' )
 2074 format(8x,i4,'D-',i2.2,'H-',i2.2,'M-',i2.2,'S.',  &
             f20.4,' m/s ',f20.2,'  degr.')
 2090 format(/  &
             '  Start time simulation:',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2110 format('  Stop  time simulation:',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2125 format('  Effective repeatcycle:',i4,'D-',i2.2,  &
               'H-',i2.2,'M-',i2.2,'S.')
 2130 format  &
          (/,'  Start time map file  :',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2131 format  &
          (/,'  Start time time-hist.:',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2170 format('  Time step in map file:',i4,'D-',i2.2,  &
                'H-',i2.2,'M-',i2.2,'S.')
 2171 format('  Time step time-hist. :',i4,'D-',i2.2,  &
               'H-',i2.2,'M-',i2.2,'S.')
 2180 format(/'  Offset to the real world time scale: ',/,  &
      '  Year: ',i4,', Month: ',i2.2,', ',i2.2,      &
                'D-',i2.2,'H-',i2.2,'M-',i2.2,'S.')
 2190 format(/'  Time steps written to plot file (*.plo):')
 2210 format(5x,i4,'D-',i2.2,'H-',i2.2,'M-',i2.2,'S.',  &
                                              '  Recovery rate:',f10.5 )
 2229 format(/'  Zoom window')
 2230 format( '     Xmin        = ',f11.2,' Xmax        = ',f11.2)
 2240 format( '     Ymin        = ',f11.2,' Ymax        = ',f11.2)
 2241 format( '     Cells for x = ',i11  ,' Cells for y = ',i11)
 2250 format(/'  Instantaneous release stations:')
 2258 format(8x,'Station name :',a20  )
 2259 format(8x,'Station name :',a20  )
 2260 format(12x,'Release time            =',   &
                         i4,'D-',i2.2,'H-',i2.2,'M-',i2.2,'S.')
 2280 format(12x,'Coordinates             = (',f11.2,',',f11.2,')'/  &
             12x,'Depth(%) under surface  =  ',f11.0)
 2281 format(12x,'Coordinates             = (',f11.2,',',f11.2,')'/  &
             12x,'Layer                   =  ',i11  )
 2282 format(12x,'Initial radius          =   ',f11.0, ' m.',/,      &
             12x,'Percentage of particles =   ',f11.0, ' %')
 2289 format(12x,'Released masses : ')
 2290 format(20x,'  Substance : ',a20,e13.4,'  kg/m3')
 2300 format(/'  Number of continuous release stations:', i2  /       )
 2301 format(/'  Number of user defined releases    :', i2  /         )
 2316 format( '  User defined release: ',i3,' no. of particles: ', i9,/,  &
              '  Percentage of particles =   ',f11.0, ' %'           ,/,  &
              '  Scale factor of release =   ',f11.0, '  '             )
 2317 format( '  Filename ud release : ',  a80,                       /,  &
              '  Option for this file: ',i3,                          /)
 2329 format(/8x,'Station name :',a,' released substances')
 2331 format(14x,a,f12.3,5x,a)
 2335 format(//'  Number of breakpoints for continuous loads : ',i5,/  )
 2338 format(//' Settling velocities ')
 2339 format(/4x,'Power for settling velocity (v=w*C**n)  : ',e15.6/  &
              4x,'Refinement factor of the plotgrid for C : ',i5    )
 2352 format( 18x,i4.2,'-',i2.2,'-',i2.2,'-',i2.2,e20.3,' kg/s')
!                              1234567890112345678901....123456789
 2373 format(10x,'time',8x,'base',4x,'amplitude',7x,'period',       &
             8x,'phase',6x,'minimum',6x,'maximum',/,          &
             4x,'dd-hh-mm-ss',7x,'(m/s)',7x,'(m/s)',6x,'(hours)',   &
             6x,'(hours)',8x,'(m/s)',8x,'(m/s)')
!                              1234567890112345678901....123456789
 2374 format(15x,' ' ,i4.2,'-',i2.2,'-',i2.2,'-',i2.2,e11.3,' 1/day')
 2375 format(2x,'  ',i4.2,'-',i2.2,'-',i2.2,'-',i2.2, 6(e11.3,2x)  )
 2376 format(/'     time       option nosys  delpar subst delwaq subst'/  &
             ,'  dd-hh-mm-ss ',//                                      )
 2377 format( ' ',i4.2,'-',i2.2,'-',i2.2,'-',i2.2,':',  &
                  2(i4,4x),3x,2(i4,6x)    )
 2378 format(/'     time      mud number     delpar subst             '/  &
             ,'  dd-hh-mm-ss '                                         )
 2379 format( ' ',i4.2,'-',i2.2,'-',i2.2,'-',i2.2,':',2(i4,4x)         )
 2389 format(/'   time on file for mud release = ',i9                   )
 3100 format(/,'   Number of particles for calculation set to', i7, '.')
 3115 format(  '  Relative thickness per layer')
 3120 format(  '        layer ',i4,'; relative thickness = ',f12.5    )
 3125 format(/,'  Critical shear stress sedimentation= ',f12.5,' (Pa)',    &
             /,'  Critical shear stress for erosion  = ',f12.5,' (Pa)',    &
             /,'  Chezy value                        = ',f12.5,'(m^1/2 s-1) ')
 3132 format(/,'  Total number of sticking substances = ',i3          )
 3133 format(  '     Sticking number for substance ',i3,' = ',i3      )
!
!     warning formats
!
 2175 format('  Warning 1001. DELWAQ coupling specified; check map-',  &
               'step!')
 2176 format('  Warning 3001. Time ud release on file ',i3,            &
             '  out of simulation                                     ')
!
!     error formats
!
 2015 format('  Error 1001. Model-type-choice', i5, '; out of range!'  )
 2022 format('  Error 1101. Time step is not a diviior of time step ',  &
             '  in hydrodynamic database: interpol. errors will occur' )
 2023 format('  Error 1100. Time step should be less equal than step',  &
              ' in hydrodynamic database '                          )
 2024 format('  Error 1101. History time step should be greater than',  &
              ' zero!'                                                 )
 2025 format('  Error 1102. Number of stations exceeds the ',/,  &
             '         system maximum of',i7,'!'                       )
 2070 format(/' Error 1104. Number of wind variations; min. 2, max.',  &
                i4, '; choosen :', i4, '!'                             )
 2072 format(/' Error 1201. Start time wind series must be equal to',  &
              ' start time simulation!'                                )
 2073 format(/' Error 1202. Wind time serie must be in ascending',     &
              ' order!'                                                )
 2075 format(/' Error 1203. Stop time wind series must be equal to',   &
              ' stop time simulation!'                                 )
 2100 format(/' Error 1301. Start time flow-file: ',i4,'D-',i2,'H-',i2,&
               'M-',i2,'s; times do not match!'                        )
 2120 format('  Error 1302. Simulation time does not divide by time',  &
               ' step of', i6,'s!'                                     )
 2140 format('  Error 1401. Start time of map-file earlier than start',&
              ' of simulation !'                                       )
 2145 format('  Error 1402. Start time of map-file not equal to start',&
              ' time of simulation for delwaq take-over! '             )
 2150 format('  Stop  time map file  :',i4,'D-',i2.2,'H-',i2.2,'M-',   &
                i2.2,'S.')
 2151 format('  Stop  time-histories :',i4,'D-',i2.2,'H-',i2.2,'M-',   &
                i2.2,'S.')
 2160 format('  Error 1403. Stop time of map-file earlier than start', &
              ' time of map-file !'                                    )
 2165 format('  Error 1404. Stop time of map-file not equal to start', &
              ' time of simulation for delwaq take over !'             )
 2166 format('  Error 1405. Start time of time-histories earlier than', &
              ' start time of simulation !'                            )
 2167 format('  Error 1406. Stop time of time-hist. earlier than start',&
              ' time of time-hist.'                                    )
 2200 format('  Error 1501. Number exceeds the system maximum of',i3,  &
                                                                    '!')
 2220 format('  Error 1502. Plot step out of simulation range ! '      )
 2222 format('  Error 1503. Plot step out of ascending order ! '       )
 2224 format('  Error 1504. Time step map output not matching with ',  &
              ' time step hydrodynamics!'                              )
 2270 format('  Error 1601. Dye release time out of simulation range!' )
!2305 format('  Error 1701. No continuous releases for oil allowed!'   )
 2337 format('  Error 1801. Out of range, minimum = 2, maximum =', i5, &
                                                                    '!')
 2360 format('  Error 1802. Start of time series not equal to start',  &
              ' of simulation!'                                        )
 2370 format('  Error 1803. Time steps not in increasing order!'       )
 2380 format('  Error 1901. Stop decay  time series not equal to stop',&
              ' of simulation!'                                        )
 2381 format('  Error 1902. Stop of continuous time series not equal', &
              ' to stop of simulation!'                                )
 2382 format('  Error 1903. Stop settling vel.series not equal to stop',&
              ' of simulation!'                                        )
 2390 format('  Error: invalid zoom grid',/  &
             12x,'this grid is required for output plo and psf file ')
4001  write(*,*) 'Error: version string can not be read correctly'
      call stop_exit(1)
4002  write(*,*) 'Error: 4 title strings can not be read correctly'
      call stop_exit(1)
4003  write(*,*) 'Error: name of hyd file can not be read correctly'
      call stop_exit(1)
4007  write(*,*) &
      'Error: numerical scheme or time-step can not be read correctly'
      call stop_exit(1)
4008  write(*,*) &
       'Error: vert. disperson option, vert.disp scale factor', &
       'or vert.disp constant can not be read correctly'
      call stop_exit(1)
4009  write(*,*) 'Error: names of substances can not be read correctly'
      call stop_exit(1)
4011  write(*,*) 'Error: filename of nh4-file can not be read correctly'
      call stop_exit(1)
4012  write(*,*) 'Error: filename of no3-file can not be read correctly'
      call stop_exit(1)
4013  write(*,*) 'Error: number of particles can not be read correctly'
      call stop_exit(1)
4014  write(*,*) 'Error: roughness length can not be read correctly'
      call stop_exit(1)
4015  write(*,*) 'Error: horiz. disp. params can not be read correctly'
      call stop_exit(1)
4016  write(*,*) 'Error: wind drag coefficien can not be read correctly'
      call stop_exit(1)
40161 write(*,*) 'Error: density of water can not be read correctly'
      call stop_exit(1)
4017  write(*,*) 'Error: no. wind breakpoints can not be read correctly'
      call stop_exit(1)
4018  write(*,*) 'Error: breakpoint for wind can not be read correctly'
      call stop_exit(1)
4019  write(*,*) 'Error: no. model specific constants',  &
       ' can not be read correctly'
      call stop_exit(1)
4020  write(*,*) 'Error: value of a model specific constant', &
       ' can not be read correctly'
      call stop_exit(1)
4021  write(*,*) 'Error: start time of simulation',&
       ' can not be read correctly'
      call stop_exit(1)
4022  write(*,*) 'Error: stop time of simulation', &
       ' can not be read correctly'
      call stop_exit(1)
4023  write(*,*) 'Error: waq take over time can not be read correctly'
      call stop_exit(1)
4024  write(*,*) 'Error: start time of map file',  &
       ' can not be read correctly'
      call stop_exit(1)
4025  write(*,*) 'Error: stop time of map file',   &
       ' can not be read correctly'
      call stop_exit(1)
4026  write(*,*) 'Error: time-step in map file',   &
       ' can not be read correctly'
      call stop_exit(1)
4027  write(*,*) 'Error: start time of his file',  &
       ' can not be read correctly'
      call stop_exit(1)
4028  write(*,*) 'Error: stop time of his file',   &
       ' can not be read correctly'
      call stop_exit(1)
4029  write(*,*) 'Error: time-step in map file',   &
       ' can not be read correctly'
      call stop_exit(1)
4030  write(*,*) 'Error: time offset to real time for plotfile ', &
       ' can not be read correctly'
      call stop_exit(1)
4031  write(*,*) 'Error: no. of observation points ', &
       ' can not be read correctly'
      call stop_exit(1)
4032  write(*,*) 'Error: names/coor. stations can not be read correctly'
      call stop_exit(1)
4033  write(*,*) 'Error: no. plot grids can not be read correctly'
      call stop_exit(1)
4034  write(*,*) 'Error: breakpoint plotgrid can not be read correctly'
      call stop_exit(1)
4038  write(*,*) 'Error: (x,y) of zoom window can not be read correctly'
      call stop_exit(1)
4039  write(*,*) &
      'Error: no. instantaneous releases can not be read correctly'
      call stop_exit(1)
4040  write(*,*)  &
      'Error: names instantaneous releases can not be read correctly'
      call stop_exit(1)
4041  write(*,*) &
      'Error: breakpoint instantaneous rel. can not be read correctly'
      call stop_exit(1)
4042  write(*,*) &
      'Error: (x,y,k) instantaneous release can not be read correctly'
      call stop_exit(1)
4043  write(*,*) 'Error: relative z-coordinate or radius (m) or rel-%', &
       ' of the instantaneous release can not be read correctly'
4044  write(*,*) &
       'Error: mass of substances released for instantaneous release ', &
       ' can not be read correctly'
      call stop_exit(1)
4045  write(*,*) 'Error: no. cont.releases can not be read correctly'
      call stop_exit(1)
4046  write(*,*) 'Error: names cont.releases can not be read correctly'
      call stop_exit(1)
4047  write(*,*) 'Error: (x,y,k) cont.release can not be read correctly'
      call stop_exit(1)
4049  write(*,*) 'Error: scale factor of substance ', &
       ' can not be read correctly'
4050  write(*,*) 'Error: stoechiometric coefficient of substance ', &
       ' can not be read correctly'
      call stop_exit(1)
4051  write(*,*) 'Error: no. breakp. cont.rel can not be read correctly'
      call stop_exit(1)
4052  write(*,*) 'Error: breakpoint cont.rel. can not be read correctly'
      call stop_exit(1)
4053  write(*,*) 'Error: no. of user defined releases ', &
       ' can not be read correctly'
      call stop_exit(1)
4054  write(*,*) 'Error: percentages and scale factors of user def.rel', &
       ' can not be read correctly'
      call stop_exit(1)
4055  write(*,*) 'Error: breakpoint and subst-no. of user def.release', &
       ' can not be read correctly'
      call stop_exit(1)
4056  write(*,*) 'Error: option for file type of user def.release', &
       ' can not be read correctly'
      call stop_exit(1)
4057  write(*,*) 'Error: filename of user defined release', &
       ' can not be read correctly'
      call stop_exit(1)
4058  write(*,*) 'Error: breakpoint and subst-no. of user def.release', &
       ' for the map or restart file from waq can not be read correctly'
      call stop_exit(1)
4059  write(*,*) 'Error: no. of breakpoints for decay rate ', &
       ' can not be read correctly'
      call stop_exit(1)
4060  write(*,*) 'Error: breakp. decay rate can not be read correctly'
      call stop_exit(1)
4061  write(*,*) 'Error: no. of breakpoints for settling velocities ', &
       ' can not be read correctly'
      call stop_exit(1)
4062  write(*,*) 'Error: breakp. settl. vel. can not be read correctly'
      call stop_exit(1)
4063  write(*,*) ' Error: sedimentation-erosion params are missing or', &
       ' can not be read correctly'
      call stop_exit(1)
4064  write(*,*) ' Error: power for concentration dependent settling', &
       ' can not be read correctly'
      call stop_exit(1)
4065  write(*,*) ' Error: grid refinement factor', &
       ' can not be read correctly'
      call stop_exit(1)
5001  write(*,*) 'Error: 1th record of particles coordinates file', &
       ' can not be read correctly'
      call stop_exit(1)
5002  write(*,*) 'Error: 2nd record of particles coordinates file', &
       ' can not be read correctly'
      call stop_exit(1)
5049  write(*,*) 'Error: interpolation options for cont. releases ', &
       ' can not be read correctly'
      call stop_exit(1)
6001  write(*,*) 'Error: option for initial condition (oil) ', &
       ' can not be read correctly'
      call stop_exit(1)
6002  write(*,*) 'Error: file name for initial condition (ini-file)', &
       ' can not be read correctly (warm start)'
      call stop_exit(1)
      end subroutine
end module
