module part10_mod
!
!  module declarations
!
contains
      subroutine part10 ( lgrid  , volume , flow   , dx     , dy     ,      &
                          area   , angle  , nmax   , mnmaxk , idelt  ,      &
                          nopart , npart  , mpart  , xpart  , ypart  ,      &
                          zpart  , iptime , rough  , drand  , lgrid2 ,      &
                          wvelo  , wdir   , decays , wpart  , pblay  ,      &
                          npwndw , vdiff  , nosubs , dfact  , modtyp ,      &
                          t0buoy , abuoy  , kpart  , mmax   , layt   ,      &
                          wsettl , depth  , ldiffz , ldiffh , lcorr  ,      &
                          acomp  , ipc    , accur  , xcor   , ycor   ,      &
                          tcktot , lun2   , alpha  , mapsub , nfract ,      &
                          taucs  , tauce  , chezy  , rhow   , lsettl ,      &
                          mstick , nstick , ioptdv , cdisp  , dminim ,      &
                          fstick , defang , floil  , xpart0 , ypart0 ,      &
                          xa0    , ya0    , xa     , ya     , npart0 ,      &
                          mpart0 , za     , locdep , dps    , nolay  ,      &
                          vrtdsp , stickdf, subst  , nbmax  , nconn  ,      &
                          conn   , tau    , caltau )
!
!    CALCULATES PARTICLE MOTION FROM ADVECTION, DISPERSION AND SETTLING
!                 (per time step)
!

!     system administration : frank kleissen


!     created               : january 1990, by l. postma

!     modified              : january 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     logical unit numbers  : standard output: error messages
!                             lun2           : error and debug messages

!
!  data definition module(s)
!
      use precision        ! single/double precision
      use typos
!
      implicit none             ! force explicit typing

      real   (sp), external      :: rnd                 ! random number function

!**   parameters used for dimensioning

      integer(ip), intent(in)    :: layt                ! number of layers of hydr. database
      integer(ip), intent(in)    :: mmax                ! second grid dimension
      integer(ip), intent(in)    :: mnmaxk              ! total number of active grid cells
      integer(ip), intent(in)    :: nmax                ! first grid dimension
      integer(ip), intent(in)    :: nopart              ! total number of particles
      integer(ip), intent(in)    :: nosubs              ! number of substances per particle

!**   other parameters

      integer(ip), intent(in)    :: idelt               ! time step size in seconds
      integer(ip), intent(in)    :: ioptdv              ! if 0 constant vertical diffusion &
                                                        ! if 1 depth averaged algebraic model
      integer(ip), intent(in)    :: ipc                 ! if > 1 predictor corrector method used &
                                                        ! if   5 something special happens
      integer(ip), pointer    :: lgrid ( : , : )     ! grid with active grid numbers, negatives for open boundaries
      integer(ip), pointer    :: lgrid2( :, : )      ! total grid with grid numbers
      integer(ip), intent(in)    :: lun2                ! unit number debug in formation file
      integer(ip), pointer    :: mapsub( : )         ! index for substances, used for oil
      integer(ip), intent(in)    :: modtyp              ! 1 = tracer model              &
                                                        ! 2 = 2-layer temperature model &
                                                        ! 3 = red tide model            &
                                                        ! 4 = oil model                 &
                                                        ! 5 = 1-layer temperature model
      integer(ip), intent(in)    :: nfract              ! nr of oil fractions, each fraction 3 substances &
                                                        ! floating, dispersed and sticked
      integer(ip), intent(in)    :: npwndw              ! first active particle
      integer(ip), intent(in)    :: nstick              ! number of sticked particles !!!!! in !!!!!
      integer(ip), pointer :: iptime( : )         ! particle age in seconds
      integer(ip), pointer :: kpart ( : )         ! third grid index of the particles
      integer(ip), pointer :: mpart ( : )         ! second grid index of the particles
      integer(ip), pointer :: mpart0( : )         ! second grid index particles for previous time step
      integer(ip), pointer :: mstick( : )         ! which active substances can sticking (>0) and what is inactive partner?
                                                        ! j = mstick(i), j = inactive, i = active ; if j = 0 no sticking
                                                        ! if j is negative then i itself is sticking
      integer(ip), intent(inout) :: nolay               ! number of layers == layt
      integer(ip), pointer :: npart ( : )         ! first  grid index of the particles
      integer(ip), pointer :: npart0( : )         ! first  grid index particles for previous time step
      integer(ip), pointer :: floil ( : )         ! contains values 1 or 0
!
      logical    , intent(in)    :: acomp               ! use an analytical function for umagi
      logical    , intent(in)    :: lcorr               ! if true, apply the corrector step
      logical    , intent(in)    :: ldiffh              ! horizontal diffusion is on/off
      logical    , intent(in)    :: ldiffz              ! vertical diffusion is on/off
      logical    , intent(in)    :: lsettl              ! if on deposition/erosion may occur at the bed
!
      real   (sp), pointer    :: abuoy ( : )         ! dispersion coefficient buoyancy
      real   (sp), intent(in)    :: accur               ! accuracy limit taylor expansion
      real   (sp), intent(in)    :: alpha               ! scale factor vertical diffusion
      real   (sp), pointer    :: angle ( : )         ! angle with horizontal
      real   (sp), pointer    :: area  ( :  )        ! horizontal surface areas
      real   (sp), intent(in)    :: cdisp               ! constant vertical diffusivity in m2/s
      real   (sp), intent(in)    :: dminim              ! minimum value vertical diffusivity in m2/s
      real   (sp), pointer    :: decays( :  )        ! decay coefficients of substances in 1/day
      real   (sp), pointer    :: depth ( :  )        ! depth of segment  (in m)
      real   (sp), intent(in)    :: drand ( 3 )         ! drand(1) is 2.0*sqrt(dt*a) in D = a*t^b &
                                                        ! drand(2) is 0.5*b          in D = a*t^b &
                                                        ! drand(3) is winddrag coefficient in %
      real   (sp), pointer    :: flow  ( :    )      ! all flows
      real   (sp), pointer    :: fstick( : )         ! part of mass that sticks to land
      real   (sp), intent(in)    :: pblay               ! relative thickness of bottom layer ( 0.0 - 1.0 )
      real   (sp), intent(in)    :: rhow                ! density of water in g/l (= kg/m3)
      real   (sp), intent(in)    :: rough               ! bottom roughness length (m)
      real   (sp), pointer    :: t0buoy( : )         ! t0-coeff buoyancy
      real   (sp), intent(in)    :: tauce               ! critical shear stress for erosion (pa)
      real   (sp), intent(in)    :: taucs               ! critical shear stress for sedimentation (pa)
      real   (sp), pointer    :: volume( : )         ! volumes of all computational elements
      real   (dp), intent(in)    :: wdir (:)            ! wind direction from north
      real   (dp), intent(in)    :: wvelo(:)            ! wind velocity
      real   (sp), pointer    :: xcor  ( : )         ! bottom coordinate x
      real   (sp), pointer    :: ycor  ( : )         ! bottom coordinate y
      real   (sp), intent(inout) :: chezy               ! chezy coefficient (is set to 50 if .le. 1.0)
      real   (sp), intent(inout) :: defang              ! deflection angle for 3d oil simulations
                                                        ! enters in degrees, becomes radians
      real   (sp), pointer :: dfact ( : )         ! decay factor ( is exp(-decays*t) )
      real   (sp), pointer :: dps   ( : )         ! depths
      real   (sp), pointer :: dx    ( : )         ! dx of the computational elements
      real   (sp), pointer :: dy    ( : )         ! dy of the computational elements
      real   (sp), pointer :: locdep( :, : )      ! depth per layer
      real   (sp), pointer :: tcktot( : )         ! relative thickness of the layers
      real   (sp), pointer :: wpart ( :, :)       ! weight factors of the subs per particle
      real   (sp), pointer :: wsettl( : )         ! settling per particle
      real   (sp), pointer :: xa    ( : )         !
      real   (sp), pointer :: xa0   ( : )         !
      real   (sp), pointer :: xpart ( : )         ! x-value (0.0-1.0) first  direction within grid cell
      real   (sp), pointer :: xpart0( : )         ! x of particles for previous time step
      real   (sp), pointer :: ya    ( : )         !
      real   (sp), pointer :: ya0   ( : )         !
      real   (sp), pointer :: ypart ( : )         ! y-value (0.0-1.0) second direction within grid cell
      real   (sp), pointer :: ypart0( : )         ! y of particles for previous time step
      real   (sp), pointer :: za    ( : )         !
      real   (sp), pointer :: zpart ( : )         ! z-value (0.0-1.0) third  direction within grid cell
      real   (sp), pointer :: vdiff ( : )         ! vertical diffusion - work array
      real   (sp), pointer :: vrtdsp( :,: )       ! storage of vert disp info for debugging
      character(len=*), pointer  :: subst ( : )         ! substance names per substance &
                                                        ! and per layer for the plo file
      integer(ip), intent(in   ) :: stickdf             ! if 1 oil sticks at drying flats
      integer(ip), intent(in   ) :: nbmax         ! highest regular open boundary number
      integer(ip), intent(in   ) :: nconn         ! number of interdomain connections
      type( pnt ), intent(in   ) :: conn (nconn)  ! array with interdomain connections
      real   (sp), pointer       :: tau   ( : )   ! tau
      logical    , intent(in   ) :: caltau        ! if true, tau must be calculated

      return

      end subroutine part10

 end module
