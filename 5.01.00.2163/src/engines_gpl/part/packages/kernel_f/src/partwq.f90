module partwq_mod
!
!  module declarations
!
!  data definition module(s)
!
use precision               ! single/double precision
!
implicit none               ! force explicit typing
!
contains
      subroutine partwq ( lgrid  , nmax   , conc   , volume , area   , &
                          npart  , mpart  , wpart  , radius , nodye  , &
                          npwndw , nopart , idelt  , velo   , wvelo  , &
                          const  , nocons , ptlay  , lun2   , nosubs , &
                          nolay  , lgrid2 , mmax   , xb     , yb     , &
                          t0cf   , acf    , nwaste , mwaste , kpart  , &
                          mapsub , layt   , mnmaxk       )
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.60
!
!
!     system administration : m. zeeuw
!
!
!     created               : 25 may 1994, by a. markus for sizewell
!
!     modified              : march 1999 by rj vos for 3d-temperature model
!                             in this case parameters pblay and gamma are not used
!                             number of constants per outfall/intake increased
!                             to 10 (see ncload)
!                             January 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!     function              : calculates the processes that change
!                             the mass of individual particles
!
!
!
!     subroutines called    : stop_exit.
!
!
!     functions   called    : none.
!
!
!
!     parameters
!
      integer(ip), pointer, dimension(:)     :: npart , mpart
      integer(ip), pointer, dimension(:,:)   :: lgrid , lgrid2
      real   (sp), pointer, dimension(:)     :: volume, area  , velo  , radius , const , xb , yb
      real   (dp), pointer, dimension(:)     :: wvelo
      real   (sp), pointer, dimension(:,:)   :: conc
      real   (sp), pointer, dimension(:,:)   :: wpart

      integer(ip), pointer, dimension(:)     :: kpart    , mapsub
      integer(ip), pointer, dimension(:)     :: nwaste   , mwaste
      real   (sp), pointer, dimension(:)     :: t0cf     , acf

      integer(ip), parameter        :: maxld = 50

      integer(ip) ::  idelt
      integer(ip) ::  layt
      integer(ip) ::  lun2
      integer(ip) ::  nodye , mmax   , mnmaxk
      integer(ip) ::  nocons , nolay  , nmax   , nosubs , nopart
      integer(ip) ::  npwndw

      real   (sp) ::  ptlay

      return
!
      end subroutine
end module

