module part18_mod
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
      subroutine part18 ( lgrid  , velo   , conc   , flres  , volume , &
                          area   , mnmaxk , npart  , mpart  , wpart  , &
                          zpart  , nopart , idelt  , nolay  , npwndw , &
                          vdiff  , pblay  , ptlay  , const  , nocons , &
                          lun2   , nosubs , layt   , kpart  , icvdf  , &
                          wvelo  , alpha  , nosubc , icvdf2    )
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.30
!
!
!     system administration : r.j. vos
!
!
!     created               : january 1991, by a. markus
!
!
!     modified              : cleared may 1996, 3d version
!                             26/7/1996:
!                             v3.12: delwaq map is standard for conc-array
!                             v3.20: recalculates dispersion without depth-aver.
!                             11/10/1996: corrected for error in 3d version
!                                         (icvdf2 = icvdf + nosubs)
!                                         (lead. dim of conc array is nosubc)
!                             v3.30: icvdf2 as argument
!                             January  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!     function              : calculates the exchange between the
!                             two layers and exchanges the particles
!                             as part of the 3d version
!
!
!     dimensioning
!
      integer(ip),dimension(:)    :: npart , mpart , kpart
      integer(ip),dimension(:,:)  :: lgrid
!
!     dimensioning
!
      real   (sp),dimension(:)    :: vdiff , velo  , volume , area , const , zpart
      real   (dp),dimension(:)    :: wvelo
      real   (sp),dimension(:,:)  :: conc
      real   (sp),dimension(:,:)  :: flres
      real   (sp),dimension(:,:)  :: wpart
!
!     local scalars
!
      integer(ip) ::  icvdf  , icvdf2 , idelt , layt  , lun2
      integer(ip) ::  mnmaxk, nocons, nolay , nopart
      integer(ip) ::  nosubc, nosubs, npwndw
      real   (sp) ::  alpha , pblay  , ptlay

      return
!
      end subroutine
end module

