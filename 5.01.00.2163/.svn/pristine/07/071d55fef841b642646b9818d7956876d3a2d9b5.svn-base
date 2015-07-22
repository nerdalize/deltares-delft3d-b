module partwr_mod
!
!  module declarations
!
!  data definition module(s)
!
use precision            ! single/double precision
!
implicit none            ! force explicit typing
!
contains
      subroutine partwr ( lgrid  , nmax   , conc   , volume , area   , &
                          npart  , mpart  , wpart  , zpart  , npwndw , &
                          nopart , idelt  , const  , nocons , buffer , &
                          lun2   , nolay  , mmax   , itime  , kpart  , &
                          mapsub , isfile , mnmaxk , mnmax2 , finnh4 , &
                          finno3         )
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.40
!
!
!     system administration : m. zeeuw
!
!
!     created               : 26 june 1996 by r.j. vos for kajima
!                             red tide model according to
!                             yanagi t. et al.,j. marine systems 6(1995)269-285
!                             26 july 1996: incorporated light limitation,
!                             according to steele, also growth at night due to
!                             due to nutrients is allowed
!
!     modified              : cleared 1996 by rj vos for 3d
!                             26 july 1996, conc. like for standard delwaq mapfile
!                             i.e. layers are not substances
!                             nov 1997: open files with openfl (v3.40)
!                             april 1998: correcte comment statement before goto 4
!                             January 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!
!     function              : calculates the processes that change
!                             the mass of individual particles
!
!     functions   called    : none.
!
!
      character(len=256) :: finnh4, finno3
!
!     dimensioning
!
      integer(ip), dimension(:)   :: isfile
      integer(ip), dimension(:)   :: kpart , mapsub
      integer(ip), dimension(:)   :: npart , mpart
      integer(ip), dimension(:,:) :: lgrid
!
      real   (sp), dimension(:)   :: buffer
      real   (sp), dimension(:)   :: volume, area  , const , zpart
      real   (sp), dimension(:,:) :: conc
      real   (sp), dimension(:,:) :: wpart
!
!     local scalars
!
      integer(ip) ::  idelt
      integer(ip) ::  mnmax2 , nopart, npwndw
      integer(ip) ::  itime  , lun2  , nocons
      integer(ip) ::  mnmaxk , nolay
      integer(ip) ::  mmax   , nmax

      return
      end subroutine
end module
