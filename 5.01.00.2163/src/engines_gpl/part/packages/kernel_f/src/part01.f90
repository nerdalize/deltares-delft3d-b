      subroutine part01 ( lgrid  , lgrid2 , xp     , yp     , dx     ,  &
                          dy     , area   , angle  , nmax   , mmax   )
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v1.30
!
!
!     system administration : m. zeeuw
!
!
!     created               : january 1990, by l.postma
!
!     modified              : january 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!     function              : computes distances in the grid
!
!
!     note                  : none.
!
!
!     logical unit numbers  : none.
!
!
!     subroutines called    : none.
!
!
!     functions   called    : none.
!
!
      use precision       ! single/double precision

      implicit none       ! force explicit typing

!     parameters

!     kind         function         name                    Descriptipon

      integer(ip), intent(in   ) :: nmax                  !< first dimension lgrid
      integer(ip), intent(in   ) :: mmax                  !< second dimension lgrid
      integer(ip), intent(in   ) :: lgrid (nmax,mmax)     !< active grid indices matrix
      integer(ip), intent(in   ) :: lgrid2(nmax,mmax)     !< total grid indices matrix
      real   (sp), intent(in   ) :: xp    (nmax*mmax)     !< x of upper right corner grid point
      real   (sp), intent(in   ) :: yp    (nmax*mmax)     !< y of upper right corner grid point
      real   (sp), intent(  out) :: dx    (nmax*mmax)     !< x distance of grid cell
      real   (sp), intent(  out) :: dy    (nmax*mmax)     !< y distance of grid cell
      real   (sp), intent(  out) :: area  (nmax*mmax)     !< horizontal surface area
      real   (sp), intent(  out) :: angle (nmax*mmax)     !< angle of the gridcell

!     local scalars

      real   (sp), parameter        :: default = 999.99

      return
!
      end subroutine
