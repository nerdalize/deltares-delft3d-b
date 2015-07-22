      subroutine part11( lgrid  , xp     , yp     , nmax   , npart  ,        &
                         mpart  , xpart  , ypart  , xa     , ya     ,        &
                         nopart , npwndw , lgrid2 , kpart  , zpart  ,        &
                         za     , locdep , dps    , nolay  , mmax   ,        &
                         tcktot )

!       Deltares Software Centre

!>\file
!>         Calculates real world x,y,z from the particles in-cell position
!>
!>         Interpolates bi-linearly between the corner coordinates of the particle cell.
!>         That is why both active grid (lgrid) and the total (lgrid2) are needed.\n
!>         It seems that in part10 the n,m of non-floating particles is set to a negative
!>         number to exclude them from this computation (needs to be checked).

!     System administration : Antoon Koster

!     Created               : Januari 1990 by Leo Postma

!     Modified              : March   1991 by Arjen Markus : use both lgrid and lgrid2
!                             January 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     Logical unit numbers  : none

!     Subroutines called    : none

!     Functions   called    : none

      use precision               ! single/double precision

      implicit none               ! force explicit typing

!     Arguments

!     kind            function         name                      description

      integer  ( ip), intent(in   ) :: nmax                    !< first grid index
      integer  ( ip), intent(in   ) :: mmax                    !< second grid index
      integer  ( ip), intent(in   ) :: nolay                   !< number of layers
      integer  ( ip), intent(in   ) :: npwndw                  !< start nr of active particle
      integer  ( ip), intent(in   ) :: nopart                  !< total number of active particles
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)       !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)       !< total grid matrix
      real     ( rp), intent(in   ) :: xp    (nmax*mmax)       !< x of the grid cell corner
      real     ( rp), intent(in   ) :: yp    (nmax*mmax)       !< y of the grid cell corner
      real     ( rp), intent(in   ) :: locdep(nmax*mmax,nolay) !< local depth of a gridcell
      real     ( rp), intent(in   ) :: tcktot(nolay )          !< relative thickness of a layer
      real     ( rp), intent(in   ) :: dps   (nmax*mmax)       !< depth of the reference plain
      integer  ( ip), intent(in   ) :: npart (nopart)          !< first grid cell index particles
      integer  ( ip), intent(in   ) :: mpart (nopart)          !< second grid cell index particles
      integer  ( ip), intent(in   ) :: kpart (nopart)          !< third grid cell index particles
      real     ( rp), intent(in   ) :: xpart (nopart)          !< x-in the grid of particles
      real     ( rp), intent(in   ) :: ypart (nopart)          !< y-in the grid of particles
      real     ( rp), intent(in   ) :: zpart (nopart)          !< z-in the grid of particles
      real     ( rp), intent(  out) :: xa    (nopart)          !< absolute x of particles
      real     ( rp), intent(  out) :: ya    (nopart)          !< absolute y of particles
      real     ( rp), intent(  out) :: za    (nopart)          !< absolute z of particles

      return
!
      end subroutine part11

