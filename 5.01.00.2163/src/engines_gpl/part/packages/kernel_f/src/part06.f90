      subroutine part06 ( lun    , lgrid  , lgrid2 , nmax   , mmax   ,      &
                          xb     , yb     , nodye  , nocont , xwaste ,      &
                          ywaste , nwaste , mwaste )

!       Deltares Software Centre

!>\file
!>            Determines the grid cells and relative coordinates of waste locations
!>
!>            The wastelocations are given by the user in global x,y coordinates.\n
!>            This routine determines the n,m grid indices and the local x,y coordinates.\n
!>            The local x,y coordinates are 0< .. <1 and are store in the old x,y locations

!     System administration : Antoon Koster

!     Created               : February 1990, by Leo Postma

!     Modified              : August   2011, by Leo Postma, only warning if outside grid

!     modified              : january 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     logical unit numbers  : lun    - output log file

      use precision
      
      implicit none

!     Arguments

!     kind            function         name                   description

      integer  ( ip), intent(in   ) :: lun                  !< unit number output log file
      integer  ( ip), intent(in   ) :: nmax                 !< first index hydrodynamic grid
      integer  ( ip), intent(in   ) :: mmax                 !< second index hydrodynamic grid
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)    !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)    !< total grid matrix
      real     ( rp), intent(in   ) :: xb    (nmax*mmax)    !< x of grid corner points
      real     ( rp), intent(in   ) :: yb    (nmax*mmax)    !< y of grid corner points
      integer  ( ip), intent(in   ) :: nodye                !< number of dye releases
      integer  ( ip), intent(in   ) :: nocont               !< number of continuous release
      real     ( rp), intent(inout) :: xwaste(nodye+nocont) !< x of wasteload location
      real     ( rp), intent(inout) :: ywaste(nodye+nocont) !< y of wasteload location
      integer  ( ip), intent(  out) :: nwaste(nodye+nocont) !< first grid index wasteloads
      integer  ( ip), intent(  out) :: mwaste(nodye+nocont) !< second grid index wasteloads
      return

      end subroutine
