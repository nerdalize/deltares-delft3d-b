      subroutine getdps ( lunpr  , lundp  , lnam   , nmax   , mmax   ,      &
     &                    nosegl , dps    , cellpnt, ltrack )
!
!     programmer : antoon koster

!     function   : read bathymetry from *.dps file,
!                  as created by coup203
!                  note that the bathymetry is defined
!                  w.r.t. reference level
!     date       : feb 2003
!
!     modified   : jan 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!     note:
!       1. note that the depths in this routine refer to
!          depths w.r.t. reference level, so called
!          bathymetry.
!
!       2. as these depths are only required in case of
!          particle tracking, this file may be missing
!          in other cases (see itrack)

      use precision      ! single and double precision

      implicit none

!     Arguments

!     kind           function         name                  description

      integer  ( ip), intent(in   ) :: lunpr               !< unit nr of the diagnostics file
      integer  ( ip), intent(in   ) :: lundp               !< unit nr of the depth file
      character( * ), intent(in   ) :: lnam                !< name of the depth file
      integer  ( ip), intent(in   ) :: nmax                !< first dimension of the grid
      integer  ( ip), intent(in   ) :: mmax                !< second dimension of the grid
      integer  ( ip), intent(in   ) :: nosegl              !< nr of active segments of a layer
      real     ( rp), intent(  out) :: dps   (nmax*mmax)   !< array with depth values
      integer  ( ip), intent(in   ) :: cellpnt(nosegl)     !< backpointer of nosegl to mnmax
      logical       , intent(in   ) :: ltrack              !< if .true. then particle tracing

      return

      end subroutine
