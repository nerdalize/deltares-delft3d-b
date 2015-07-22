      subroutine wrttrk ( lundia , fout   , filnam , ittrkc , ntrk   ,     &
                          npmax  , xa     , ya     , za     , xyztrk )
!
!-----------------------------------------------------------------------
!          Deltares
!
!             module: subroutine wrttrk
!           function: writes the time varying groups (2 & 3) to the
!                     nefis particle tracking file
!
!                     derived from wrtdro drogue subroutine in flow
!                     adapted for particle tracking
!             date  : 30-09-2001
!           modified: january 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
      use precision       ! single and double precision

      implicit none       ! force explicit typing

!     Arguments

!     kind           function         name                      description

      integer  ( ip), intent(in   ) :: lundia                  !< unit nr of the diagnostics file
      logical       , intent(  out) :: fout                    !< output is written
      character( * )                :: filnam                  !< name of the output file
      integer  ( ip)                :: ittrkc
      integer  ( ip), intent(in   ) :: ntrk                    !< number of particles to track
      integer  ( ip), intent(in   ) :: npmax                   !< total number of particles
      real     ( rp), intent(in   ) :: xa    (npmax)           !< x of the particles
      real     ( rp), intent(in   ) :: ya    (npmax)           !< y of the particles
      real     ( rp), intent(in   ) :: za    (npmax)           !< z of the particles
      real     ( rp), intent(  out) :: xyztrk(  3  , npmax)    !< work array to padd the particles in

      return
      end subroutine
