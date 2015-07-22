module part09_mod
!
contains
      subroutine part09 ( lun2   , itime  , nodye  , nwaste , mwaste ,  &
                          xwaste , ywaste , iwtime , amassd , aconc  ,  &
                          npart  , mpart  , xpart  , ypart  , zpart  ,  &
                          wpart  , iptime , nopart , radius , lgrid  ,  &
                          dx     , dy     , ndprt  , nosubs , kpart  ,  &
                          layt   , tcktot , nplay  , kwaste , nolay  ,  &
                          modtyp , zwaste , track  , nmdyer , substi )

!       Deltares Software Centre

!     System administration : Antoon Koster

!     Created               : February 1990 by Leo Postma

!     Modified              : May      1996 by Robert Vos    : 3d version
!                             January  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     Note                  : none

!     Logical unit numbers  : lun2 - output file to print statistics

!     Subroutines called    : find - distributes particles over a circel

!     functions   called    : rnd  - random number generator

      use precision          ! single/double precision

      implicit none

!     Arguments

!     kind            function         name                    description

      integer  ( ip), intent(in   ) :: nodye                 !< nr of dye release points
      integer  ( ip), intent(in   ) :: nosubs                !< nr of substances
      integer  ( ip), intent(in   ) :: layt                  !< number of hydr. layer
      integer  ( ip), intent(in   ) :: itime                 !< actual time
      integer  ( ip), intent(inout) :: iwtime (nodye)        !< array of wasteload times
      integer  ( ip), intent(in   ) :: nwaste (nodye)        !< n-values of waste locations
      integer  ( ip), intent(in   ) :: mwaste (nodye)        !< m-values of waste locations
      real     ( rp), intent(in   ) :: xwaste (nodye)        !< x-values of waste locations
      real     ( rp), intent(in   ) :: ywaste (nodye)        !< y-values of waste locations
      real     ( rp), intent(in   ) :: zwaste (nodye)        !< z-values of waste locations
      real     ( rp), intent(in   ) :: amassd (nosubs,nodye) !< total masses per dye release
      real     ( rp), pointer       :: aconc  (:,:)          !< mass per particle
      integer  ( ip), intent(  out) :: npart  (*)            !< n-values particles
      integer  ( ip), intent(in   ) :: ndprt  (nodye)        !< no. particles per waste entry
      integer  ( ip), intent(  out) :: mpart  (*)            !< m-values particles
      real     ( rp), intent(  out) :: xpart  (*)            !< x-in-cell of particles
      real     ( rp), intent(  out) :: ypart  (*)            !< y-in-cell of particles
      real     ( rp), intent(  out) :: zpart  (*)            !< z-in-cell of particles
      real     ( rp), intent(  out) :: wpart  (nosubs,*)     !< weight of the particles
      integer  ( ip), intent(  out) :: iptime (*)            !< particle age
      integer  ( ip), intent(inout) :: nopart                !< number of active particles
      real     ( rp), intent(in   ) :: radius (nodye)        !< help var. radius (speed)
      integer  ( ip), pointer       :: lgrid  (:,:)          !< grid numbering active
      real     ( rp), pointer       :: dx     (:)            !< dx of the grid cells
      real     ( rp), pointer       :: dy     (:)            !< dy of the grid cells
      integer  ( ip), intent(in   ) :: modtyp                !< for model type 2 temperature
      integer  ( ip), intent(in   ) :: lun2                  !< output report unit number
      integer  ( ip), intent(  out) :: kpart  (*)            !< k-values particles
      real     ( rp), intent(in   ) :: tcktot (layt)         !< thickness hydrod.layer
      integer  ( ip)                :: nplay  (layt)         !< work array that could as well remain inside
      integer  ( ip), intent(inout) :: kwaste (nodye)        !< k-values of dye points
      integer  ( ip), intent(in   ) :: nolay                 !< number of comp. layer
      real     ( rp), intent(inout) :: track  (8,*)          !< track array for all particles
      character( 20), intent(in   ) :: nmdyer (nodye)        !< names of the dye loads
      character( 20), intent(in   ) :: substi (nosubs)       !< names of the substances

      return

      end subroutine
end module
