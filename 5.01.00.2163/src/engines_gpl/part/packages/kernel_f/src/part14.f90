module part14_mod

   contains
      subroutine part14 ( itime  , idelt  , nodye  , nocont , ictime ,    &
                          ictmax , nwaste , mwaste , xwaste , ywaste ,    &
                          zwaste , aconc  , rem    , npart  , ndprt  ,    &
                          mpart  , xpart  , ypart  , zpart  , wpart  ,    &
                          iptime , nopart , pblay  , radius , lgrid  ,    &
                          dx     , dy     , ftime  , tmassu , nosubs ,    &
                          ncheck , t0buoy , modtyp , abuoy  , t0cf   ,    &
                          acf    , lun2   , kpart  , layt   , tcktot ,    &
                          nplay  , kwaste , nolay  , linear , track  ,    &
                          nmconr  )

!       Deltares Software Centre

!     System administration : Antoon Koster

!     Created               : February 1990 by Leo Postma

!     Modified              : May      1996 by Robert Vos    : 3d version
!                             July     2001 by Antoon koster : mass conservation &
!                                                              suppression round off errors
!                             January  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     Note                  : none

!     Logical unit numbers  : lun2 - output file to print statistics

      use precision          ! single/double precision

      implicit none

!     Arguments

!     kind            function         name                    description

      integer  ( ip), intent(in   ) :: nocont                !< nr of continuous loads
      integer  ( ip), intent(in   ) :: nodye                 !< nr of dye release points
      integer  ( ip), intent(in   ) :: nosubs                !< nr of substances
      integer  ( ip), intent(in   ) :: layt                  !< number of hydr. layer
      integer  ( ip), intent(in   ) :: itime                 !< actual time
      integer  ( ip), intent(in   ) :: idelt                 !< time step size
      integer  ( ip), intent(in   ) :: ictime (nocont,*)     !< array of breakpoint times
      integer  ( ip), intent(in   ) :: ictmax (nocont)       !< nr of breakpoints per load
      integer  ( ip), intent(in   ) :: nwaste (nodye+nocont) !< n-values of waste locations
      integer  ( ip), intent(in   ) :: mwaste (nodye+nocont) !< m-values of waste locations
      real     ( rp), intent(in   ) :: xwaste (nodye+nocont) !< x-values of waste locations
      real     ( rp), intent(in   ) :: ywaste (nodye+nocont) !< y-values of waste locations
      real     ( rp), intent(in   ) :: zwaste (nodye+nocont) !< z-values of waste locations
      real     ( rp), intent(in   ) :: aconc  (nocont+nodye,nosubs)    !< mass per particle
      real     ( rp), intent(inout) :: rem    (nocont)       !< remainder of mass to be released
      integer  ( ip), intent(  out) :: npart  (*)            !< n-values particles
      integer  ( ip), intent(in   ) :: ndprt  (nodye+nocont) !< no. particles per waste entry
      integer  ( ip), intent(  out) :: mpart  (*)            !< m-values particles
      real     ( rp), intent(  out) :: xpart  (*)            !< x-in-cell of particles
      real     ( rp), intent(  out) :: ypart  (*)            !< y-in-cell of particles
      real     ( rp), intent(  out) :: zpart  (*)            !< z-in-cell of particles
      real     ( rp), intent(  out) :: wpart  (nosubs,*)     !< weight of the particles
      integer  ( ip), intent(  out) :: iptime (*)            !< particle age
      integer  ( ip), intent(inout) :: nopart                !< number of active particles
      real     ( rp), intent(in   ) :: pblay                 !< relative thickness lower layer
      real     ( rp), intent(in   ) :: radius (nodye+nocont) !< help var. radius (speed)
      integer  ( ip), pointer       :: lgrid  (:,:)          !< grid numbering active
      real     ( rp), pointer       :: dx     (:)            !< dx of the grid cells
      real     ( rp), pointer       :: dy     (:)            !< dy of the grid cells
      real     ( rp), intent(in   ) :: ftime  (nocont,*)     !< time matrix for wasteloads (mass/s)
      real     ( rp), intent(in   ) :: tmassu (nocont)       !< total unit masses cont releases
      integer  ( ip), intent(  out) :: ncheck (nocont)       !< check number of particles per load
      real     ( rp), intent(  out) :: t0buoy (*)            !< t0 for particles for buoyancy spreading
      integer  ( ip), intent(in   ) :: modtyp                !< for model type 2 temperature
      real     ( rp), intent(  out) :: abuoy  (*)            !< 2*sqrt(a*dt) particles-buoyancy spreading
      real     ( rp), intent(in   ) :: t0cf   (nocont)       !< coefficients for loads
      real     ( rp), intent(in   ) :: acf    (nocont)       !< coefficients for loads
      integer  ( ip), intent(in   ) :: lun2                  !< output report unit number
      integer  ( ip), intent(  out) :: kpart  (*)            !< k-values particles
      real     ( rp), intent(in   ) :: tcktot (layt)         !< thickness hydrod.layer
      integer  ( ip)                :: nplay  (layt)         !< work array that could as well remain inside
      integer  ( ip), intent(in   ) :: kwaste (nodye+nocont) !< k-values of wasteload points
      integer  ( ip), intent(in   ) :: nolay                 !< number of comp. layer
      integer  ( ip), intent(in   ) :: linear (nocont)       !< 1 = linear interpolated loads
      real     ( rp), intent(inout) :: track  (8,*)          !< track array for all particles
      character( 20), intent(in   ) :: nmconr (nocont)       !< names of the continuous loads

      return

      end subroutine
end module
