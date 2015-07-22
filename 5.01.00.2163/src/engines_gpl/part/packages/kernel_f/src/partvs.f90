      subroutine partvs ( lun2   , itime  , nosubs , nopart , ivtset ,   &
                          ivtime , vsfour , vsfact , wpart  , wsettl )

!       Deltares Software Centre

!     System administration : Antoon Koster

!     Created      : June 1996      by Robert Vos

!     Modified     : July 2011      by Leo Postma, cosmetic redesign and OMP paralellism
!                    January 2013   by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     logical unit numbers  : lun2  output report file

!     subroutines called    : none.

!     functions   called    : none.

      use precision    ! single/double precision

      implicit none    ! explicit typing

!     Arguments

!     kind            function         name                      description

      integer  ( ip), intent(in   ) :: lun2                    !< unit of output report file
      integer  ( ip), intent(in   ) :: itime                   !< actual time
      integer  ( ip), intent(in   ) :: nosubs                  !< number of substances
      integer  ( ip), intent(in   ) :: nopart                  !< number of particles
      integer  ( ip), intent(in   ) :: ivtset                  !< number of time breakpoints
      integer  ( ip), intent(in   ) :: ivtime(ivtset)          !< time breakpoint values settling velocities
      real     ( rp), intent(in   ) :: vsfour(6,nosubs,ivtset) !< settling velocity parameters
      real     ( rp)                :: vsfact(6,nosubs)        !< local work array
      real     ( rp), intent(in   ) :: wpart (  nosubs,nopart) !< weight of substances per particle
      real     ( rp), intent(  out) :: wsettl(         nopart) !< actual settling velocity per particle

      return

      end subroutine
