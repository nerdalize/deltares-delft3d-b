      subroutine part17 ( itime  , nosubs , idtset , idtime , decay  ,          &
                          decays )

!       Deltares Software Centre

!     system administration : Antoon Koster

!     Created               : February  1990 by Leo Postma

!     Modified              : July      1992 by Robert Vos : nosubs substances
!                             July      2011 by Leo Postma : cosmetics
!                             January   2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     Subroutines called    : none

!     Functions called      : none

!     Logical units         : none

      use precision    ! single/double precision

      implicit none    ! explicit typing

!     Arguments

!     kind            function         name                    description

      integer  ( ip), intent(in   ) :: itime                 !< actual time
      integer  ( ip), intent(in   ) :: nosubs                !< number of substances
      integer  ( ip), intent(in   ) :: idtset                !< number of time breakpoints
      integer  ( ip), intent(in   ) :: idtime(idtset)        !< time breakpoint values
      real     ( rp), intent(in   ) :: decay (nosubs,idtset) !< time series of decay factors
      real     ( rp), intent(  out) :: decays(nosubs)        !< interpolated decay factors

      return
!
      end subroutine
