      subroutine part15 ( lunpr  , itime  , spawnd , noseg  , nowind ,          &
                          iwndtm , wveloa , wdira  , wvelo  , wdir   )

!       Deltares Software Centre

!     Created               : August 1991 by Marcel Zeeuw

!     Modifies              : July   2011 by Leo Postma : cosmetics
!                             January  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     logical unit numbers  : lunpr: standard output report file

!     subroutines called    : none.

!     functions   called    :

      use precision    ! single/double precision
      
      implicit none    ! force explicit typing

!     Arguments

!     kind            function         name                    description

      integer       , intent(in   ) :: lunpr                 !< unit nr output file
      integer       , intent(in   ) :: itime                 !< actual time
      logical       , intent(in   ) :: spawnd                !< if true space varying wind
      integer       , intent(in   ) :: noseg                 !< size of the array with winds
      integer       , intent(in   ) :: nowind                !< number of time breakpoints
      integer       , intent(in   ) :: iwndtm(nowind)        !< time breakpoint values
      real     ( 4 ), intent(in   ) :: wveloa(nowind)        !< time series of wind velocity
      real     ( 4 ), intent(in   ) :: wdira (nowind)        !< time series of wind direction
      real     ( 8 ), intent(  out) :: wvelo (noseg )        !< wind velocity at this time
      real     ( 8 ), intent(  out) :: wdir  (noseg )        !< wind direction at this time

      return
!
      end subroutine
