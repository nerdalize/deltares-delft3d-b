      subroutine rdlgri ( nfiles , lunit  , fname  , ftype   )

!     Deltares Software Centre

!     system administration : Antoon Koster

!     created               : February 1990 by Leo Postma

!     modified              : June     2011 by Leo Postma: pointers from noseg and noq added
!                                                          to support active only hydrodynamic files
!                             October  2011 by Leo Postma: support Domain Decomposition
!                             January  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     logical unit numbers  : lunit( 1), the delpar input file
!                             lunit( 2), output report file
!                             lunit( 3), active grid table
!                             lunit( 4), total grid table
!                             lunit(19), from-to pointer table

!     subroutines called    : srstop   - ends the simulation with return code

!     functions   called    : none.

      use precision               ! single and double precision

      implicit none               ! force explicit typing

!     Arguments

!     kind           function         name                 description

      integer  ( ip), intent(in   ) :: nfiles            !< nr. of files
      integer  ( ip), intent(inout) :: lunit(nfiles)     !< unit nrs of all files
      character(256), intent(inout) :: fname(nfiles)     !< file names of all files
      character( 20), intent(inout) :: ftype(2)          !< 'binary'

      return

      end subroutine
