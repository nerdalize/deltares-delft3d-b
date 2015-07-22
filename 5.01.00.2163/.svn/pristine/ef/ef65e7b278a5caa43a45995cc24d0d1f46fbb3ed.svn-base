module part16_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision              ! single and double precision
!
!  module procedure(s)
!
implicit none              ! force explicit typing
!
contains
      subroutine part16(lun2  , lgrid , conc  , mnmaxk, npart ,    &
                        mpart , wpart , nopart, itime , iptime,    &
                        npwndw, atotal, itstrt, iddtim, itstop,    &
                        nodye , nocont, idelt , kpart , npwndn,    &
                        nosubs, nolay , mnmax2, isfile, nosubc,    &
                        modtyp                        )
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.12
!
!
!     system administration : r.j. vos
!
!
!     created               : april 1990, by m. zeeuw
!
!
!     modified              : may 1990, by a. markus
!                           : april 1991, by a. markus
!                           : july 1992 by r.j. vos, for 8 substances
!                             with identical transport but different
!                             decay
!                           : july 1996 by r.j. vos, for 3d version
!                             and conc-array like delwaq standard
!                             January  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!     function              : generates a delwaq - loads file
!
!
!     note                  : not tested yet for user-defined releases
!
!
!     logical unit numbers  : lun1 - unit nr delwaq - map-file
!                             lun2 - output log file
!
!
!     subroutines called    : srstop
!
!
!     functions   called    : none.
!
!     dimensioning
!
      integer(ip), dimension(:)        :: iptime
      integer(ip), dimension(:)        :: isfile
      integer(ip), dimension(:)        :: npart , mpart , kpart
      integer(ip), dimension(:,:)      :: lgrid
!
      real   (sp), dimension(:)        :: atotal
      real   (sp), dimension(:,:)      :: conc
      real   (sp), dimension(:,:)      :: wpart
!
!     local scalars
!
      integer(ip) :: iddtim , idelt
      integer(ip) :: itime  , itstop , itstrt , lun2   , mnmax2
      integer(ip) :: mnmaxk , modtyp , nocont , nodye  , nolay
      integer(ip) :: nopart , nosubc , nosubs , npwndn , npwndw

      return

      end subroutine
end module

