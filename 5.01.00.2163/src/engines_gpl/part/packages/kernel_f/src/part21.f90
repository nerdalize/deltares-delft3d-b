module part21_mod
!
!  module declarations
!
!  data definition module(s)
!
use precision           ! single/double precision
!
!  module procedure(s)
!
use typos
!
implicit none           ! force explicit typing
!
contains
      subroutine part21 ( lun2   , lgrid  , lgrid2 , xb     , yb     , &
                          area   , volume , nmax   , mmax   , nolay  , &
                          nosubs , nopart , npart  , mpart  , kpart  , &
                          xpart  , ypart  , zpart  , wpart  , npwndw , &
                          pg     , amap   , xa     , ya     , za     , &
                          atotal , apeak  , adepth , imap   , nplay  , &
                          wsettl , irfac  , anfac  , lsettl , locdep , &
                          tcktot , dps   )
!
!
!     CALCULATES (CONC. DEPENDENT) SETTING VELOCITIES
!                     (per time step)
!
!     created       : december 2000, by l. postma
!
!     modified      : june 2001 : by antoon koster
!                     settling out (set w=0 in bottom layer) only
!                     if sed-erosion process active
!                     January  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!     note          : just the grid is produced, no file is written
!                     no checking on timings is conducted
!
!     logical unit numbers  : lun2 - output log file
!
!
!     declarations
!
      type(PlotGrid)                   pg    !< plot grid information
      logical :: lsettl
!
!     integer arrays
!
      integer(ip),dimension(:)        :: npart , mpart , kpart
      integer(ip),dimension(:)        :: nplay
      integer(ip),dimension(:,:)      :: imap
      integer(ip),dimension(:,:)      :: lgrid , lgrid2
!
!     real arrays
!
      real   (sp),dimension(:)        :: dps
      real   (sp),dimension(:)        :: tcktot
      real   (sp),dimension(:)        :: volume
      real   (sp),dimension(:)        :: wsettl
      real   (sp),dimension(:)        :: xa    , ya    , za
      real   (sp),dimension(:)        :: xb    , yb    , area
      real   (sp),dimension(:)        :: xpart , ypart , zpart
      real   (sp),dimension(:,:)      :: adepth, apeak
      real   (sp),dimension(:,:)      :: atotal
      real   (sp),dimension(:,:)      :: locdep
      real   (sp),dimension(:,:)      :: wpart
      real   (sp),dimension(:,:,:,:)  :: amap
!
!     local scalars
!
      integer(ip) ::  lun2
      integer(ip) ::  irfac , mmax
      integer(ip) ::  npwndw , nmax   , nolay  , nopart , nosubs
!
      real   (sp) ::  anfac
      return

      end subroutine
end module
