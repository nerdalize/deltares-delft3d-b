module partur_mod
!
!  module declarations
!
!  data definition module(s)
!
use precision               ! single/double precision
!
implicit none               ! force explicit typing
!
contains
      subroutine partur ( itime  , noudef , iutime  , mpart   , npart  , &
                          kpart  , xpart  , ypart   , zpart   , wpart  , &
                          iptime , nopart , lgrid   , modtyp  , nmax   , &
                          mmax   , amasud , ipnt    , sname   , nosubs , &
                          nolay  , nocont , ndprt   , npmax   , const  , &
                          nodye  , lun    , mapsub  , buffer  , volume , &
                          aconud , uscal  , isub    , finam   , iftime , &
                          ifopt  , nosyss  , isfil   , nosubud , snamud   )
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.30
!
!
!     system administration : r.j. vos
!
!
!     created               : july 1996, by r.j. vos
!                             for 3d.....red tide model
!
!     modified              : jan  1997, by r.j. vos  (version 3.23)
!                             general use for all modtyp's
!                             no splitting and reading of constants when modtyp lt 3
!                             use cscag and q0star = 1 in this case
!                             array isub is used to indicate substances
!                             january 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!     function              : adds user defined release to bunch of particles.
!                             check splitting of particles on isub(2) for red tide model
!                             iredtq = isub(2)
!
!     note                  : none.
!
!
!
!     dimensioning
!
      integer(ip),dimension(:)    :: iftime, ifopt , nosyss, ipnt
      integer(ip),dimension(:)    :: iptime , kpart
      integer(ip),dimension(:)    :: iutime
      integer(ip),dimension(:)    :: mapsub
      integer(ip),dimension(:)    :: ndprt
      integer(ip),dimension(:)    :: npart , mpart
      integer(ip),dimension(:,:)  :: lgrid
!
      real   (sp),dimension(:)    :: buffer, volume, uscal
      real   (sp),dimension(:)    :: const
      real   (sp),dimension(:)    :: xpart , ypart , zpart
      real   (sp),dimension(:,:)  :: aconud
      real   (sp),dimension(:,:)  :: amasud
      real   (sp),dimension(:,:)  :: wpart
!
      integer(ip),dimension(:)    :: isub  , isfil
!
      character(len=256), dimension(:)  :: finam
      character(len= 20), dimension(:)  :: sname
      character(len= 20), dimension(:)  :: snamud
!
!     local scalars
!
      integer(ip) ::  nmax
      integer(ip) ::  itime
      integer(ip) ::  lun    , mmax    , modtyp
      integer(ip) ::  nocont , nodye , nolay   , nopart
      integer(ip) ::  noudef
      integer(ip) ::  nosubs , nosubud, npmax
      return

      end subroutine
end module

