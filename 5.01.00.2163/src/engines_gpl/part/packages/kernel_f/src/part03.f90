      subroutine part03 ( lgrid  , volume , flow   , dx     , dy     ,   &
                          nmax   , mmax   , mnmaxk , lgrid2 , velo   ,   &
                          layt   , area   , depth  , dps    , locdep ,   &
                          zlevel , tcktot , ltrack)
!
!
!                   Deltares (former: Deltares)
!
!                        d e l p a r    v3.10
!
!
!     system administration : r.j. vos
!
!
!     created               : july 1991, by l.postma
!
!     modified              : january 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!     function              : computes velocities
!
!
!     note                  : none.
!
!
!     logical unit numbers  : none.
!
!
!     functions   called    : none.
!
!     i0      integer     1       local   grid help variable
!     i1      integer     1       local   grid help variable
!     i2      integer     1       local   grid help variable
!     i3      integer     1       local   grid help variable
!     i4      integer     1       local   grid help variable
!     sum     real        1       local   summed help variable
!     vx      real        1       local   velocity in x direction
!     vy      real        1       local   velocity in y direction
!
      use precision    ! single/double precision

      implicit none    ! force explicit typing

!     parameters

!     kind         function         name                     Descriptipon

      integer(ip), intent(in   ) :: nmax                   !< first dimension lgrid
      integer(ip), intent(in   ) :: mmax                   !< second dimension lgrid
      integer(ip), intent(in   ) :: mnmaxk                 !< total size of 3D matrix
      integer(ip), intent(in   ) :: layt                   !< number of layers
      integer(ip), intent(in   ) :: lgrid (nmax,mmax)      !< active grid indices matrix
      integer(ip), intent(in   ) :: lgrid2(nmax,mmax)      !< total grid indices matrix
      real   (rp), intent(in   ) :: dx    (nmax*mmax)      !< x distance of grid cell
      real   (rp), intent(in   ) :: dy    (nmax*mmax)      !< y distance of grid cell
      real   (rp), intent(in   ) :: volume(mnmaxk)         !< volumes
      real   (rp), intent(in   ) :: flow  (*)              !< flows
      real   (rp), intent(in   ) :: area  (nmax*mmax)      !< horizontal surface area
      real   (rp), intent(  out) :: depth (nmax*mmax)      !< water depth
      real   (rp), intent(  out) :: velo  (mnmaxk)         !< velocities in 3D
      real   (rp), intent(in   ) :: dps   (nmax*mmax)      !< bed depth
      real   (rp), intent(  out) :: locdep(nmax*mmax,layt) !< depth per layer
      real   (rp), intent(  out) :: zlevel(nmax*mmax)
      real   (rp), intent(in   ) :: tcktot(layt)
      logical    , intent(in   ) :: ltrack

      return
!
      end subroutine
