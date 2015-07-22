module parths_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use precision            ! single and double precision
use typos
!
implicit none            ! force explicit typing
!
contains
      subroutine parths(lun1     , lun2     , title    , subst    , mmax     ,  &
                        lgrid2   , nmax     , volume   , area     , npart    ,  &
                        mpart    , xpart    , ypart    , wpart    , nopart   ,  &
                        itime    , idelt    , xa       , npwndw   , lgrid    ,  &
                        ya       , xb       , yb       , pg       , pblay    ,  &
                        modtyp   , nolay    , nosubs   , conc     , chismp   ,  &
                        chispl   , nosta    , nmstat   , xstat    , ystat    ,  &
                        nstat    , mstat    , nplsta   , mplsta   , ihstrt   ,  &
                        ihstop   , ihstep   , ihplot   , finam    , kpart    ,  &
                        mnmax2   , nfract   , lsettl   , mstick   , elt_names,  &
                        elt_types, elt_dims , elt_bytes, rbuffr   , zpart    ,  &
                        za       , locdep   , dps      , tcktot   , lgrid3   )
!
!     WRITING HISTORY FILE (*.his)
!            (per time step)
!
!     system administration : r.j. vos
!
!     created               : january  1993, by r.j. vos
!
!     modified              : cleared may 1996, now 3d
!                             20/11/96 restored error in amap (ippl and not ipos)
!                                      amap should be intialized with nosubt
!                             contains openfl
!                             july 1998: for settling substances also
!                                        also correction for floating oil
!                             sept 1998: also for sticking material
!                                        corrected normalization for oil
!                             apr  1998: vs 3.60: version for release of 1 jun
!                             jan  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!
!     declarations
!
      type(PlotGrid)                   pg                     !< first plot grid information
      character(len=*), pointer, dimension(:) :: nmstat
      character(len=*), pointer, dimension(:) :: subst
      character( 40), intent(in   ) :: title (4)              !< model titles
      character(len=256)                      :: finam
      logical                                 :: lsettl
!
!     declare putget help var's
!
!     dimensioning
!
      character(len=16), pointer, dimension(:) ::  elt_names, elt_types
!
      integer(ip), pointer, dimension(:)       :: ihplot
      integer(ip), pointer, dimension(:)       :: nplsta, mplsta
      integer(ip), pointer, dimension(:)       :: nstat , mstat
      integer(ip), pointer, dimension(:,:)     :: elt_dims
      integer(ip), pointer, dimension(:)       :: elt_bytes
      integer(ip), pointer, dimension(:)       :: mstick
      integer(ip), pointer, dimension(:)       :: npart , mpart , kpart
      integer(ip), pointer, dimension(:,:)     :: lgrid , lgrid2, lgrid3
      real   (sp), pointer, dimension(:)       :: xa    , ya    , xb     , yb
      real   (sp), pointer, dimension(:)       :: xpart , ypart , volume , area
      real   (sp), pointer, dimension(:)       :: xstat , ystat
      real   (sp), pointer, dimension(:,:,:)   :: chismp
      real   (sp), pointer, dimension(:,:,:)   :: chispl
      real   (sp), pointer, dimension(:)       :: zpart , za
      real   (sp), pointer, dimension(:,:)     :: conc
      real   (sp), pointer, dimension(:,:)     :: wpart
      real   (sp), pointer, dimension(:,:)     :: locdep
      real   (sp), pointer, dimension(:)       :: tcktot
      real   (sp), pointer, dimension(:)       :: dps
      real   (sp), pointer, dimension(:)       :: rbuffr

!
!     local scalars
!
      integer(ip) :: idelt     ,ihstep    ,ihstop
      integer(ip) :: lun1      ,lun2      ,mmax
      integer(ip) :: mnmax2    ,modtyp    ,nfract    ,ihstrt
      integer(ip) :: nmax      ,nolay     ,nopart
      integer(ip) :: nosubs    ,npwndw    ,itime     ,nosta
      real   (sp) :: pblay
      return
!
      end subroutine
end module
