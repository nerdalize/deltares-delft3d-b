module part13_mod
contains
      subroutine part13 ( lun1     , lname    , lun2     , title    , subst    ,   &
                          lgrid2   , nmax     , volume   , area     , npart    ,   &
                          mpart    , xpart    , ypart    , wpart    , nopart   ,   &
                          itime    , idelt    , ipset    , iptmax   , xa       ,   &
                          ya       , xb       , yb       , pg       , recovr   ,   &
                          atotal   , iyear    , imonth   , iofset   , npwndw   ,   &
                          lgrid    , pblay    , modtyp   , apeak    , adepth   ,   &
                          nolay    , nosubs   , rbuffr   , kpart    , itrack   ,   &
                          nplot    , mapsub   , ntrack   , isfile   , mmax     ,   &
                          nfract   , lsettl   , mstick   , elt_names, elt_types,   &
                          elt_dims , elt_bytes, locdep   , zpart    , za       ,   &
                          dps      , tcktot   , nosub_max, bufsize  )

!     CREATING PLO FILE FOR PLOT GRID
!       (Nefis and binary files / per time step)

!     system administration : r.j. vos

!     created               : february 1990, by l. postma

!     modified              : January  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     note                  : delpar sort different from partplot
!                             partplot:  first layers
!                             delpar  :  first substances

!     logical unit numbers  : lun1 - plot grid file
!                             lun2 - output log file

!     subroutines called    : part11 - converts model cooordinates to
!                                      national grid and plot coordinates,
!                             putget
!                             putget_chars

!     functions   called    : findcell

      use precision          ! single and double precision
      use typos

      implicit none          ! force explicit typing

!     Arguments

!     kind           function         name                       description

      integer  ( ip), intent(in   ) :: lun1                    !< unit nr of the map file
      character( * ), intent(in   ) :: lname                   !< name of the plotgrid-file
      integer  ( ip), intent(in   ) :: lun2                    !< unit nr of the log file
      character( 40), intent(in   ) :: title (4)               !< model and run titles
      integer  ( ip), intent(in   ) :: nosubs                  !< actual number of substances
      integer  ( ip), intent(in   ) :: nosub_max               !< maximum number of substances
      character( * ), intent(in   ) :: subst (nosub_max)       !< substance name and unit specs
      integer  ( ip), intent(in   ) :: nmax                    !< first dimension of lgrid2
      integer  ( ip), intent(in   ) :: mmax                    !< sec. dimension of lgrid2
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)       !< model grid layout (total)
      integer  ( ip), intent(in   ) :: nolay                   !< actual number of layers
      real     ( rp), intent(in   ) :: volume(nmax*mmax*nolay) !< volumes of the lgrid2 cells
      real     ( rp), intent(in   ) :: area  (nmax*mmax*nolay) !< horizontal surface areas of the lgrid2 cells
      integer  ( ip), intent(in   ) :: nopart                  !< nr of particles
      integer  ( ip), intent(in   ) :: npart (nopart)          !< n-values of particles
      integer  ( ip), intent(in   ) :: mpart (nopart)          !< m-values of particles
      real     ( rp), intent(in   ) :: xpart (nopart)          !< x-values of particles
      real     ( rp), intent(in   ) :: ypart (nopart)          !< y-values of particles
      real     ( rp), intent(in   ) :: wpart (nosubs,nopart)   !< weights of the particles
      integer  ( ip), intent(in   ) :: itime                   !< simulation time
      integer  ( ip), intent(in   ) :: idelt                   !< simulation time step
      integer  ( ip), intent(in   ) :: iptmax                  !< nr of plot grids
      integer  ( ip), intent(in   ) :: ipset (iptmax)          !< plot grid times
      real     ( rp), intent(  out) :: xa    (nopart)          !< national coordinates of parts
      real     ( rp), intent(  out) :: ya    (nopart)          !< national coordinates of parts
      real     ( rp), intent(in   ) :: xb    (nmax*mmax)       !< x-values of bottom points
      real     ( rp), intent(in   ) :: yb    (nmax*mmax)       !< y-values of bottom points
      type(PlotGrid), intent(in   ) :: pg                      !< plot grid information
      real     ( rp), intent(in   ) :: recovr(iptmax)          !< recovery for the plots
      real     ( rp), intent(  out) :: atotal(nolay,nosubs)    !< total per mass per subst/per layer
      integer  ( ip), intent(in   ) :: iyear                   !< year offset to real time
      integer  ( ip), intent(in   ) :: imonth                  !< month offset to real time
      integer  ( ip), intent(in   ) :: iofset                  !< day offset in seconds to real time
      integer  ( ip), intent(in   ) :: npwndw                  !< start of active nopart number
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)       !< active grid numbers
      real     ( rp), intent(in   ) :: pblay                   !< relative thickness lower layer
      integer  ( ip), intent(in   ) :: modtyp                  !< model type
      real     ( rp), intent(  out) :: apeak (nosubs,nolay)    !< max mass per subst/per layer
      real     ( rp), intent(  out) :: adepth(nosubs,nolay)    !< depth for max mass
      integer  ( ip), intent(in   ) :: bufsize                 !< size of rbuffr
      real     ( rp)                :: rbuffr(bufsize)         !< work storage
      integer  ( ip), intent(in   ) :: kpart (nopart)          !< k-values of particles
      integer  ( ip), intent(in   ) :: itrack                  !< substance number for tracks
      integer  ( ip), intent(in   ) :: ntrack                  !< nr of particles to track
      integer  ( ip), intent(in   ) :: nplot (ntrack)          !< particle nr's for particle tracks
      integer  ( ip), intent(in   ) :: mapsub(nosubs)          !< substances numbers in map
      integer  ( ip), intent(in   ) :: isfile(nosubs)          !< when 1 then from conc array
      integer  ( ip), intent(in   ) :: nfract                  !< number of oil fractions
      logical       , intent(in   ) :: lsettl                  !< if .true. then settling in an extra layer
      integer  ( ip), intent(in   ) :: mstick(nosubs)          !< sticking oil material if < 0 then sticky
      character( * ), pointer       :: elt_names(:)            !<  NEFIS
      character( * ), pointer       :: elt_types(:)            !<  NEFIS
      integer  ( ip), pointer       :: elt_dims (:,:)          !<  NEFIS
      integer  ( ip), pointer       :: elt_bytes(:)            !<  NEFIS
      real     ( rp)                :: locdep(nmax*mmax,nolay)
      real     ( rp), intent(in   ) :: zpart (nopart)          !< z-values of particles
      real     ( rp), intent(  out) :: za    (nopart)          !< national coordinates of parts
      real     ( rp), intent(in   ) :: dps   (nmax*mmax)       !< depth
      real     ( rp), intent(in   ) :: tcktot(nolay+1)         !< layer thickness

      return
!
      end subroutine
end module part13_mod
