module part12_mod
contains
      subroutine part12 ( lun1     , lname    , lun2     , title    , subst    ,    &
                          lgrid    , lgrid2   , lgrid3   , nmax     , mmax     ,    &
                          conc     , volume   , npart    , mpart    , wpart    ,    &
                          nopart   , itime    , idelt    , icwsta   , icwsto   ,    &
                          icwste   , atotal   , npwndw   , kpart    , pblay    ,    &
                          iptime   , npwndn   , modtyp   , nosubs   , nolay    ,    &
                          iyear    , imonth   , iofset   , pg       , rbuffr   ,    &
                          nosta    , mnmax2   , nosegl   , isfile   , mapsub   ,    &
                          layt     , area     , nfract   , lsettl   , mstick   ,    &
                          elt_names, elt_types, elt_dims , elt_bytes, locdep   ,    &
                          nosub_max, bufsize  )

!     CREATING MAP FILE FOR CURVILINEAR GRID
!          (Nefis and binary files / per time step)
!
!     system administration : r.j. vos

!     created               : february 1990, by l. postma

!     modified              : January  2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

!     function              : generates a standard delwaq - map-file,
!                             and concentration-array for partwq
!                             3d version...........
!
!     note                  : include of file 'crefd.inc'
!                             include is no standard (ansi) fortran77!!
!                             check if this include facility is available!
!
!     logical unit numbers  : lun1 - unit nr delwaq - map-file
!                             lun2 - output log file

!     subroutines called    : srstop
!                             putget
!                             putget_chars

!     functions   called    : none.

      use precision          ! single and double precision
      use typos

      implicit none          ! force explicit typing

!     Arguments

!     kind           function         name                      description

      integer  ( ip), intent(in   ) :: lun1                    !< unit nr of the Delwaq .map file
      character( * ), intent(in   ) :: lname                   !< name of the .map file
      integer  ( ip), intent(in   ) :: lun2                    !< unit nr of the output log file
      character( 40), intent(in   ) :: title (4)               !< model- and run titles
      integer  ( ip), intent(in   ) :: nmax                    !< first dimension of the grid
      integer  ( ip), intent(in   ) :: mmax                    !< second dimension of the grid
      integer  ( ip), intent(in   ) :: nolay                   !< number of layers of the grid
      integer  ( ip), intent(in   ) :: nosubs                  !< number of substances to plot
      integer  ( ip), intent(in   ) :: nopart                  !< number of particles
      character( 20), intent(in   ) :: subst (nosubs+1)        !< substance names with layer extension
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)       !< active grid table
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)       !< total grid table
      integer  ( ip), intent(in   ) :: lgrid3(nmax,mmax)       !< plot grid either total or active condensed
      integer  ( ip), intent(in   ) :: nosub_max               !< maximum number of substances
      real     ( rp), intent(  out) :: conc  (nosub_max,nmax*mmax*nolay) !< computed concentrations
      real     ( sp), intent(in   ) :: volume( * )             !< volumes of the grid cells
      integer  ( ip), intent(inout) :: npart ( nopart )        !< n-values of particles
      integer  ( ip), intent(inout) :: mpart ( nopart )        !< m-values of particles
      integer  ( ip), intent(inout) :: kpart ( nopart )        !< k-values of particles
      real     ( sp), intent(inout) :: wpart (nosubs,nopart)   !< weights of particles
      integer  ( ip), intent(in   ) :: itime                   !< model time
      integer  ( ip), intent(in   ) :: idelt                   !< model time step
      integer  ( ip), intent(in   ) :: icwsta                  !< start time map-file
      integer  ( ip), intent(in   ) :: icwsto                  !< stop  time map-file
      integer  ( ip), intent(in   ) :: icwste                  !< time step map-file
      real     ( rp), intent(  out) :: atotal(nolay,nosubs)    !< total mass per subst/per layer
      integer  ( ip), intent(inout) :: npwndw                  !< start of active particle number
      real     ( sp), intent(in   ) :: pblay                   !< relative thickness lower layer
      integer  ( ip), intent(inout) :: iptime( nopart )        !< age of particles
      integer  ( ip), intent(in   ) :: npwndn                  !< new start of active particle number - 1
      integer  ( ip), intent(in   ) :: modtyp                  !< model-run-type
      integer  ( ip), intent(in   ) :: iyear                   !< year
      integer  ( ip), intent(in   ) :: imonth                  !< month
      integer  ( ip), intent(in   ) :: iofset                  !< offset in time
      type(PlotGrid)                   pg                      !< first plot grid information
      integer  ( ip), intent(in   ) :: bufsize                 !< size of rbuffr
      real     ( rp)                :: rbuffr(bufsize)         !< work storage
      integer  ( ip), intent(in   ) :: nosta                   !< number of observation points
      integer  ( ip), intent(in   ) :: mnmax2                  !< number of grid cells in one grid layer
      integer  ( ip), intent(in   ) :: nosegl                  !< number of computational elements per layer
      integer  ( ip), intent(in   ) :: isfile(nosub_max)       !< file output for the substance?
      integer  ( ip), intent(in   ) :: mapsub(nosub_max)
      integer  ( ip), intent(in   ) :: layt                    !< number of hydrodynamic layers
      real     ( sp), intent(in   ) :: area  (mnmax2)
      integer  ( ip), intent(in   ) :: nfract                  !< number of oil fractions
      logical       , intent(in   ) :: lsettl                  !< if .true. settling occurs in an extra layer
      integer  ( ip), intent(in   ) :: mstick(nosub_max)
      character( * ), pointer       :: elt_names(:)            !<  NEFIS
      character( * ), pointer       :: elt_types(:)            !<  NEFIS
      integer  ( ip), pointer       :: elt_dims (:,:)          !<  NEFIS
      integer  ( ip), pointer       :: elt_bytes(:)            !<  NEFIS
      real     ( rp)                :: locdep (nmax*mmax,nolay)

      return
!
      end subroutine
end module part12_mod
