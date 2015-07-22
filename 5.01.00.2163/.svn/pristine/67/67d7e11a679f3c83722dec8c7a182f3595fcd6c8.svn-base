module oildsp_mod

   contains
        subroutine oildsp ( lgrid   , nmax    , conc    , volume  , area    ,    &
                            npart   , mpart   , wpart   , radius  , nodye   ,    &
                            npwndw  , nopart  , idelt   , wvelo   , const   ,    &
                            lun2    , nosubs  , nolay   , lgrid2  , mmax    ,    &
                            xb      , yb      , kpart   , mapsub  , isfile  ,    &
                            nfract  , mstick  , nstick  , fstick  , xa      ,    &
                            ya      , pg      , lsettl  , xpart   , ypart   ,    &
                            zpart   , za      , locdep  , dps     , tcktot  ,    &
                            substi  , hmin    , npmax   , rhow    , amassd  ,    &
                            ioptrad )

!       Deltares Software Centre
!     System administration : Antoon Koster

!     Created               : 27 November  1997 by Robert Vos

!     modified              : jan 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release

      use precision         ! single/double precision
      use typos             ! the derived types

      implicit none

      !     Arguments

!     kind            function         name                    description

      integer  ( ip), intent(in   ) :: idelt                 !< computational time step size
      integer  ( ip), intent(in   ) :: lun2                  !< unit number output report file
      integer  ( ip), intent(in   ) :: nfract                !< number of oil fractions
      integer  ( ip), intent(in   ) :: npmax                 !< total number of particles
      integer  ( ip), intent(in   ) :: npwndw                !< first active particle in the array
      integer  ( ip), intent(in   ) :: nopart                !< current maximum of active particles
      integer  ( ip), intent(in   ) :: nodye                 !< number of dye releases
      integer  ( ip), intent(in   ) :: nmax                  !< first dimension of the grid
      integer  ( ip), intent(in   ) :: mmax                  !< second dimension of the grid
      integer  ( ip), intent(in   ) :: nolay                 !< number of layers     (may be more is lsettl)
      integer  ( ip), intent(in   ) :: nosubs                !< number of substances (may be more than 3*fract)
      integer  ( ip), intent(in   ) :: nstick                !< number of sticking substances
      logical       , intent(in   ) :: lsettl                !< if true, settling and an additional layer exists
      real     ( rp), pointer       :: const  (:)            !< constants as read from the input file
      real     ( rp), intent(  out) :: fstick (nfract)       !< sticking probability of a fraction
      real     ( rp), intent(in   ) :: rhow                  !< density of water
      real     ( rp), intent(in   ) :: hmin                  !< hmin=0.0005 m based on adios (see kleissen 2003)
      real     ( rp), intent(  out) :: radius (nodye)        !< computed radius of dye releases of oil
      real     ( dp), intent(in   ) :: wvelo  (*)            !< wind velocity
      integer  ( ip), pointer       :: lgrid (:,:)           !< active grid layout of the area
      integer  ( ip), pointer       :: lgrid2(:,:)           !< total grid layout of the area
      real     ( rp), pointer       :: xb     (:)            !< x-values of the corners of the gridcells
      real     ( rp), pointer       :: yb     (:)            !< y-values of the corners of the gridclls
      integer  ( ip), pointer       :: npart  (:)            !< 1st cell index of each particle
      integer  ( ip), pointer       :: mpart  (:)            !< 2nd cell index of each particle
      integer  ( ip), pointer       :: kpart  (:)            !< 3rd cell index of each particle
      real     ( rp), pointer       :: xpart  (:)            !< normalized x-value within the cell of the particles
      real     ( rp), pointer       :: ypart  (:)            !< normalized y-value within the cell of the particles
      real     ( rp), pointer       :: zpart  (:)            !< normalized z-value within the cell of the particles
      real     ( rp), pointer       :: xa     (:)            !< x-world coordinate of the particles
      real     ( rp), pointer       :: ya     (:)            !< y-world coordinate of the particles
      type(PlotGrid)                :: pg                    !< plot grid information
      real     ( rp), pointer       :: za     (:)            !< z-world coordinate of the particles
      real     ( rp), pointer       :: locdep(:,:)           !< local depths of the gridcells
      real     ( rp), pointer       :: dps    (:)            !< depth of the reference plain in the grid cells
      real     ( rp), pointer       :: tcktot (:)            !< relative layer thickness of the layers
      real     ( rp), pointer       :: conc  (:,:)           !< concentrations on hydrodynamic grid
      real     ( rp), pointer       :: volume (:)            !< volume of the computational cells
      real     ( rp), pointer       :: area   (:)            !< horizontal surface area of the grid
      integer  ( ip), intent(in   ) :: ioptrad(nodye)        !< if 1 use fay-holt formula for the radius
      character( 20), intent(in   ) :: substi (nosubs)       !< names of the substances
      integer  ( ip), intent(in   ) :: isfile (nosubs)       !< when 1 conc. follows from file in user routine
      integer  ( ip), intent(in   ) :: mapsub (nosubs)       !< index array substances in map file
      integer  ( ip), intent(in   ) :: mstick (nosubs)       !< sticking index substances
      real     ( rp), pointer       :: amassd  (:,:)         !< mass of substances per dye releases
      real     ( rp), pointer       :: wpart   (:,:)         !< weight of the particles per substance

      return

      end subroutine
end module
