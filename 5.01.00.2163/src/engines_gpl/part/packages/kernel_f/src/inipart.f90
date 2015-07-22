      subroutine inipart( lgrid   , lgrid2  , nmax    , mmax    , xcor    ,     &
                          ycor    , nopart  , nosubs  , subst   , ini_file,     &
                          xpol    , ypol    , npol    , wpart   , xpart   ,     &
                          ypart   , zpart   , npart   , mpart   , kpart   ,     &
                          iptime  , npmax   , nrowsmax, lunpr   )

!
!     programmer : antoon koster
!     function   : set up of initial condition for oil patches
!     date       : may 2004
!     modified   : jan 2013 by Michel Jeuken : created dummy 'part'-subroutine for 'waq' open source release
!
!
!     method     : for each of the oil patches defined in the part ini-file,
!                  their total mass will be spread out uniformly over the
!                  circumscribing polygone.
!
!                  for each of these polygone the
!                       - oil fraction
!                       - total mass
!                       - number of particles to be spread
!                  must be provided in the header of the ini-file (tekal)
!
!                  spreading occurs by random sampling of the polygone
!                  area by random pairs (x,y) for the given number of
!                  particles.
!
!                  finally all spread particles will be assigned the
!                  average mass (total mass/no. of particles)
!
!                  the oil patch is supposed to be floating oil, so located
!                  at z=0.0 of the top layer (k=1)
!
      use precision ! single/double precision

      implicit none ! force explicit typing

!     Arguments

!     kind           function         name                      description

      integer  ( ip), intent(in   ) :: nmax                    !< first dimension matrix
      integer  ( ip), intent(in   ) :: mmax                    !< second dimension matrix
      integer  ( ip), intent(in   ) :: npmax                   !< maximum number of particles
      integer  ( ip), intent(inout) :: nopart                  !< number of active particles
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)       !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)       !< total grid matrix
      real     ( rp), intent(in   ) :: xcor  (nmax*mmax)
      real     ( rp), intent(in   ) :: ycor  (nmax*mmax)
      integer  ( ip), intent(inout) :: nosubs                  !< number of substances
      character( * ), intent(in   ) :: subst (*)               !< substance names
      character( * ), intent(in   ) :: ini_file                !< polygon file
      integer  ( ip), intent(in   ) :: npol                    !< number of substances
      integer  ( ip), intent(in   ) :: nrowsmax                !< dimension of poligons
      real     ( rp), intent(  out) :: xpol  (nrowsmax)        !< xvalues polygons
      real     ( rp), intent(  out) :: ypol  (nrowsmax)        !< yvalues polygons
      real     ( rp), intent(  out) :: wpart (nosubs,npmax)    !< weight of the particles
      real     ( rp), intent(  out) :: xpart (npmax)           !< x of theparticles
      real     ( rp), intent(  out) :: ypart (npmax)           !< y of the particles
      real     ( rp), intent(  out) :: zpart (npmax)           !< z of the particles
      integer  ( ip), intent(  out) :: npart (npmax)           !< n of the particles
      integer  ( ip), intent(  out) :: mpart (npmax)           !< m of the particles
      integer  ( ip), intent(  out) :: kpart (npmax)           !< k of the particles
      integer  ( ip), intent(  out) :: iptime(npmax)           !< time in the system
      integer  ( ip), intent(in   ) :: lunpr                   !< unit nr of the diagnostics file

      return

      end subroutine inipart
