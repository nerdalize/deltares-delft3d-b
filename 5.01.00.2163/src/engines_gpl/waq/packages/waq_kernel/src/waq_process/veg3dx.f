!!  Copyright(C) Stichting Deltares, 2012.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine veg3dx     ( pmsa   , fl     , ipoint , increm, noseg ,
     +                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     +                        noq3   , noq4   )

      ! function distribute multiple vegetation fluxes over the vertical

      implicit none

!     arguments            i/o description

      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint(*)   ! i  array of pointers in pmsa to get and store the data
      integer increm(*)   ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)

      ! from pmsa array

      real(4) depth       ! i  depth of segment                               (m)
      real(4) totaldepth  ! i  total depth water column                       (m)
      real(4) localdepth  ! i  depth from water surface to bottom of segment  (m)
      real(4) swmacdis    ! i  switch gr. distr.vb   (1)cont. (2)lin. (3)exp. (-)
      real(4) hmax        ! i  maxmimum lenght macrophytes                    (m)
      real(4) ffac        ! i  form factor macropyhyte                        (m)
      integer nvbxx       ! i  number of vb fractions to be distributed       (-)
      real(4) vb          ! i  macrophyte submerged                          (gc)
      real(4) frbmlay     ! o  fraction bm per layer                          (-)
      real(4) bmlayvb     ! o  biomass layer vb                              (gc)

      ! local declarations

      integer iseg        !    local loop counter for computational element loop
      real(4) z2          !    height bottom segment from bottom              (m)
      real(4) z1          !    height top segment from bottom                 (m)
      integer ikmrk1
      integer ikmrk2
      real(4) zm          !    watersurface to top macropyte                  (-)
      real(4) a           !    lineair factor a (ax + b)                      (-)
      real(4) b           !    lineair factor b (ax + b)                      (-)
      integer iq          !    loop counter
      integer ifrom       !    from segment
      integer ito         !    from segment
      integer iflux       !    index in the fl array

      integer, parameter           :: nipfix =  7         ! first number of entries in pmsa independent of number of parameters
      integer, parameter           :: nopfix =  1         ! first output entries in pmsa independent of number of parameters
      integer, parameter           :: nivar  =  1         ! number of variable inputs per nvbxx
      integer, parameter           :: novar  =  1         ! number of variable outputs per nvbxx
      integer                      :: npnt                ! number of pointers
      integer                      :: ivbxx               ! loop counter nvbxx
      integer, allocatable         :: ipnt(:)             ! local work array for the pointering
      logical, save                :: first = .true.      !
      integer                      :: ibotseg             ! bottom segment for macrophyte
      integer, allocatable, save   :: botseg(:)           ! bottom segment for macrophyte

      ! initialise variable indicating bottom segment

      if (first) then

         allocate(botseg(noseg))
         botseg = -1

         ! set botseg equal to iseg for the segments which have a bottom

         do iseg = 1,noseg
            call dhkmrk(3,iknmrk(iseg),ikmrk1)
            if (ikmrk1.eq.1) then
               call dhkmrk(2,iknmrk(iseg),ikmrk2)
               if ((ikmrk2.eq.0).or.(ikmrk2.eq.3)) then
                  botseg(iseg) = iseg
               endif
            endif
         enddo

         ! loop to find bottom segment in water columns

         do iq = noq1+noq2+noq3, noq1 + noq2 +1, -1
            ifrom   = iexpnt(1,iq)
            ito     = iexpnt(2,iq)
            if ( ifrom .gt. 0 .and. ito .gt. 0 ) then
               ibotseg = botseg(ito)
               if ( ibotseg .gt. 0 ) then
                  botseg(ifrom) = ibotseg
               endif
            endif
         enddo

         ! do the same for the delwaq-g bottom

         do iq = noq1+noq2+noq3+1, noq1+noq2+noq3+noq4
            ifrom   = iexpnt(1,iq)
            ito     = iexpnt(2,iq)
            if ( ifrom .gt. 0 .and. ito .gt. 0 ) then
               ibotseg = botseg(ifrom)
               if ( ibotseg .gt. 0 ) then
                  botseg(ito) = ibotseg
               endif
            endif
         enddo

         first = .false.

      endif

      nvbxx = nint(pmsa(ipoint(7)))
      npnt  = nipfix + nivar*nvbxx + nopfix + novar*nvbxx
      allocate(ipnt(npnt))
      ipnt  = ipoint(1:npnt)
      iflux = 0
      do iseg = 1 , noseg

         depth       = pmsa(ipnt(1))
         totaldepth  = pmsa(ipnt(2))
         localdepth  = pmsa(ipnt(3))
         swmacdis    = pmsa(ipnt(4))
         hmax        = pmsa(ipnt(5))
         ffac        = pmsa(ipnt(6))

         ibotseg     = botseg(iseg)

         call dhkmrk(1,iknmrk(iseg),ikmrk1)
         if (ikmrk1.eq.1) then

            ! active water segment

            if ( hmax .gt. 0.0 ) then

               ! distribution over the water segments

               hmax = min(hmax,totaldepth)
               zm = totaldepth - hmax
               z1 = localdepth - depth
               z2 = localdepth

               ! switch = 1:  constant biomass distribution

               if (swmacdis .eq. 1 ) then
                  ffac = 1
               endif

               a = (2. - (2. * ffac)) / (totaldepth - zm) / hmax
               b = (ffac * (zm + totaldepth) - 2. * zm) / (totaldepth - zm) / hmax

               if (zm .gt. z2) then
                  ! macrophyte is not in segment:
                  frbmlay = 0
               elseif (zm . lt. z1 ) then
                  ! macropyhte is partialy in segment:
                  frbmlay = (a/2)  * (z2*z2 - z1*z1) + b * (z2 - z1)
               else
                  ! macropyhte is completely in segment:
                  frbmlay = (a/2)  * (z2*z2 - zm*zm) + b * (z2 - zm)
               endif

            else

               ! distribution over the bottom, no values for water segment

               frbmlay = 0.0

            endif

         elseif (ikmrk1.eq.2) then

            ! delwaq-g segment

            if ( hmax .lt. 0.0 ) then

               ! distribution over the bottom segments

               hmax = -hmax
               hmax = min(hmax,totaldepth)
               zm = totaldepth - hmax
               z1 = totaldepth - localdepth
               z2 = z1 + depth

               ! switch = 1:  constant biomass distribution

               if (swmacdis .eq. 1 ) then
                  ffac = 1
               endif

               a = (2. - (2. * ffac)) / (totaldepth - zm) / hmax
               b = (ffac * (zm + totaldepth) - 2. * zm) / (totaldepth - zm) / hmax

               if (zm .gt. z2) then
                  ! macrophyte is not in segment:
                  frbmlay = 0
               elseif (zm . lt. z1 ) then
                  ! macropyhte is partialy in segment:
                  frbmlay = (a/2)  * (z2*z2 - z1*z1) + b * (z2 - z1)
               else
                  ! macropyhte is completely in segment:
                  frbmlay = (a/2)  * (z2*z2 - zm*zm) + b * (z2 - zm)
               endif

            else

               ! distribution over the water column, no values for bottom segment

               frbmlay = 0.0

            endif
         else

            ! inactive segment

            frbmlay = 0.0

         endif

         pmsa(ipnt(nipfix+nivar*nvbxx+1)) = frbmlay
         do ivbxx = 1, nvbxx
            if (ikmrk1.ne.0) then
               vb      = pmsa(ipoint(nipfix+ivbxx)+(ibotseg-1)*increm(nipfix+ivbxx))
               bmlayvb = frbmlay * vb
            else
               bmlayvb = 0.0
            endif
            pmsa(ipnt(nipfix+nivar*nvbxx+1+ivbxx)) = bmlayvb
            fl(ivbxx+iflux) =  bmlayvb/depth
         enddo

         ipnt  = ipnt  + increm(1:npnt)
         iflux = iflux + noflux

      enddo

      return
      end
