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

      subroutine veg2dn     ( pmsa   , fl     , ipoint , increm, noseg ,
     +                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     +                        noq3   , noq4   )

      ! function determine nutrient availability for vegetation

      implicit none

      ! arguments          i/o description

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
      real(4) volume      ! i  volume                                        (m3)
      real(4) surf        ! i  surf                                          (m2)
      real(4) hmax        ! i  maxmimum length roots                          (m)
      real(4) nh4         ! i  nh4                                         (g/m3)
      real(4) aap         ! i  aap                                         (g/m3)
      real(4) so4         ! i  so4                                         (g/m3)
      real(4) no3         ! i  no3                                         (g/m3)
      real(4) po4         ! i  po4                                         (g/m3)
      real(4) sud         ! i  sud                                         (g/m3)
      real(4) vbxxnavail  ! o  available nitrogen                          (g/m2)
      real(4) vbxxpavail  ! o  available p                                 (g/m2)
      real(4) vbxxsavail  ! o  available s                                 (g/m2)

      ! local declarations

      integer iseg        !    local loop counter for computational element loop
      real(4) z2          !    height bottom segment from bottom              (m)
      real(4) z1          !    height top segment from bottom                 (m)
      integer ikmrk1
      integer ikmrk2
      real(4) zm          !    watersurface to top macropyte                  (-)
      real(4) frlay       !    fraction witin layer                           (-)
      integer iq          !    loop counter
      integer ifrom       !    from segment
      integer ito         !    from segment
      integer iflux       !    index in the fl array

      integer, parameter           :: npnt = 15           ! number of pointers
      integer                      :: ipnt(npnt)          ! local work array for the pointering
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

      ! zero the pool for all segments

      ipnt  = ipoint(1:npnt)
      do iseg = 1 , noseg
         pmsa(ipnt(13)) = 0.0
         pmsa(ipnt(14)) = 0.0
         pmsa(ipnt(15)) = 0.0
         ipnt  = ipnt  + increm(1:npnt)
      enddo

      ! accumulate mass in the rooting zone in the pool of the bottom segment

      ipnt  = ipoint(1:npnt)
      do iseg = 1 , noseg

         depth       = pmsa(ipnt(1))
         totaldepth  = pmsa(ipnt(2))
         localdepth  = pmsa(ipnt(3))
         volume      = pmsa(ipnt(4))
         surf        = pmsa(ipnt(5))
         hmax        = pmsa(ipnt(6))
         nh4         = pmsa(ipnt(7))
         no3         = pmsa(ipnt(8))
         aap         = pmsa(ipnt(9))
         po4         = pmsa(ipnt(10))
         so4         = pmsa(ipnt(11))
         sud         = pmsa(ipnt(12))


         ibotseg     = botseg(iseg)

         call dhkmrk(1,iknmrk(iseg),ikmrk1)
         if (ikmrk1.eq.1) then

            ! active water segment

            if ( hmax .gt. 0.0 ) then

               ! in, partly in or out of the active zone

               hmax = min(hmax,totaldepth)
               zm = totaldepth - hmax
               z1 = localdepth - depth
               z2 = localdepth

               if (zm .gt. z2) then
                  ! not in segment:

               elseif (zm . lt. z1 ) then
                  ! partialy in segment:
                  frlay = (z2-zm)/depth
                  pmsa(ipoint(13)+(ibotseg-1)*increm(13)) = pmsa(ipoint(13)+(ibotseg-1)*increm(13)) + (nh4+no3)*volume*frlay
                  pmsa(ipoint(14)+(ibotseg-1)*increm(14)) = pmsa(ipoint(14)+(ibotseg-1)*increm(14)) + (aap+po4)*volume*frlay
                  pmsa(ipoint(15)+(ibotseg-1)*increm(15)) = pmsa(ipoint(15)+(ibotseg-1)*increm(15)) + (so4+sud)*volume*frlay
               else
                  ! completely in segment:
                  pmsa(ipoint(13)+(ibotseg-1)*increm(13)) = pmsa(ipoint(13)+(ibotseg-1)*increm(13)) + (nh4+no3)*volume
                  pmsa(ipoint(14)+(ibotseg-1)*increm(14)) = pmsa(ipoint(14)+(ibotseg-1)*increm(14)) + (aap+po4)*volume
                  pmsa(ipoint(15)+(ibotseg-1)*increm(15)) = pmsa(ipoint(15)+(ibotseg-1)*increm(15)) + (so4+sud)*volume
               endif

            endif

         elseif (ikmrk1.eq.2) then

            ! delwaq-g segment

            if ( hmax .lt. 0.0 ) then

               ! distribution over the bottom segments

               hmax = -hmax
               hmax = min(hmax,totaldepth)
               z1 = localdepth - depth

               if (hmax .gt. localdepth) then
                  ! completely in segment:
                  pmsa(ipoint(13)+(ibotseg-1)*increm(13)) = pmsa(ipoint(13)+(ibotseg-1)*increm(13)) + (nh4+no3)*volume
                  pmsa(ipoint(14)+(ibotseg-1)*increm(14)) = pmsa(ipoint(14)+(ibotseg-1)*increm(14)) + (aap+po4)*volume
                  pmsa(ipoint(15)+(ibotseg-1)*increm(15)) = pmsa(ipoint(15)+(ibotseg-1)*increm(15)) + (so4+sud)*volume
               elseif (hmax .gt. z1 ) then
                  ! partialy in segment:
                  frlay = (hmax-z1)/depth
                  pmsa(ipoint(13)+(ibotseg-1)*increm(13)) = pmsa(ipoint(13)+(ibotseg-1)*increm(13)) + (nh4+no3)*volume*frlay
                  pmsa(ipoint(14)+(ibotseg-1)*increm(14)) = pmsa(ipoint(14)+(ibotseg-1)*increm(14)) + (aap+po4)*volume*frlay
                  pmsa(ipoint(15)+(ibotseg-1)*increm(15)) = pmsa(ipoint(15)+(ibotseg-1)*increm(15)) + (so4+sud)*volume*frlay
               else
                  ! not in segment:
               endif

            endif

         endif

         ipnt  = ipnt  + increm(1:npnt)

      enddo

      ! express the availeble pool as g/m2

      ipnt  = ipoint(1:npnt)
      do iseg = 1 , noseg
         ibotseg     = botseg(iseg)
         if ( ibotseg .eq. iseg ) then
            surf           = pmsa(ipnt(5))
            pmsa(ipnt(13)) = pmsa(ipnt(13))/surf
            pmsa(ipnt(14)) = pmsa(ipnt(14))/surf
            pmsa(ipnt(15)) = pmsa(ipnt(15))/surf
         endif
         ipnt  = ipnt  + increm(1:npnt)
      enddo

      return
      end
