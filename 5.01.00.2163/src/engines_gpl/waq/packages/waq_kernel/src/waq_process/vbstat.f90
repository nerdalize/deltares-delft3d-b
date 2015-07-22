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

      subroutine VBSTAT     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 12) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 12) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 12)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
      real(4) DELT        ! I  timestep for processes                             (d)
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) SwEmersion  ! I  switch indicating submersion(0) or emersion(1)     (-)
      real(4) nsfVB     ! I  nr successive emersion(flood) VB01                 (d)
      real(4) CrnsfVB01   ! I  critical number successive flood days VB01         (d)
      real(4) SwNutVB01   ! I  switch indicating nutrient limitation (0=no,1=yes) (-)
      real(4) Initnsf     ! I  initial nr of flood days at start of simulation    (d)
      real(4) nsnlVB01    ! I  number of successive days nutrient lim. VB01       (d)
      real(4) CrnsnlVB01  ! I  critical number of successive nut. lim VB01        (d)
      real(4) SwVB01Gro   ! O  vegetation biomass growth allowed (0=no,1=yes)     (-)
      real(4) SwVB01Mrt   ! O  vegetation biomass dead (0=no,1=yes)               (-)
      integer, save       :: ifirst = 0     !    for initialisation
      integer ilumon
!
!*******************************************************************************
!
      ipnt        = ipoint
!
      CALL GETMLU(ILUMON)

      do 9000 iseg = 1 , noseg
!
         SwEmersion = pmsa( ipnt(  1) )
         nsfVB      = pmsa( ipnt(  2) )
         CrnsfVB01  = pmsa( ipnt(  3) )
         Initnsf    = pmsa( ipnt(  4) )
         SwNutVB01  = pmsa( ipnt(  5) )
         nsnlVB01   = pmsa( ipnt(  6) )
         CrnsnlVB01 = pmsa( ipnt(  7) )
         DELT       = pmsa( ipnt(  8) )
!
!   *****     Insert your code here  *****
!

!        initialise growth
         SWVB01Gro = 1.0
         SwVB01Mrt = 0.0

       if (ifirst .eq. 0) then
          nsfVB = Initnsf
!          WRITE (ILUMON, *) 'ifirst, iseg, nsf', ifirst, iseg, nsfvb
       endif

         if ( NINT(SwEmersion) .eq. 0 ) then
            nsfVB = nsfVB + DELT
            SWVB01Gro = 0.0
         else
            nsfVB = 0
         endif

         if ( NINT(SWNutVB01) .eq. 1 ) then
            nsnlVB01 = nsnlVB01 + DELT
            SWVB01Gro = 0.0
         else
            nsnlVB01 = 0
         endif

         if ( (nsfVB .gt. CrnsfVB01) .or. (nsnlVB01 .gt. CrnsnlVB01) ) then
            SwVB01Mrt = 1.0
         endif

!
!   *****     End of your code       *****
!
         pmsa( ipnt(  9)   ) = SwVB01Gro
         pmsa( ipnt( 10)   ) = SwVB01Mrt
         pmsa( ipnt( 11)   ) = nsfVB
         pmsa( ipnt( 12)   ) = nsnlVB01
!
         ipnt        = ipnt        + increm
!
 9000 continue
      ifirst = 1
!
      return
      end
