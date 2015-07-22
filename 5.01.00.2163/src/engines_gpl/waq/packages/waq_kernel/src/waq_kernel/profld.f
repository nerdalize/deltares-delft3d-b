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

      SUBROUTINE PROFLD (NOFLUX, NFLUX1, NFLUXP, IGRID , NOSEG2,
     +                   NOSEG , NDT   , ISDMP , GRDSEG, FLUX  ,
     +                   VOLUME, FLXDMP)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : Oct 1998 by Jan van Beek
C
C     FUNCTION            : make FLXDMP array from FLUX array
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          : 12
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes (total)
C     NFLUX1  INTEGER       1     INPUT   first flux to be dumped
C     NFLUXP  INTEGER       1     INPUT   number of fluxes to be dumped
C     IGRID   INTEGER       1     INPUT   Grid number for FLUX array
C     NOSEG2  INTEGER       1     INPUT   number of segments in IGRID
C     NOSEG   INTEGER       1     INPUT   number of segments
C     NDT     INTEGER       1     INPUT   timestep multiplier in fractional step
C     ISDMP   INTEGER       *     INPUT   Segment to dumped segment pointer
C     GRDSEG  INTEGER       *     INPUT   Segment to sub-segment pointer for grids
C     FLUX    REAL          *     INPUT   fluxes at all segments
C     VOLUME  REAL          *     INPUT   Segment volumes
C     FLXDMP  REAL    NOFLUX*?    OUTPUT  fluxes at dump segments
C
C     Declaration of arguments
C
      use timers

      INTEGER NOFLUX, NFLUX1, NFLUXP, IGRID , NOSEG2,
     +        NOSEG , NDT
      INTEGER ISDMP(NOSEG)       , GRDSEG(NOSEG,*)
      REAL    FLUX(NOFLUX,NOSEG2), VOLUME(NOSEG)       ,
     +        FLXDMP(NOFLUX,*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "profld", ithandl )
C
C     We construeren nu de FLUXDUMPEN
C
      DO ISEG = 1 , NOSEG
         IF ( ISDMP(ISEG) .GT. 0 ) THEN
            VOL   = VOLUME(ISEG)
            IPS   = ISDMP(ISEG)
            IF ( IGRID .NE. 1 ) THEN
               ISEG2 = GRDSEG(ISEG,IGRID)
            ELSE
               ISEG2 = ISEG
            ENDIF
            DO IFLUX = NFLUX1 , NFLUX1 + NFLUXP - 1
               FLXDMP(IFLUX,IPS) = FLUX(IFLUX,ISEG2)*VOL*NDT
            ENDDO
         ENDIF
      ENDDO
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
