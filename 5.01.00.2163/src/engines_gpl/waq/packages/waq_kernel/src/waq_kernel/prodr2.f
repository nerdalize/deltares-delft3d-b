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

      SUBROUTINE PRODR2 (DERIV , NOTOT , NOFLUX, STOCHI, NFLUX1,
     +                   NFLUXP, FLUX  , NOSEG , VOLUME, NDT   ,
     +                   OWNERS, MYPART)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            :
C
C     FUNCTION            :
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     DERIV   REAL     NOTOT,*    OUTPUT  Model derivatives
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
C     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
C     NFLUX1  INTEGER       1     INPUT   first flux to construct deriv
C     NFLUXP  INTEGER       1     INPUT   number of fluxes to construct deriv
C     FLUX    REAL          *     INPUT   fluxes at all segments
C     NOSEG   INTEGER       1     INPUT   number of segments
C     VOLUME  REAL          *     INPUT   Segment volumes
C     NDT     INTEGER       1     INPUT   nuber of timesteps in fractional step
C     OWNERS  INTEGER     NOSEG   INPUT   Ownership array for segments
C     MYPART  INTEGER       1     INPUT   Number of current part/subdomain
C
C     Declaration of arguments
C
      use timers
      INTEGER NOTOT , NOFLUX, NFLUX1, NFLUXP, NOSEG, MYPART
      INTEGER OWNERS(NOSEG)
      REAL    DERIV(NOTOT,NOSEG) , STOCHI(NOTOT,NOFLUX) ,
     +        FLUX(NOFLUX,NOSEG) , VOLUME(NOSEG)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "prodr2", ithandl )
C
C     We construeren nu de DERIV's
C
      FDT = NDT
      DO ISYS = 1,NOTOT
         DO IFLUX = NFLUX1 , NFLUX1 + NFLUXP - 1
            ST = STOCHI(ISYS,IFLUX)
            IF ( ST .NE. 0.0 ) THEN
               FACT = FDT*ST
               IF ( ABS(FACT-1.0) .LT. 1.E-10 ) THEN
                  DO ISEG = 1 , NOSEG
                     IF ( OWNERS(ISEG) .EQ. MYPART )
     +                  DERIV(ISYS,ISEG) = DERIV(ISYS,ISEG) +
     +                                     FLUX(IFLUX,ISEG)*VOLUME(ISEG)
                  ENDDO
               ELSE
                  DO ISEG = 1 , NOSEG
                     IF ( OWNERS(ISEG) .EQ. MYPART )
     +                  DERIV(ISYS,ISEG) = DERIV(ISYS,ISEG) +
     +                                     FLUX(IFLUX,ISEG)*VOLUME(ISEG)*FACT
                  ENDDO
               ENDIF
            ENDIF
         ENDDO
      ENDDO
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
