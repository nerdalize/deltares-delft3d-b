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

      SUBROUTINE FLXBAL (NOTOT , NOFLUX, NDMPAR, NOBALT, STOCHI,
     +                   FLXINT, ASMASS, BALINT)
C
C     Deltares
C
C     CREATED:            : march 1998 by Jan van Beek
C
C     FUNCTION            : Makes BALINT from FLXINT and STOCHI
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          : 8
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
C     NDMPAR  INTEGER       1     INPUT   Nr. of dump areas
C     NOBALT  INTEGER       1     INPUT   Nr. of balance terms total
C     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
C     FLXINT  REAL  NOFLUX*NDMPAR INPUT   Accumulated fluxes
C     ASMASS  REAL NOTOT*NDMPAR*6 INPUT   Mass balance terms
C     BALINT  REAL  NOBALT*NDMPAR OUTPUT  Balance terms
C
C     Declaration of arguments
C
      use timers

      INTEGER NOTOT , NOFLUX, NDMPAR, NOBALT
      REAL    STOCHI(NOTOT,NOFLUX)  , FLXINT(NOFLUX,NDMPAR),
     +        ASMASS(NOTOT,NDMPAR,6), BALINT(NOBALT,NDMPAR)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "flxbal", ithandl )
C
C     We construeren nu de BALINT's
C
      IBALT = 0
      DO ISYS = 1,NOTOT
         DO I = 1 , 4
            IBALT = IBALT + 1
            IF ( I .EQ. 1 .OR. I .EQ. 3 ) THEN
               DO IDMP = 1 , NDMPAR
                  BALINT(IBALT,IDMP) = ASMASS(ISYS,IDMP,I+2)
               ENDDO
            ELSE
               DO IDMP = 1 , NDMPAR
                  BALINT(IBALT,IDMP) = -ASMASS(ISYS,IDMP,I+2)
               ENDDO
            ENDIF
         ENDDO
         DO IFLX = 1 , NOFLUX
            ST = STOCHI(ISYS,IFLX)
            IF ( ABS(ST) .GT. 1.E-20 ) THEN
               IBALT = IBALT + 1
               IF ( IBALT .GT. NOBALT ) THEN
                  CALL GETMLU(LUREP)
                  WRITE(LUREP,*) 'ERROR, INTERNAL FLXBAL'
                  WRITE(*,*)     'ERROR, INTERNAL FLXBAL'
                  CALL SRSTOP(1)
               ENDIF
               DO IDMP = 1 , NDMPAR
                  BALINT(IBALT,IDMP) = FLXINT(IFLX,IDMP)*ST
               ENDDO
            ENDIF
         ENDDO
      ENDDO
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
