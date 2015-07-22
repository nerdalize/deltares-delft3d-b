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

      SUBROUTINE DHKMRK ( IKNMRK , KENMRK , KNMRKI )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: june  1994 by Jan van Beek
C
C     FUNCTION            : utility that evaluates a feature from
C                           the "feature" integer
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     IKNMRK  INTEGER     1       INPUT   Index of feature
C     KENMRK  INTEGER     1       INPUT   feature
C     KNMRKI  INTEGER     1       OUTPUT  evaluated feature
C
      INTEGER IKNMRK, KENMRK, KNMRKI
C
C     Local
C
      INTEGER DHIMIS
C
      IF ( IKNMRK .EQ. 1 ) THEN
         KNMRKI = MOD(KENMRK,10)
      ELSEIF ( IKNMRK .EQ. 2 ) THEN
         KNMRKI = KENMRK / 10
         KNMRKI = MOD(KNMRKI,10)
      ELSEIF ( IKNMRK .EQ. 3 ) THEN
         KNMRKI = KENMRK / 100
         KNMRKI = MOD(KNMRKI,10)
      ELSEIF ( IKNMRK .LE. 0 .OR.
     +     IKNMRK .GT. 9      ) THEN
         DHIMIS = -999.
         KNMRKI = DHIMIS
      ELSE
         KNMRKI = KENMRK / 10**(IKNMRK-1)
         KNMRKI = MOD(KNMRKI,10)
      ENDIF
C
      RETURN
      END
