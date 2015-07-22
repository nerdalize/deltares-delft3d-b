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

      SUBROUTINE DHSWTR ( ISWITR , NOQ3   , IOK    )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : december 1994 by Jan van Beek
C
C     FUNCTION            : utility that evaluates the TRswitch
C                           for the target model dimension
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ISWITR  INTEGER       1     INPUT   Target dimension indicator
C     NOQ3    INTEGER       1     INPUT   Number of exchanges in 3 dir.
C     IOK     LOGICAL       1     OUTPUT  dimension match indicator
C
      INTEGER ISWITR, NOQ3
      LOGICAL IOK
C
C     Local
C
      IOK = .TRUE.
      IF ( ISWITR .EQ. 3 .AND. NOQ3   .EQ. 0 ) THEN
         IOK = .FALSE.
      ENDIF
      IF ( ISWITR .EQ. 12 .AND. NOQ3   .GT. 0 ) THEN
         IOK = .FALSE.
      ENDIF
C
      RETURN
      END
