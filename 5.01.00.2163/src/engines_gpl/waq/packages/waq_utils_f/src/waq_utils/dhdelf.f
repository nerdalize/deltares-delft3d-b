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

      SUBROUTINE DHDELF ( FILNAM, IERROR )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: aug   1993 by Jan van Beek
C
C     FUNCTION            : deletes a file by name
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     FILNAM  CHAR*(*)      1     INPUT   file to be deleted
C     IERROR  INTEGER       1     OUTPUT  Error indication
C
      CHARACTER*(*) FILNAM
      INTEGER       IERROR
C
C     Local
C
      INTEGER IOLUN, ILUN
      LOGICAL LOPEN, LEXIST
C
C     Init
C
      IERROR = 0
      IOLUN  = 0
C
C     If file exist
C
      INQUIRE ( FILE=FILNAM, EXIST = LEXIST )
      IF ( .NOT. LEXIST ) RETURN
C
C     Select availeble ubitnumber
C
      DO 100 ILUN = 10, 99
         INQUIRE ( UNIT=ILUN, OPENED = LOPEN )
         IF ( .NOT. LOPEN ) THEN
            IOLUN = ILUN
            GOTO 101
         ENDIF
  100 CONTINUE
  101 CONTINUE
C
C     Open and close file
C
      IF ( IOLUN .NE. 0 ) THEN
         OPEN  ( IOLUN, FILE = FILNAM  , ERR = 900 )
         CLOSE ( IOLUN, STATUS='DELETE', ERR = 900 )
      ELSE
         IERROR = 1
      ENDIF
C
      RETURN
C
C     Errors close anyway
C
  900 CONTINUE
      CLOSE (IOLUN)
      IERROR = 1
      RETURN
      END
