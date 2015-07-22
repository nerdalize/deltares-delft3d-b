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

      SUBROUTINE DHGWRD ( LINE  , IWRD  , WORD  )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED       : june  1993 BY J.K.L. van Beek
C
C     FUNCTION      : Returns the IWRD'th word from line
C
C     SUBROUTINE CALLED  : none
C
C     LOGICAL UNITS      : none
C
C     PARAMETERS         :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LINE    CHAR*(*)      1     INPUT   line
C     IWRD    INT           1     INPUT   number of argument asked
C     WORD    CHAR*(*)      1     OUTPUT  value of the IWRD'th word
C
C     Declaration of arguments
C
      INTEGER       IWRD
      CHARACTER*(*) LINE  , WORD
C
C     Local declaration
C
      LOGICAL       NEW
C
C     Test arguments
C
      IF ( IWRD .EQ. 0 ) THEN
         WORD = ' '
         GOTO 200
      ENDIF
C
C     initialize
C
      IA = 0
      WORD = ' '
      NEW = .TRUE.
      LENLIN = LEN(LINE)
C
C     Skip till iwrd'th word
C
      DO 100 ICH = 1 , LENLIN
         IF ( LINE(ICH:ICH) .NE. ' ' .AND. NEW ) THEN
            NEW = .FALSE.
            I1 = ICH
            IA = IA + 1
         ENDIF
         IF ( LINE(ICH:ICH) .EQ. ' ' ) THEN
            IF ( IA .EQ. IWRD ) THEN
               WORD = LINE(I1:ICH)
               GOTO 200
            ENDIF
            NEW = .TRUE.
         ENDIF
  100 CONTINUE
      IF ( IA .EQ. IWRD ) THEN
         WORD = LINE(I1:LENLIN)
      ENDIF
C
  200 CONTINUE
C
      RETURN
      END
