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

      SUBROUTINE DHPATH ( FILNAM, FILPATH, PATHLEN)
C
C
C     Deltares
C
C     CREATED       : june  2002 BY J.K.L. van Beek
C
C     FUNCTION      : get file path and path length including last separator
C
C     SUBROUTINE CALLED  : none
C
C     LOGICAL UNITS      : none
C
C     PARAMETERS         :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     FILNAM  CHAR*(*) 1          I       filename
C     FILPATH CHAR*(*) 1          O       file path
C     PATHLEN INT      1          O       path length
C
      IMPLICIT NONE
C
C     Declaration of arguments
C
      INTEGER       PATHLEN
      CHARACTER*(*) FILNAM, FILPATH
C
C     Local declaration
C
      INTEGER       LENNAM, ICH
      CHARACTER     DIRSEP_DOS, DIRSEP_UX
C
      DIRSEP_DOS = CHAR(92)
      DIRSEP_UX  = CHAR(47)
C
C     blank name get out of here
C
      FILPATH = ' '
      PATHLEN = 0
      IF ( FILNAM .EQ. ' ' ) RETURN
C
      LENNAM = LEN(FILNAM)
C
C     get last directory seperator
C
      DO ICH = LENNAM , 1 , -1
         IF ( FILNAM(ICH:ICH) .EQ. DIRSEP_DOS .OR.
     +        FILNAM(ICH:ICH) .EQ. DIRSEP_UX       ) THEN
            PATHLEN = ICH
            EXIT
         ENDIF
      ENDDO
C
      IF ( PATHLEN .GT. 0 ) THEN
         FILPATH = FILNAM(1:PATHLEN)
      ENDIF
C
      RETURN
      END
