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

      SUBROUTINE DHSLEN ( STRING, ILEN  )
C
C     Deltares
C
C     Created             : November 1998 by Jan van Beek
C
C     Function            : Retrun length of trimmed string
C                           Always >= 1
C
C     Parameters          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     STRING  CHAR*(*) 1          IN      String
C     ILEN    INTEGER  1          OUT     Length of trimmed string
C
C     Declaration of arguments
C
      INTEGER       ILEN
      CHARACTER*(*) STRING
C
C     Local declaration
C
C     ISLEN   INTEGER  1          LOCAL   length of variable
C
      INTEGER    ISLEN
C
      ISLEN = LEN(STRING)
      ILEN  = 1
      DO I = 1 , ISLEN
         IF ( STRING(I:I) .NE. ' ' ) ILEN = I
      ENDDO
C
      RETURN
      END
