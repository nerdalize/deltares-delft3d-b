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

C
C Integer function to replace part of a character string with blanks.
C
      INTEGER FUNCTION WIPE (SOURCE,FIRST,LAST)
      CHARACTER*1 SOURCE(*),BLANK
      INTEGER FIRST, LAST
      DATA BLANK /' '/
C
      IF (LAST .LE. 0 .OR. LAST .GT. 236) GOTO 100
      IF (FIRST .LE. 0 .OR. FIRST .GT. 236) GOTO 100
      IF (FIRST .GT. LAST) GO TO 200
      DO 10 I = FIRST,LAST
      SOURCE(I) = BLANK
10    CONTINUE
      WIPE = 0
      RETURN
100   CONTINUE
      WIPE = 1
      RETURN
200   CONTINUE
      WIPE = 2
      RETURN
      END
