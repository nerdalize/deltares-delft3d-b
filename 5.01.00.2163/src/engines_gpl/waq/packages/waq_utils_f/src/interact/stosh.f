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
C Integer function to extract part of one string and move it to another
C string. The move operation starts at POSIN en continues to LENIN,
C putting the result in elements POSOUT through LENOUT of RESULT.
C
      INTEGER FUNCTION STOSH (SOURCE,POSIN,LENIN,RESULT,POSOUT,LENOUT)
C
      CHARACTER*(*) SOURCE, RESULT
      INTEGER POSIN, POSOUT, LENIN, LENOUT
C
      IF (POSIN .LE. 0) GO TO 30
      IF (POSOUT .LE. 0) GO TO 30
      IF (LENIN .LT. POSIN) GO TO 40
      J = POSOUT - 1
      DO 20 I = POSIN,LENIN
      J = J + 1
20    RESULT(J:j) = SOURCE(I:i)
      LENOUT = LENIN - POSIN + 1
      STOSH = 0
      RETURN
30    STOSH = 1
      RETURN
40    STOSH = 2
      RETURN
      END
