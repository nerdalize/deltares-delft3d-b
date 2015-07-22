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
C Integer function to search for the first occurrence of a needle in
C a haystack.
C The arguments of this function are similar to the EXEC2 function
C &LOCATION OF.
C
      INTEGER FUNCTION LOCATE (SOURCE,NEEDLE,MAXSO,MAXNE)
      CHARACTER*(*) SOURCE,NEEDLE
      INTEGER MAXSO,MAXNE,LENSTR
C
C Determine the lengths of SOURCE and NEEDLE. Compare the first
C character in NEEDLE with each character in SOURCE. If a match is
C found, continue up to the last character in NEEDLE to get a complete
C match.
C
      IF (MAXSO .LE. 0 .OR. MAXSO .GT. 255) GO TO 100
      IF (MAXNE .LE. 0 .OR. MAXNE .GT. 255) GO TO 100
      LENSO = LENSTR (SOURCE,MAXSO)
      LENNE = LENSTR (NEEDLE,MAXNE)
      DO 20 I = 1,LENSO
      IF (NEEDLE(1:1) .NE. SOURCE(I:i)) GO TO 20
      DO 10 J = 2,LENNE
      IF (NEEDLE(J:j) .NE. SOURCE(I+J-1:i+j-1)) GO TO 15
10    CONTINUE
      LOCATE = I
      RETURN
15    CONTINUE
20    CONTINUE
C
C No match is found. Set LOCATE to 0.
C
      LOCATE = 0
      RETURN
C
C An error is determined. Set LOCATE to -1.
C
100   LOCATE = -1
      RETURN
      END
