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
      INTEGER FUNCTION STOST (SOURCE, POS, LENGTH, RESULT, LENOUT)
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*1 SOURCE(1), RESULT(1), BLANK
      COMMON /ZDLMTZ/DLM(256), IGN(256)
      DATA CHR/0/
      DATA BLANK /' '/
C
      IF (POS .LE. 0) GO TO 30
      IF (LENGTH .LT. POS) GO TO 40
      J = LENGTH + 1
      DO 10 IFND = POS, LENGTH
      J = J - 1
      CHR = ICHAR (SOURCE(J))
      IF (DLM(CHR+1) .EQ. 0) GO TO 15
10    CONTINUE
      GO TO 25
15    CONTINUE
C
      LENSIG = LENGTH - IFND + POS
      LENOUT = 0
      DO 20 I = POS,LENSIG
      LENOUT = LENOUT + 1
20    RESULT(LENOUT) = SOURCE(I)
      IEND = LENOUT + 1
      DO 22 I = IEND,LENGTH
22    RESULT(I) = BLANK
      STOST = 0
      RETURN
25    STOST = -1
      RETURN
30    STOST = 1
      RETURN
40    STOST = 2
      RETURN
      END
