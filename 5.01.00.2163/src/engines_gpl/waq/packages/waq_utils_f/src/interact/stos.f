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

      INTEGER FUNCTION STOS (SOURCE, POS, LENGTH, RESULT, LENOUT)
C
      CHARACTER*1 SOURCE(1), RESULT(1)
      INTEGER POS, LENGTH, LENOUT
C
      IF (POS .LE. 0) GO TO 30
      IF (LENGTH .LT. POS) GO TO 40
      LENOUT = 0
      DO 20 I = POS,LENGTH
      LENOUT = LENOUT + 1
20    RESULT(LENOUT) = SOURCE(I)
      STOS = 0
      RETURN
30    STOS = 1
      RETURN
40    STOS = 2
      RETURN
      END
