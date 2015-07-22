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

      INTEGER FUNCTION STOI(SOURCE, STPOS, LENGTH, NUMBER)
C
      IMPLICIT INTEGER (A-Z)
      INTEGER STOD
      CHARACTER*1 SOURCE(1)
      DOUBLE PRECISION DP
C
      RC = STOD(SOURCE, STPOS, LENGTH, DP)
      IF (RC.NE.0) GOTO 1
      IF (DP.GT.2147483647.0D0) GOTO 1010
C     IF (DP.LT.-2147483648.0D0) GOTO 1011
      IF (DP.LT.-2147483647.0D0) GOTO 1011
      NUMBER = DP
1     STOI = RC
      RETURN
1010  STOI = 10
      RETURN
1011  STOI = 11
      RETURN
      END
