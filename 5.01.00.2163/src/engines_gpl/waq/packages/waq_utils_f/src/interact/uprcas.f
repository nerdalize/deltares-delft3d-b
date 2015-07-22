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

      INTEGER FUNCTION UPRCAS (LOWCAS,UPPCAS,LENGTH)
      CHARACTER*1 LOWCAS(1),UPPCAS(1)
      INTEGER LENGTH,ILOW
C
      DO 10 I=1,LENGTH
      UPPCAS(I) = LOWCAS(I)
      ILOW = ICHAR(LOWCAS(I))
C     IF (ILOW .GT. 169 .OR.  ILOW .LT. 129) GO TO 10
C *** Replace previous line by following line on ASCI computers.
      IF (ILOW .GT. 122 .OR.  ILOW .LT.  97) GO TO 10
C     ILOW = ILOW + 64
C *** Replace previous line by following line on ASCI computers.
      ILOW = ILOW - 32
      UPPCAS(I) = CHAR(ILOW)
  10  CONTINUE
      UPRCAS = 0
      RETURN
      END
