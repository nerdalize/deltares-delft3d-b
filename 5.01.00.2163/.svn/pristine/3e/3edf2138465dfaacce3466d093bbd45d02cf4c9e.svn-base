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

      INTEGER FUNCTION SETDLM(DLMCHR, NDLM, IGNCHR, NIGN)
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*1 DLMCHR(1), IGNCHR(1)
      COMMON /ZDLMTZ/DLM(256), IGN(256), IDLM, IIGN, PUSH(256), PTR
      DATA CHR/0/
C
      IF (NDLM.LE.0 .OR. NDLM.GT.256) GOTO 1001
      IF (NIGN.LE.0 .OR. NIGN.GT.256) GOTO 1002
      DO 10 I=1,256
      DLM(I) = 0
10    IGN(I) = 0
      IDLM = 0
      IIGN = 0
      DO 20 I=1,NDLM
C *** Convert CHARACTER to INTEGER.
      CHR = ICHAR(DLMCHR(I))
C *** Conversion completed.
      IF (DLM(CHR+1).EQ.0) IDLM = IDLM + 1
20    DLM(CHR+1) = 1
      DO 30 I=1,NIGN
C *** Convert CHARACTER to INTEGER.
      CHR = ICHAR(IGNCHR(I))
C *** Conversion completed.
      IF (IGN(CHR+1).EQ.0) IIGN = IIGN + 1
30    IGN(CHR+1) = 1
      SETDLM = 0
      RETURN
1001  SETDLM = 1
      RETURN
1002  SETDLM = 2
      RETURN
      END
