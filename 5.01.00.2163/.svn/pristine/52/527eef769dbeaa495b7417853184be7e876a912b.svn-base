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

      INTEGER FUNCTION POPDLM(N)
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*1 DLMCHR(256),IGNCHR(256)
      COMMON /ZDLMTZ/DLM(256), IGN(256), IDLM, IIGN, PUSH(256), PTR
      DATA CHR/0/
C
      IF (PTR.EQ.0) GOTO 1001
      NDLM = PUSH(PTR-1)
      NIGN = PUSH(PTR)
      PTR = PTR - 2 - NIGN
      DO 10 I=1,NIGN
      CHR = PUSH(PTR+I)
C *** Convert INTEGER to CHARACTER
10    DLMCHR(I) = CHAR(CHR)
C *** Conversion completed.
      PTR = PTR - NDLM
      DO 20 I=1,NDLM
      CHR = PUSH(PTR+I)
C *** Convert INTEGER to CHARACTER
20    IGNCHR(I) = CHAR(CHR)
C *** Conversion completed.
      POPDLM = SETDLM(DLMCHR, NDLM, IGNCHR, NIGN)
      RETURN
1001  POPDLM = 1
      RETURN
      END
