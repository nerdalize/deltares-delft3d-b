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
      INTEGER FUNCTION PSHDLM(DLMCHR, NDLM, IGNCHR, NIGN)
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*1 DLMCHR(1),IGNCHR(1)
      COMMON /ZDLMTZ/DLM(256), IGN(256), IDLM, IIGN, PUSH(256), PTR
C
      IF (NDLM.LE.0 .OR. NDLM.GT.256) GOTO 1001
      IF (NIGN.LE.0 .OR. NIGN.GT.256) GOTO 1002
      IF (PTR+IDLM+IIGN+2.GT.256) GOTO 1003
      INDX = 0
      DO 10 CHR=1,IDLM
11    INDX = INDX + 1
      IF (DLM(INDX).EQ.0) GOTO 11
      PTR = PTR + 1
      PUSH(PTR) = INDX - 1
10    CONTINUE
      INDX = 0
      DO 20 CHR=1,IIGN
21    INDX = INDX+1
      IF (IGN(INDX).EQ.0) GOTO 21
      PTR = PTR + 1
      PUSH(PTR) = INDX - 1
20    CONTINUE
      PUSH(PTR+1) = IDLM
      PUSH(PTR+2) = IIGN
      PTR = PTR + 2
      PSHDLM = SETDLM(DLMCHR, NDLM, IGNCHR, NIGN)
      RETURN
1001  PSHDLM=1
      RETURN
1002  PSHDLM=2
      RETURN
1003  PSHDLM=3
      RETURN
      END
