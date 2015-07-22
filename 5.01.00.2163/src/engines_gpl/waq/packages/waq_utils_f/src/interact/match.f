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

      INTEGER FUNCTION MATCH(ARRAY, NELEM, NLENEL, STRING, NLENST,
     X     FLAG, FIRST)
C
      IMPLICIT INTEGER (A-Z)
      CHARACTER*1 STRING(1), LCHARA(4), LCHARB(4), BLANK
      CHARACTER*(nlenel) ARRAY(nelem)
      CHARACTER*4 CHARA,CHARB
      EQUIVALENCE (CHARA,LCHARA), (CHARB,LCHARB)
      DATA CHARA/'    '/, CHARB /'    '/, BLANK/' '/
C
      IF (NELEM.LE.0) GOTO 1005
      IF (FLAG.NE.0 .AND. FLAG.NE.1) GOTO 1004
      IF (NLENST.LE.0) GOTO 1002
      IF (NLENEL.LE.0 .OR. NLENEL.GT.256) GOTO 1001
      DO 10 I=1,NLENST
      LCHARA(1) = STRING(I)
      IF (CHARA.NE.BLANK) GOTO 11
10    CONTINUE
      GOTO 1003
11    NCHARS = NLENST
      IF (NLENST.GT.NLENEL) NCHARS = NLENEL
      NDXELM = 1
      FLG12 = 0
      DO 20 I=1,NELEM
      IF (FLG12.NE.0) GOTO 30
      DO 21 J=1,NCHARS
      LCHARA(1) = ARRAY(NDXELM)(j:J)
      LCHARB(1) = STRING(J)
      IF (CHARA.NE.CHARB) GOTO 20
21    CONTINUE
      IF (FLAG.EQ.0) GOTO 25
      IF (NCHARS.EQ.NLENEL) GOTO 36
      MCHARS = NCHARS + 1
      DO 35 J=MCHARS,NLENEL
      LCHARA(1) = ARRAY(NDXELM)(J:j)
      IF (CHARA.NE.BLANK) GOTO 20
35    CONTINUE
36    FIRST = I
      GOTO 40
25    FLG12 = 1
      FIRST = I
      GOTO 20
30    DO 31 J=1,NCHARS
      LCHARA(1) = ARRAY(NDXELM)(j:j)
      LCHARB(1) = STRING(J)
      IF (CHARA.NE.CHARB) GOTO 20
31    CONTINUE
      GOTO 100
20    NDXELM = NDXELM + 1
      IF (FLG12.EQ.0) GOTO 1000
40    MATCH = 1
      RETURN
100   MATCH = 2
      RETURN
1000  MATCH = 0
      RETURN
1001  MATCH = -1
      RETURN
1002  MATCH = -2
      RETURN
1003  MATCH = -3
      RETURN
1004  MATCH = -4
      RETURN
1005  MATCH = -5
      RETURN
      END
