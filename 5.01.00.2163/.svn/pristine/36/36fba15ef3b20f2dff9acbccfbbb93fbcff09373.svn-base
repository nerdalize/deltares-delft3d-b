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

      INTEGER FUNCTION STOD(SOURCE, STPOS, LENGTH, NUMBER)
C
      IMPLICIT INTEGER (A-Z)
      DOUBLE PRECISION NUMBER, DB, DP, CONVER
      CHARACTER*1 SOURCE(1)
      CHARACTER*1 CCHRZE,CPLUS,CMINUS,CZERO,CNINE,CDECPT,CE,CD,CBLANK
      CHARACTER*1 CCHRB
      COMMON /ZDLMTZ/DLM(256), IGN(256)
      DATA CCHRZE, CHRA, CCHRB/'0', 0, ' '/,
     X CPLUS,CMINUS,CZERO,CNINE,CDECPT,CE,CD,CBLANK/'+', '-', '0', '9',
     X '.', 'E', 'D', ' '/
C
C Convert CHARACTERS to corresponding INTEGERS.
C
      CHR0 = ICHAR(CCHRZE)
      CHRB = ICHAR(CCHRB)
      PLUS = ICHAR(CPLUS)
      MINUS= ICHAR(CMINUS)
      ZERO = ICHAR(CZERO)
      NINE = ICHAR(CNINE)
      DECPT = ICHAR(CDECPT)
      E = ICHAR(CE)
      D = ICHAR(CD)
      BLANK = ICHAR(CBLANK)
C
      IF (STPOS.LE.0) GOTO 1001
      IF (LENGTH.LT.STPOS) GOTO 1002
      DO 10 IFND=STPOS,LENGTH
C *** Convert CHARACTER to INTEGER.
      CHRA = ICHAR(SOURCE(IFND))
C *** Conversion completed.
      IF (IGN(CHRA+1).EQ.0) GOTO 15
10    CONTINUE
      GOTO 100
15    SFLAG = 0
      ESFLAG = 0
      MFLAG = 0
      EPFLAG = 0
      DB = 0.0D0
      WFLAG = 1
      EFLAG = 0
      EXP = 0
      DO 20 JFND=IFND,LENGTH
C *** Convert CHARACTER to INTEGER.
      CHRA = ICHAR(SOURCE(JFND))
      CHRB = CHRA
C *** Conversion completed.
      GOTO (30,40,50,60,70),WFLAG
30    IF (CHRB.EQ.PLUS) GOTO 32
      IF (CHRB.NE.MINUS) GOTO 33
      SFLAG = 1
32    WFLAG = 2
      SFLAG = SFLAG + 1
      GOTO 21
33    WFLAG = 2
40    IF (CHRB.LT.ZERO .OR. CHRB.GT.NINE) GOTO 41
      CONVER = CHRA - CHR0
      DB = DB*10.0D0 + CONVER
      MFLAG = 1
      GOTO 21
41    IF (CHRB.NE.DECPT) GOTO 51
      DP = 1.0D0
      WFLAG = 3
      GOTO 21
50    IF (CHRB.LT.ZERO .OR. CHRB.GT.NINE) GOTO 51
      CONVER = CHRA - CHR0
      DP = DP/10.0D0
      DB = DB + CONVER*DP
      MFLAG = 1
      GOTO 21
51    IF (MFLAG.EQ.0) DB = 1.0
      IF (CHRB.NE.E.AND.CHRB.NE.D) GOTO 71
      WFLAG = 4
      EPFLAG = 1
      GOTO 21
60    IF (CHRB.EQ.PLUS) GOTO 62
      IF (CHRB.NE.MINUS) GOTO 63
      ESFLAG = 1
62    WFLAG = 5
      ESFLAG = ESFLAG + 1
      GOTO 21
63    WFLAG = 5
70    IF (CHRB.LT.ZERO .OR. CHRB.GT.NINE) GOTO 71
      N = CHRA - CHR0
      EXP = EXP*10 + N
      IF (EXP.GT.78) GOTO 1007
      EFLAG = 1
      GOTO 21
71    IF (DLM(CHRA+1).EQ.0) GOTO 1008
      GOTO 22
21    CONTINUE
20    CONTINUE
      JFND = LENGTH + 1
22    IF (IFND.EQ.JFND) GOTO 1008
      IF (SFLAG.NE.0 .AND. MFLAG.EQ.0) GOTO 1003
      IF (EPFLAG.EQ.1 .AND. ESFLAG.EQ.0 .AND. EFLAG.EQ.0) GOTO 1005
      IF (ESFLAG.EQ.1 .AND. EFLAG.EQ.0) GOTO 1006
      IF (SFLAG.EQ.2) DB = -DB
      IF (ESFLAG.EQ.2) EXP = -EXP
      NUMBER = DB * 10.0D0**EXP
      STPOS = JFND
      STOD = 0
      RETURN
100   STOD = -1
      RETURN
1001  STOD = 1
      RETURN
1002  STOD = 2
      RETURN
1003  STOD = 3
      RETURN
1005  STOD = 5
      RETURN
1006  STOD = 6
      RETURN
1007  STOD = 7
      RETURN
1008  STOD = 8
      RETURN
      END
