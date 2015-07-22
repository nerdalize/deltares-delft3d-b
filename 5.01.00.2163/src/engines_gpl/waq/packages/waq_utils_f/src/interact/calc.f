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

      INTEGER FUNCTION CALC(SOURCE, STPOS, LENGTH, TYPE, VALUE)
      IMPLICIT INTEGER (A-Z)
      CHARACTER*8 EQUELM(18), TOKEN, DELIMT
      CHARACTER*4 IGNORE
      CHARACTER*1 SOURCE(1)
      REAL*4  NSTACK(100), NUMBER, VALUE, FUNCS
      INTEGER FSTACK(200), PRIOR(18), OPTYPE(19)
      DATA EQUELM/ '(', ')', '+', '-', '*', '/', ']', 'SIN', 'COS',
     X 'TAN', 'EXP', 'LN', 'ABS', 'INT', 'TRUNC', 'LOG10', 'SQRT',
     X 'LOG'/, NOPS/18/
      DATA PRIOR /0, 0, 4, 4, 3, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
      DATA OPTYPE/1, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
     X 5/
      DATA IGNORE, DELIMT/' ', ' ()+-*/]'/
C
      IF (STPOS.LE.0) GOTO 101
      IF (LENGTH.LT.STPOS) GOTO 102
      IF (TYPE.LT.0.OR.TYPE.GT.3) GOTO 103
C
C SET DELIMITING CHARACTERS
      RC = PSHDLM(DELIMT, 8, IGNORE, 1)
      POS = STPOS
      IF (TYPE.EQ.0.OR.TYPE.EQ.2) GOTO 300
      NPAREN = 0
      FSTKCT = 2
      NSTKCT = 2
      PRVOP = 6
C
300   PRVPOS = POS
      IF (GETS(SOURCE,POS,LENGTH,8,TOKEN,LEN).LT.0) GOTO 500
      DO 301 OP=1,NOPS
      IF (TOKEN.EQ.EQUELM(OP)) GOTO 302
301   CONTINUE
303   POS = PRVPOS
      IF (STOR(SOURCE,POS,LENGTH,NUMBER).NE.0) GOTO 400
      OP = NOPS + 1
302   ITYPE = OPTYPE(OP)
      GOTO (311,312,313,314,315,316), PRVOP
C
C PREVIOUS TOKEN WAS A (
C CURRENT CAN BE A NUMBER, (, OR FUNCTION
311   IF (ITYPE.EQ.5) GOTO 325
      IF (ITYPE.EQ.1) GOTO 321
      IF (ITYPE.EQ.4) GOTO 324
      IF (OP.EQ.3.OR.OP.EQ.4) GOTO 303
      GOTO 400
C
C PREVIOUS TOKEN WAS A )
C CURRENT CAN BE A ) OR ARITHMETIC OPERATOR
312   IF (ITYPE.EQ.2) GOTO 322
      IF (ITYPE.EQ.3) GOTO 323
      GOTO 400
C
C PREVIOUS TOKEN WAS AN ARITHMETIC OPERATOR
C CURRENT CAN BE A NUMBER, (, OR FUNCTION
313   IF (ITYPE.EQ.5) GOTO 325
      IF (ITYPE.EQ.1) GOTO 321
      IF (ITYPE.EQ.4) GOTO 324
      GOTO 400
C
C PREVIOUS TOKEN WAS A FUNCTION
C CURRENT CAN BE A (
314   IF (ITYPE.EQ.1) GOTO 321
      GOTO 400
C
C PREVIOUS TOKEN WAS A NUMBER
C CURRENT CAN BE A ) OR ARITHMETIC OPERATOR
315   IF (ITYPE.EQ.2) GOTO 322
      IF (ITYPE.EQ.3) GOTO 323
      GOTO 400
C
C THERE WAS NO PREVIOUS TOKEN
C CURRENT CAN BE A NUMBER, (, OR FUNCTION
316   IF (ITYPE.EQ.5) GOTO 325
      IF (ITYPE.EQ.1) GOTO 321
      IF (ITYPE.EQ.4) GOTO 324
      IF (OP.EQ.3.OR.OP.EQ.4) GOTO 303
      GOTO 400
C
C TOKEN IS A (
321   NPAREN = NPAREN + 1
      PRVOP = 1
      FSTKCT = FSTKCT + 1
      FSTACK(FSTKCT) = OP
      GOTO 300
C
C TOKEN IS A )
322   IF (NPAREN.EQ.0) GOTO 409
      NPAREN = NPAREN - 1
      PRVOP = 2
      FSTKCT = FSTKCT + 1
      FSTACK(FSTKCT) = OP
      GOTO 300
C
C TOKEN IS AN ARITHMETIC OPERATOR
323   PRVOP = 3
      FSTKCT = FSTKCT + 1
      FSTACK(FSTKCT) = OP
      GOTO 300
C
C TOKEN IS A FUNCTION
324   PRVOP = 4
      FSTKCT = FSTKCT + 1
      FSTACK(FSTKCT) = OP
      GOTO 300
C
C TOKEN IS A NUMBER
325   PRVOP = 5
      NSTKCT = NSTKCT + 1
      NSTACK(NSTKCT) = NUMBER
      FSTKCT = FSTKCT + 1
      FSTACK(FSTKCT) = OP
      GOTO 300
C
C AN ERROR HAS OCCURRED; SET RETURN CODE
400   GOTO (401,403,401,405,403,401), PRVOP
401   CALC =  999 + PRVPOS
      GOTO 600
403   CALC = 1999 + PRVPOS
      GOTO 600
405   CALC = 2999 + PRVPOS
      GOTO 600
409   CALC = 3999 + PRVPOS
      GOTO 600
C
500   IF (TYPE.EQ.0.OR.TYPE.EQ.1) GOTO 601
      IF (FSTKCT.EQ.2) GOTO 100
      IF (PRVOP.NE.2.AND.PRVOP.NE.5) GOTO 400
      FSTK = 3
      NSTK = 3
      FSTKMX = FSTKCT
      FSTKCT = 1
      FSTACK(FSTKCT) = 1
      NSTKCT = 0
C
501   OP = FSTACK(FSTK)
      ITYPE = OPTYPE(OP)
      GOTO (511,521,531,511,551), ITYPE
C
C TOKEN IS A ( OR FUNCTION
511   FSTKCT = FSTKCT + 1
      FSTACK(FSTKCT) = OP
      GOTO 599
C
C TOKEN IS A )
521   PRVOP = FSTACK(FSTKCT)
      IF (PRVOP.EQ.1) GOTO 522
      NUMBER = FUNCS(PRVOP-2, NSTACK(NSTKCT), NUMBER)
      IF (OPTYPE(PRVOP).NE.4) NSTKCT = NSTKCT - 1
      FSTKCT = FSTKCT - 1
      GOTO 521
522   FSTKCT = FSTKCT - 1
      GOTO 599
C
C TOKEN IS AN ARITHMETIC OPERATOR
531   PRVOP = FSTACK(FSTKCT)
      IF (PRVOP.EQ.1) GOTO 511
      IF (PRIOR(PRVOP).GT.PRIOR(OP)) GOTO 511
      NUMBER = FUNCS(PRVOP-2, NSTACK(NSTKCT), NUMBER)
      IF (OPTYPE(PRVOP).NE.4) NSTKCT = NSTKCT - 1
      FSTKCT = FSTKCT - 1
      GOTO 531
C
C TOKEN IS A NUMBER
551   NSTKCT = NSTKCT + 1
      NSTACK(NSTKCT) = NUMBER
      NUMBER = NSTACK(NSTK)
      NSTK = NSTK + 1
599   FSTK = FSTK + 1
      IF (FSTK.LE.FSTKMX) GOTO 501
591   PRVOP = FSTACK(FSTKCT)
      IF (PRVOP.EQ.1) GOTO 592
      NUMBER = FUNCS(PRVOP-2, NSTACK(NSTKCT), NUMBER)
      IF (OPTYPE(PRVOP).NE.4) NSTKCT = NSTKCT - 1
      FSTKCT = FSTKCT - 1
      GOTO 591
592   FSTKCT = FSTKCT - 1
      IF (FSTKCT.GT.0) GOTO 591
      VALUE = NUMBER
      NPAREN = 0
      FSTKCT = 2
      NSTKCT = 2
      PRVOP = 6
C
601   CALC = 0
C
600   RC = POPDLM(1)
      RETURN
100   CALC = -1
      GOTO 600
101   CALC = 1
      RETURN
102   CALC = 2
      RETURN
103   CALC = 3
      RETURN
      END
      FUNCTION FUNCS(INDX, A, B)
      REAL*4 A, B, SIN, COS, TAN, EXP, LOG, ABS,
     X LOG10, SQRT
C
      GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,10), INDX
      FUNCS = 0.0D0
      RETURN
C
1     FUNCS = A+B
      RETURN
2     FUNCS = A-B
      RETURN
3     FUNCS = A*B
      RETURN
4     FUNCS = A/B
      RETURN
5     FUNCS = A**B
      RETURN
6     FUNCS = SIN(B)
      RETURN
7     FUNCS = COS(B)
      RETURN
8     FUNCS = TAN(B)
      RETURN
9     FUNCS = EXP(B)
      RETURN
10    FUNCS = LOG(B)
      RETURN
11    FUNCS = ABS(B)
      RETURN
12    FUNCS = INT(B)
      RETURN
13    FUNCS = B-INT(B)
      RETURN
14    FUNCS = LOG10(B)
      RETURN
15    FUNCS = SQRT(B)
      RETURN
      END
