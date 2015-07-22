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

C-----------------------------------------------------------------------
C QSLP Quick Simplex algorithm to solve a Linear Program.
C The technique used here is a variant of the primal - dual algorithm.
C The call to this routine is similar to the one to DOSP.
C
C Version 1.1
C Update 1.1: added check for negative "<" constraints.
C
C Program written by Hans Los.
C-----------------------------------------------------------------------
      SUBROUTINE QSLP(A,IA,NR,NC,B,LSC,C,IOPT,IRS,LIB,D,ID,X,P,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(1:IA,1:ID),B(*),C(*),D(*),X(*),P(*)
      INTEGER LIB(*),IER,LSC(*),IOPT(*),IRS(*)
      DATA NQSLP /0/
C
C If the subroutine is called for the first time, perform some initial
C checks.
C
      NQSLP = NQSLP + 1
      IF (NQSLP .GT. 1) GO TO 10
      IF (NR .GE. IA .OR. NC .GE. ID) THEN
         IER = 1000
         GO TO 340
      END IF
C
C Initiate control integers, copy arrays and set signs for constraints.
C
   10 CONTINUE
      IER = 0
      IRS(2) = 0
      ITER = 0
      ITFLAG = 0
      XOPT = 0.0D0
      X (NR+NC+1) = 0.0D0
C
C Initiate LIB. This array contains information on currently basic
C variables.
C
      DO 20 I = 1, NR + NC
         LIB(I) = I
         X (I) = 0.0D0
   20 CONTINUE
C
C Optionally copy A, B and C to D, which is not used for computational
C purposes in this subroutine (unlike DOSP).
C At the end the original values of these arrays are restored.
C
      IF (IOPT(3) .EQ. 1) GO TO 60
      K = 0
      L = NR*NC
      DO 40 J = 1, NC
         L = L + 1
         D(L) = C(J)
         DO 30 I = 1, NR
            K = K + 1
            D(K) = A(I,J)
   30    CONTINUE
   40    CONTINUE
         DO 50 I = 1, NC
            L = L + 1
            D(L) = B(I)
   50    CONTINUE
   60    CONTINUE
C
C Check if there are any ">" constraint. Reverse the sign of the entries
C in the A matrix and of the B vector.
C Check if all "<" constraints are positive. If not, problem is
C infeasible. Set exit values and leave.
C
      INEG = 0
      DO 80 I = 1, NR
         IF (LSC (I) .LE. 0) THEN
            IF (B(I) .GE. 0.0) GO TO 80
            IER = 100
            IRS(2) = 4
            IRS(3) = I

            GO TO 290
         END IF
         DO 70 J = 1, NC
            A(I,J) = - A(I,J)
   70    CONTINUE
         B (I) = - B(I)
         INEG = INEG + 1
   80 CONTINUE
C
C Reverse the sign of the C vector for a maximization problem.
C Note: minimization is NOT currently supported!
C
      IF (IOPT(4) .EQ. 1) THEN
         DO 90 J = 1,NC
            C (J) = - C(J)
   90    CONTINUE
      END IF
      IF (INEG .EQ. 0) GO TO 170
C-----------------------------------------------------------------------
C Satisfy greater than constraints, if there are any. Use the
C Dual method.
C-----------------------------------------------------------------------
  100 CONTINUE
      IER = 100
      IRS(2) = 4
      METHOD = 1
C
C Get pivot row. Use two different steps. First determine if there
C are any negative ">" constraints left. If this is the case, these
C are resolved first (INEG > 0). If there are none left, variables
C which may have become negative are selected for pivoting.
C Select the minimum value of B(I) as pivot row.
C
      INEG = 0
      DO 110 I = 1,NR
         IF (B(I) .GE. -1.0D-12) GO TO 110
         IF (LIB(I) .LE. NR .AND. LSC (LIB(I)) .EQ. 1) INEG = INEG + 1
  110 CONTINUE
      BPIVOT = 1.0D40
      BMIN = 1.0D40
      DO 120 I = 1, NR
         IF (B(I) .LT. BMIN) BMIN = B(I)
         IF (B(I) .GE. BPIVOT) GO TO 120
         IF (LIB(I) .LE. NR .AND. LSC (LIB(I)) .LE. 0
     1      .AND. INEG .GT. 0) GO TO 120
         BPIVOT = B(I)
         IP = I
  120 CONTINUE
C
C If minimum of B(I) is positive, the DUAL part of the algorithm can
C be terminated successfully. Continue with the primal method. A
C feasible solution exists. If no pivot was found, but the BMIN is
C still negative, no feasible solution exists.
C
      IF (BMIN .GT. -1.0D-12) GO TO 170
      IF (BPIVOT .GT. -1.0D-12) GO TO 140
C
C Get pivot column. For ">" constraints select Min C(J)/A(IP,J)
C under the condition that A(IP,J) and C(J) are negative.
C Additionally record the (absolute) minimum A(IP,J). If no pivot row
C was found, but there is at least one negative A(IP,J) value, this
C is used for pivoting. This pivot row is also used for "<" constraints
C and structural variables, which have become negative in previous
C iterations.
C
      JP = 0
      JPNEG = 0
      CPIVOT = 1.0D40
      APIVOT = 0.0
      DO 130 J = 1, NC
         AIPJ = A(IP,J)
         IF (AIPJ .GT. -1.0D-12) GO TO 130
         IF (AIPJ .LT. APIVOT) THEN
            APIVOT = AIPJ
            JPNEG = J
         END IF
         IF (INEG .EQ. 0) GO TO 130
         CJ = C(J)
         IF (CJ .GE. 0.0) GO TO 130
         IF (CJ/AIPJ .GT. CPIVOT) GO TO 130
         CPIVOT = CJ/AIPJ
         JP = J
  130 CONTINUE
      IF (JP .GT. 0) GO TO 200
      IF (JPNEG .GT. 0) THEN
         JP = JPNEG
         GO TO 200
      END IF
C-----------------------------------------------------------------------
C No feasible solution. Check which variables are negative. Replace
C all neagative structural variables and "<" constraint by ">"
C constraints.
C-----------------------------------------------------------------------
  140 CONTINUE
      METHOD = 3
      IP = 0
      BPIVOT = -1.0D-12
      BMIN = -1.0D-12
      DO 150 I = 1, NR
         IF (B(I) .LE. -1.0D-12 .AND. LIB(I) .LE. NR .AND.
     1       LSC (LIB(I)) .EQ. 1) THEN
             IF (B(I) .LT. BMIN) THEN
                BMIN = B(I)
                IRS(3) = LIB(I)
             END IF
             GO TO 150
         END IF
         IF (B(I) .GE. BPIVOT) GO TO 150
         BPIVOT = B(I)
         IP = I
  150 CONTINUE
      IF (IP .EQ. 0) GO TO 290
C
C Find the pivot column. Use Max A(IP,J) under the condition
C A(IP,J) > 0.0. Note: do not replace IP by a structural variable!
C
      JP = 0
      APIVOT = -1.0D40
      DO 160 J = 1, NC
         IF (LIB(NR+J) .GT. NR .OR. LSC(LIB(NR+J)) .NE. 1) GO TO 160
         IF (A(IP,J) .LT. 1.0D-12) GO TO 160
         IF (A(IP,J) .LT. APIVOT) GO TO 160
         APIVOT = A(IP,J)
         JP = J
  160 CONTINUE
      IF (JP .EQ. 0) GO TO 290
      GO TO 200
C-----------------------------------------------------------------------
C Satisfy smaller than constraints using the primal method.
C-----------------------------------------------------------------------
  170 CONTINUE
C
C Get pivot column. Use Minimum C(J). Terminate when CMIN >= 0.0.
C
      METHOD = 2
      JP = 0
      CPIVOT = 1.0D40
      DO 180 J = 1, NC
         IF (C(J) .GE. CPIVOT) GO TO 180
         CPIVOT = C(J)
         JP = J
  180 CONTINUE
      IF (CPIVOT .GE. -1.0D-12) THEN
         IER = 0
         IRS(2) = 0
         IRS(3) = NR + NC + 1
         GO TO 290
      END IF
C
C Get pivot row. Choose IP as MIN B(I) / A(I,JP) under the condition
C that A(I,JP) > 0.0.
C
      IP = 0
      BPIVOT = 1.0D40
      DO 190 I = 1, NR
         AIJP = A(I,JP)
         IF (AIJP .LT. 1.0D-12) GO TO 190
         BI = B(I)
         IF (BI/AIJP .GE. BPIVOT) GO TO 190
         BPIVOT = BI/AIJP
         IP = I
  190 CONTINUE
C-----------------------------------------------------------------------
C Start pivot operation. Update A, B and C.
C-----------------------------------------------------------------------
  200 CONTINUE
      ITER = ITER + 1
      ITFLAG = ITFLAG + 1
C
C Check the number of iterations. If exceeded, abort.
C
      IF (ITER .GE. IOPT(1)) THEN
         IER = 100
         IRS(2) = 2
         GO TO 290
      END IF
C
C Check the number of operations since the last update of the arrays.
C If exceeded, reset all small numbers to 0.0D0 to avoid round-off
C errors becoming to large.
C
      IF (ITFLAG .GE. IOPT(2)) THEN
         ITFLAG = 0
         DO 220 I = 1, NR
            IF (DABS(B(I)) .LT. 1.0D-12) B(I) = 0.0D0
            DO 210 J = I, NC
               IF (DABS(A(I,J)) .LT. 1.0D-12) A(I,J) = 0.0D0
  210       CONTINUE
  220    CONTINUE
         DO 230 J = I, NC
            IF (DABS(C(J)) .LT. 1.0D-12) C(J) = 0.0D0
  230    CONTINUE
      END IF
C
C Modify pivot and pivot row.
C
      AP = A (IP,JP)
      A(IP,JP) = 1 / AP
      DO 240 I = 1, NR
         IF (I .EQ. IP) GO TO 240
         A (I,JP) = - A (I,JP) / AP
  240 CONTINUE
C
C Update optimum.
C
      CJPAP = C(JP)/AP
      XOPT = XOPT - B(IP) * CJPAP
C
C Update C vector.
C
      DO 250 J = 1, NC
         IF (J .EQ. JP) GO TO 250
         IF (DABS(A(IP,J)) .LT. 1.0D-12) GO TO 250
         C(J) = C(J) - A(IP,J) * CJPAP
  250 CONTINUE
C
C Modify elements not in pivot row or column. Update B vector.
C
      DO 270 I = 1, NR
         IF (I .EQ. IP) GO TO 270
         AIJP =  A(I,JP)
         IF (DABS(AIJP) .LT. 1.0D-12) GO TO 270
         DO 260 J = 1, NC
            IF (J .EQ. JP) GO TO 260
            IF (DABS(A(IP,J)) .LT. 1.0D-12) GO TO 260
            A(I,J) = A(I,J) + A(IP,J) * AIJP
  260    CONTINUE
         BI = B (I) + B(IP) * AIJP
         IF (DABS(BI) .LT. 1.0D-12) THEN
            B(I) = 0.0D0
         ELSE
            B(I) = BI
         END IF
  270 CONTINUE
C
C Modify pivot column.
C
      B(IP) = B(IP)/AP
      C(JP) = - CJPAP
      DO 280 J = 1, NC
         IF (J .EQ. JP) GO TO 280
         A(IP,J) = A(IP,J)/AP
  280 CONTINUE
C
C Update LIB to indicate basic variables.
C
      IHELP1 = LIB (JP + NR)
      IHELP2 = LIB (IP)
      LIB (JP + NR) = IHELP2
      LIB (IP) = IHELP1
      IF (METHOD .EQ. 1) GO TO 100
      IF (METHOD .EQ. 2) GO TO 170
      IF (METHOD .EQ. 3) GO TO 140
C-----------------------------------------------------------------------
C Get X-vector and leave subroutine.
C-----------------------------------------------------------------------
  290 CONTINUE
      DO 300 I=1,NR
         X(LIB(I)) = B(I)
  300 CONTINUE
      X(NR+NC+1) = XOPT
C
C Optionally restore the original A, B and C arrays.
C
      IF (IOPT(3) .EQ. 1) GO TO 340
      K = 0
      L = NR*NC
      DO 320 J = 1, NC
         L = L + 1
         C(J) = D(L)
         DO 310 I = 1, NR
            K = K + 1
            A(I,J) = D(K)
  310    CONTINUE
  320    CONTINUE
         DO 330 I = 1, NR
            L = L + 1
            B(I) = D(L)
  330    CONTINUE
  340 CONTINUE
      RETURN
      END
