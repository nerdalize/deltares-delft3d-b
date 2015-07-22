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

      SUBROUTINE DELMAT (N,NUC,NLC,M,A,B,IOPT)
C
C         THE SUBROUTINE DELMAT IS A DELFT-HYDRAULICS-LABORATORY
C         PRODUCT TO SOLVE SETS OF LINEAR EQUATIONS DESCRIBED BY
C         BAND MATRICES WITH LARGEST ELEMENTS ON THE DIAGONAL.
C         THE MAKER GIVES NO WARRANTY CONCERNING PROPER SOLUTIONS.
C         NOTHING OF THE CODE CAN BE REPRODUCED WITHOUT PERMISSION
C         OF THE MAKER
C
C         THE SUBROUTINE EXPECTS THE MATRIX "A" TO BE STORED IN A
C         ONE DIMENSIONAL EQUIVALENT OF THE TWO DIMENSIONAL MATRIX
C         REPRESENTATION. IN TWO DIMENSIONAL FORM THE STORAGE RULES
C         USED BY THE IMSL PACKAGE FOR BAND MATRICES MUST BE USED.
C         HOWEVER THE ROWS AND COLLUMNS MUST BE INTERCHANGED.
C         THE SAME HOLDS FOR THE FULL MATRIX OF KNOWN VECTORS "B".
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C
C         NOTATION:                               CHANGED DURING
C                                                  CALCULATION
C
C      N    = ORDER OF MATRIX "A"                      NO
C      NUC  = NUMBER OF UPPER CODIAGONALS              NO
C      NLC  = NUMBER OF LOWER CODIAGONALS              NO
C      M    = NUMBER OF KNOWN VECTORS OF               NO
C             ORDER N IN "B"
C      A    = BANDMATRIX IN THE EQUIVALENT            YES
C             ONE DIMENSIONAL FORM
C      B    = MATRIX OF KNOWN VECTORS IN             YES/NO
C             ONE DIMENSIONAL FORM
C      IOPT = CALCULATION OPTION                       NO
C
C      OPTION 0 RETURNS THE LU-DECOMPOSITION IN MATRIX A, WHICH CAN
C                       BE USED AGAIN FOR NEW KNOWN VECTORS AND IT
C                       RETURNS THE SOLUTION FOR THE UNKNOWN VECTORS
C                       IN MATRIX B
C      OPTION 1 RETURNS ONLY THE DECOMPOSITION, B REMAINS UNCHANGED
C      OPTION 2 RETURNS THE SOLUTION FOR THE UNKNOWN VECTORS IN B
C                       IT NEEDS A PROPER DECOMPOSED AND STORED MATRIX
C                       A AS AN INPUT
C
      use timers

      DIMENSION A(*),B(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "delmat", ithandl )
      NMUC = N - NUC
      NMLC = N - NLC
      NDM1 = NUC + NLC
      ND   = NDM1 + 1
      IF (IOPT.EQ.2) GOTO 1000
C
C           THE LU-DECOMPOSITION
C
C
C           K1 IS THE OUTER LOOP VARIABLE, COUNTING THE NUMBER OF
C              COLLUMNS WITH FULL LENGTH
C           L1 IS THE CORRESPONDING LIMIT VARIABLE
C           P  IS THE PIVOT ELEMENT. FOR THE ADVECTION DISPERSION
C              EQUATION THIS IS ALMOST ALWAYS THE LARGEST ELEMENT
C
      K1 = NLC + 1
      L1 = K1 + NMLC*ND
  100 P  = A(K1)
      IF ( ABS(P) .LT. 1.0E-35 ) THEN
          WRITE(6, '('' Matrix for DELMAT singular at element:'',I5)')
     *          K1/ND + 1
          CALL SRSTOP(1)
      ENDIF
C
C           K2 IS THE MIDDLE LOOP VARIABLE COUNTING THE NUMBER OF
C              ELEMENTS TO BE ELEMINATED
C           L2 IS THE CORRESPONDING LIMIT VARIABLE
C           F  IS THE MULTIPLICATION FACTOR FOR THE CORRECTION OF
C              THE ROW OF ELIMINATION. IT ENTERS A(K2) AS THE NEGATIVE
C              OF THE ELEMENT OF THE NEW LOWER TRIANGULAR MATRIX
C
      K2 = K1
      L2 = K1 + NLC*NDM1
  200 K2 = K2 + NDM1
      F  = A(K2)/P
      A(K2) = F
      K3 = K1
C
C           K4 IS THE INNER LOOP VARIABLE COUNTING THE ELEMENTS ON
C              THE ROW OF ELIMINATION
C           L4 IS THE CORRESPONDING LIMMIT VARIABLE
C           K3 IS THE CORRESPONDING COUNTER FOR THE ELEMENTS ON THE
C              ROW OF THE DIAGONAL ELEMENT
C
      K4 = K2
      L4 = K2 + NUC
  300 K3 = K3 + 1
      K4 = K4 + 1
      A(K4) = A(K4) - F * A(K3)
C
C           BOOKKEEPING OF THE LOOP VARIABLES
C
      IF (K4.LT.L4) GOTO 300
      IF (K2.LT.L2) GOTO 200
      K1 = K1 + ND
      IF (K1.LT.L1) GOTO 100
      IF (NLC.EQ.1) GOTO 700
C
C           THE COLLUMNS BECOME SHORTER, THE REST IS THE SAME
C
      N1 = NLC
      L1 = (N-1) * ND
  400 N1 = N1 - 1
      P  = A(K1)
      IF ( ABS(P) .LT. 1.0E-35 ) THEN
         WRITE(6,'('' Matrix for DELMAT singular at element:'',I5)')
     *         K1/ND + 1
         CALL SRSTOP(1)
      ENDIF
      K2 = K1
      L2 = K1 + N1*NDM1
  500 K2 = K2 + NDM1
      F  = A(K2)/P
      A(K2) = F
      K3 = K1
      K4 = K2
      L4 = K2 + NUC
  600 K3 = K3 + 1
      K4 = K4 + 1
      A(K4) = A(K4) - F * A(K3)
      IF (K4.LT.L4) GOTO 600
      IF (K2.LT.L2) GOTO 500
      K1 = K1 + ND
      IF (K1.LT.L1) GOTO 400
C
C           ENTRY FOR SUBSTITUTION OPTION
C
  700 IF(IOPT.EQ.1) goto 9999  !   RETURN
 1000 CONTINUE
C
C           THE FORWARD SUBSTITUTION HAS ESSENTIALLY THE SAME
C           STRUCTURE
C
C           K1 = OUTER LOOP COUNTER LOWER TRIANGULAR MATRIX
C           K2 = INNER LOOP COUNTER LOWER TRIANGULAR MATRIX
C           L1 AND L2 ARE THE LOOP LIMITS
C           K5 = OUTER LOOP COUNTER MATRIX OF KNOWN VECTORS
C           K4 = ROW COUNTER IN RELATION TO K5
C           K3 = INNER LOOP AND ROW COUNTER MATRIX OF KNOWN VECTORS
C           L3 = ROW LIMIT FOR ONE SUBSTITUTION ELEMENT "F"
C           F  = SUBSTITUTION ELEMENT OF LOWER TRIANGULAR MATRIX
C
      K1 = - NUC
      L1 = K1 + NMLC*ND
      K5 = - M
 1100 K1 = K1 + ND
      K5 = K5 + M
      K2 = K1
      L2 = K1 + NLC*NDM1
      K3 = K5 + M
      L3 = K3
 1200 K2 = K2 + NDM1
      F  = A(K2)
      L3 = L3 + M
      K4 = K5
 1300 K3 = K3 + 1
      K4 = K4 + 1
      B(K3) = B(K3) - F * B(K4)
      IF (K3.LT.L3) GOTO 1300
      IF (K2.LT.L2) GOTO 1200
      IF (K1.LT.L1) GOTO 1100
      IF (NLC.EQ.1) GOTO 2000
C
C           THE COLLUMNS BECOME SHORTER, THE REST IS THE SAME
C
      N1 = NLC
      L1 = (N-2) * ND
 1400 K1 = K1 + ND
      K5 = K5 + M
      K2 = K1
      N1 = N1 - 1
      L2 = K1 + N1*NDM1
      K3 = K5 + M
      L3 = K3
 1500 K2 = K2 + NDM1
      F  = A(K2)
      L3 = L3 + M
      K4 = K5
 1600 K3 = K3 + 1
      K4 = K4 + 1
      B(K3) = B(K3) - F * B(K4)
      IF (K3.LT.L3) GOTO 1600
      IF (K2.LT.L2) GOTO 1500
      IF (K1.LT.L1) GOTO 1400
C
C
C         BACKWARD SUBSTITUTION
C
 2000 K1 = N*ND + NLC + 1
      L1 = K1 - NMUC*ND
      K5 = N*M +1
 2100 K1 = K1 - ND
      L5 = K5 - M
      F  = A(K1)
 2200 K5 = K5 - 1
      B(K5) = B(K5)/F
      IF (K5.GT.L5) GOTO 2200
      K2 = K1
      L2 = K1 - NUC * NDM1
      K3 = K5
      L3 = K5
      K6 = K5 + M
 2300 K2 = K2 - NDM1
      F = A(K2)
      L3 = L3 - M
      K4 = K6
 2400 K3 = K3 - 1
      K4 = K4 - 1
      B(K3) = B(K3) - F * B(K4)
      IF (K3.GT.L3) GOTO 2400
      IF (K2.GT.L2) GOTO 2300
      IF (K1.GT.L1) GOTO 2100
      IF (NUC.EQ.1) GOTO 2850
      N1 = NUC
      L1 = 2*ND
 2500 K1 = K1 - ND
      L5 = K5 - M
      F  = A(K1)
      N1 = N1 - 1
 2600 K5 = K5 - 1
      B(K5) = B(K5)/F
      IF (K5.GT.L5) GOTO 2600
      K2 = K1
      L2 = K1 - N1 * NDM1
      K3 = K5
      L3 = K5
      K6 = K5 + M
 2700 K2 = K2 - NDM1
      F = A(K2)
      L3 = L3 - M
      K4 = K6
 2800 K3 = K3 - 1
      K4 = K4 - 1
      B(K3) = B(K3) - F * B(K4)
      IF (K3.GT.L3) GOTO 2800
      IF (K2.GT.L2) GOTO 2700
      IF (K1.GT.L1) GOTO 2500
 2850 K1 = K1 - ND
      L5 = K5 - M
      F  = A(K1)
 2900 K5 = K5 - 1
      B(K5) = B(K5)/F
      IF (K5.GT.L5) GOTO 2900

 9999 if ( timon ) call timstop ( ithandl )
      RETURN
      END
