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

C    Date:       4 Jan 1994
C    Time:       19:05
C    Program:    PRINT6.FOR
C    Version:    1.2
C    Programmer: Hans Los
C    Previous version(s):
C    1.1 -- 12 Feb 1993 -- 08:36 -- Operating System: DOS
C    PRINT6 FORTRAN -- 14 Jun 1991 -- 10:46 -- Operating System: DOS
C    0.0 -- 14 Jun 1991 --  8:53 -- Operating System: CMS
C
C    Update 1.2: removed storage limiting factors in ISPLIM.
C    Update 1.1: added JNOW to paramter list and replaced SOME
C    occurences if INOW by JNOW
C
C  *********************************************************************
C  *   SUBROUTINE TO PRINT SOLUTIONS FOR ALL FEASIBILITY INTERVALS     *
C  *         DETERMINE AND RECORD THE MAXIMUM SOLUTION                 *
C  *********************************************************************
C
      SUBROUTINE PRINT6(BIO,BIOMAX,X,XDEF,INOW,JNOW,LINF,IRS,INT,NIN,
     1           NONUNI,NONUN,NUMUNI,NUMUN,LIB)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'size.inc'
      INCLUDE 'sumout.inc'
      INTEGER JT(MT),NONUNI(*),NONUN(*),IRS(*),LIB(*)
      DIMENSION X(*),XDEF(*),BIO(*)
      DATA NPAUSE /0/
C
C  Initialize XOPT if this is the first interval for a time period.
C  If DUMP is specified, then print solution for interval JNOW.
C
      IF (INOW .EQ. 1) XOPT = 0.0D0
      IF (IDUMP .EQ. 0) GO TO 40
      IF (INOW .EQ. 1 .OR. NPAUSE .GE. 20) THEN
         NPAUSE = 0
      IF (IOFLAG .EQ. 1) CALL MORESC
      END IF
      WRITE (IOU(6),10) JNOW,(B(II),II=NUFILI,NUABCO)
   10 FORMAT (30X,'Interval ',I2,/,2X,'Extinction limits',3X,
     1        2(3X,F8.4))
      KLX=0
      DO 20 JJJ=1,NUSPEC
      JT(JJJ)=0
      IF (A(NUEXRO,JJJ) .GT. 1.0 - 1.0D-6) GO TO 20
      KLX=KLX+1
      JT(KLX)=JJJ
   20 CONTINUE
      WRITE (IOU(6),30) (JT(JJJ),JJJ=1,KLX)
   30 FORMAT(2X,'Types permitted',4X,20I4)
   40 CONTINUE
C
C  Check for feasibility of interval JNOW.
C  if LINF ne 0, exit after increasing NIN with 1
C
      IF (LINF .EQ. 0) GO TO 180
      IF (INOW .EQ. 1 .AND. LMORCH .EQ. 0) GO TO 50
      IF (IDUMP .EQ. 0) GO TO 170
   50 NPAUSE = NPAUSE + 5
      WRITE (IOU(6),60) JNOW
   60 FORMAT ('  Error message issued for interval ',I2,':')
      GO TO ( 70, 90, 110, 130, 150), IRS(2)
   70 WRITE (IOU(6),80)
   80 FORMAT('  Solution is feasible, but not yet optimal')
      GO TO 170
   90 WRITE (IOU(6),100)
  100 FORMAT('  A feasible solution in not yet obtained')
      GO TO 170
  110 WRITE (IOU(6),120)
  120 FORMAT('  A finite solution does not exist, solution is ',
     1       'unbounded')
      GO TO 170
  130 WRITE (IOU(6),140) IRS(3)
  140 FORMAT('  A feasible solution does not exist due to row ',I2)
      GO TO 170
  150 WRITE (IOU(6),160)
  160 FORMAT('  A finite solution can not be found',/,
     1       '  all elements of a prospective pivot column are zero')
  170 NIN = NIN + 1
      RETURN
C
C  Solution for interval JNOW is feasible.
C  Determine maximum biomass and record in BIO(1)
C
  180 CONTINUE
      BIO(1)=BIOMAX
C
C  Print solution for interval JNOW, if DUMP was specified.
C
      IF (IDUMP .EQ. 0) GO TO 290
      NPAUSE = NPAUSE + 10
      WRITE (IOU(6),190) (X(II),II=1,NUNUCO)
  190 FORMAT (2X,'Nutrient Slacks',2X,6(F8.2,2X))
      WRITE (IOU(6),200) (X(II),II=NUFILI,NUABCO)
  200 FORMAT (4X,'Energy Slacks',2X,2(F8.2,2X))
C
C  Print slacks for (optional) growth constraints.
C
      IF (LGROCH .EQ. 0) GO TO 250
      II1 = NUEXRO - 4
      II2 = II1 + 5
      II2MAX = NUEXRO + NUECOG
  210 II1 = II1 + 5
      II2 = II2 + 5
      II2 = MIN0(II2,II2MAX)
      WRITE (IOU(6),220) (X(II),II=II1,II2)
  220 FORMAT (4X,'Growth slacks',2X,10(F8.2,2X))
      IF (II2 .LT. II2MAX) GO TO 210
C
C  Print slacks for (optional) mortality constraints.
C
      IF (LMORCH .EQ. 0) GO TO 250
      II1 = NUEXRO + NUECOG- 4
      II2 = II1 + 5
      II2MAX = NUEXRO + 2 * NUECOG
  230 II1 = II1 + 5
      II2 = II2 + 5
      II2 = MIN0(II2,II2MAX)
      WRITE (IOU(6),240) (X(II),II=II1,II2)
  240 FORMAT (3X,'Mortal. slacks',2X,10(F8.2,2X))
      IF (II2 .LT. II2MAX) GO TO 230
C
C Print type biomasses and the optimum of the solution.
C
  250 CONTINUE
      II1 = NUSPE1 - 5
      II2 = NUSPE1 - 1
  260 II1 = II1 + 5
      II2 = II2 + 5
      II2 = MIN0(II2,NUCOLS)
      WRITE (IOU(6),270) (X(II),II=II1,II2)
  270 FORMAT (2X,'Types   ',5(F8.2,2X))
      IF (II2 .LT. NUCOLS) GO TO 260
      WRITE (IOU(6),280) BIOMAX,X(NUCOLS+1)
  280 FORMAT (2X,'Total biomass',2X,F8.2,2X,'Optimum',2X,F8.2)
  290 CONTINUE
C
C  Compare optimum solution of JNOW to the absolute optimum of ALL
C  previous intervals recorded in XOPT, and exit if XOPT is larger.
C
C  Update 14 june 1991.
C  Use a truely small number rather than 0.01 to determine whether
C  solutions are unique. Never check the first interval.
C
      DBIO=X(NUCOLS+1) - XOPT
      IF (INOW .EQ. 1) GO TO 310
      IF (DBIO .LT. -1.0D-6) RETURN
      IF (DBIO .GT.  1.0D-6) GO TO 310
C
C  BIO(1) is equal to BIO(2):
C  two intervals have (approximately) the same total biomass.
C  Record solution in XST, maximum in BIOST,
C  and the interval number in INTST.
C
      LST=1
      BIOST=BIO(1)
      XOPT = X(NUCOLS+1)
      XDEF(NUCOLS+2) = BIO(2)
      INTST=JNOW
      DO 300 K=1,NUCOLS+1
  300 XST(K)=X(K)
      RETURN
C
C  BIO(1) is larger than BIO(2):
C  Record solution in XDEF, maximum in BIO(2), the number of types
C  with 0.0 reduced cost in NUMUN and the types with reduced costs
C  of 0.0 in NONUM(20). Put the interval number in INT.
C
  310 CONTINUE
      LST=0
      BIO(2)=BIO(1)
      XOPT = X(NUCOLS+1)
      XDEF(NUCOLS+2) = BIO(2)
      INT=JNOW
      DO 320 K=1,NUCOLS+1
  320 XDEF(K)=X(K)
C
C Update 1.2: removed storage limiting factors in ISPLIM.
C Implemented new agorithm in PRINT6
C
      NUMUN = NUMUNI
      IF (NUMUN .EQ. 0) RETURN
      DO 350 K=1,NUMUN
  350 NONUN(K)=NONUNI(K)
      RETURN
      END
