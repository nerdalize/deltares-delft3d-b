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

C    Date:       4 Nov 1992
C    Time:       14:25
C    Program:    MAXMOR.FOR
C    Version:    1.0
C    Programmer: Hans Los
C    Previous version(s):
C    0.0 -- 6 Jun 1989 -- 10:38 -- Operating System: DOS
C
C  *********************************************************************
C  *  SUBROUTINE TO CALCULATE THE REMAINING BIOMASSES FOR NEXT PERIOD  *
C  *  MvdV 961014 added subtraction of mortality due to grazing GRAMOR *
C  *  from mortality constraint                                        *
C  *********************************************************************
C
      SUBROUTINE MAXMOR(X,MI,EXTLIM,INFEAS,GRAMOR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'dynam.inc'
      DIMENSION X(*),B2(MS),GRAMOR(MT)

C  MvdV 961014 added

C
C To (re)initialize the model, put all mortality constraints to 0.0
C Set EXTLIM  = 0.0
C If NREP = 1 also set X(I), I = NUROWS+1, NUROWS+NUSPEC to 0.0.
C
      IF (NREP .EQ. 1) THEN
         I1 = NUROWS
         J = NUEXRO + NUECOG
         DO 10 I = 1,NUECOG
            DO 5 K=IT2(I,1),IT2(I,2)
               I1 = I1 + 1
    5          X(I1) = 0.0D0
            J = J + 1
            B(J) = 0.0D0
   10    CONTINUE
         EXTLIM = 0.0
         RETURN
      END IF
C
      IF (INFEAS .EQ. 1) THEN
         J = NUEXRO + NUECOG
         DO 15 I = 1,NUECOG
            J = J + 1
            B(J) = 0.0D0
   15    CONTINUE
         EXTLIM = 0.0
         RETURN
      END IF
C
C Compute the mortality of each phytoplankton type, the total biomass
C of each species AFTER applying the mortality, and compute the
C extinction of living phytoplankton and detritus.
C The new, minimum biomass levels are stored in B2 AND in the original
C X-vector. This is to enable the program to deal with infeasible
C solutions.
C
C  Update nov 4 1992: don't divide by SDMIX. Questionable for species
C  with buoyancy regulation; definitaly incorrect for species at the
C  bottom.
C

C  MvdV 961014 added GRAMOR to RMORT to subtract mortality due to grazing
C              from the mortality constraint
      I1 = NUROWS
      EXDEAD = 0.0
      EXLIVE = 0.0
      DO 40 I=1,NUECOG
         SUMSP = 0.0
         DO 30 K=IT2(I,1),IT2(I,2)
            XDELT = 0.0
            I1 = I1 + 1
            IF (X(I1) .LT. 1.D-6) GO TO 20
            XDELT = X(I1) * DEXP(-MI * TSTEP * (RMORT(K) + GRAMOR(K)))
            SUMSP = SUMSP + XDELT
            EKXI = EKX (K) * XDELT
            EXDEAD = EXDEAD + QMREM * RMORT(K) * EKXI
*           EXLIVE = EXLIVE + EKXI/SDMIX(K)
            EXLIVE = EXLIVE + EKXI
   20       X(NUROWS + K) = XDELT
   30    CONTINUE
         B2(I) = DMAX1(SUMSP,0.0D0)
   40 CONTINUE
C
C Summarize the extinction of detritus and living phytoplankton
C obtaining EXTLIM: the minimum value of the planktonic extinction
C in the next time-step.
C
      EXTLIM = EXDEAD + EXLIVE
C
C Store the minimum biomass levels in the B-vector; if they have
C fallen below the toplevel (TOPLEV), release the mortality constraint.
C
      I1 = NUEXRO + NUECOG
      DO 50 I = 1,NUECOG
         I1 = I1 + 1
         IF (B2(I) .GT. TOPLEV) THEN
            B(I1) = B2(I)
         ELSE
            B(I1) = 0.0
         END IF
   50 CONTINUE
      RETURN
      END
