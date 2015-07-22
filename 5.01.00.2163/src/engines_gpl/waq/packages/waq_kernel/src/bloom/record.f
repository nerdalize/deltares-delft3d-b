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
C  *********************************************************************
C  *    SUBROUTINE TO CALCULATE THE INITIAL BIOMASSES FOR NEXT PERIOD  *
C  *********************************************************************
C
      SUBROUTINE RECORD(X,XINIT,EXTB,INFEAS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'arran.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'dynam.inc'
      DIMENSION X(*),XINIT(*)
      DATA BASELV /1.0/
C
C  If infeasible, put a baselevel for all groups for the next period.
C  If constant baselevels are used (LGBASE = 1) these are used.
C  If fractional baselevels are used (LGBASE = 0)
C  BASE is taken as 100 * time-step size of the model in weeks.
C
      IF (INFEAS .EQ. 0) GO TO 20
      BASE = BIOBAS
      IF (LGBASE .EQ. 0) BASE = 100.0 * TSTEP/7
      DO 10 I=1,NUECOG
   10 XINIT(I)=BASE
      RETURN
C
C  Feasibile solution; record calculated bloom level of each group
C  which is present in the bloom, or use baselevel.
C
   20 CONTINUE
      IF (LGBASE .EQ. 1) GO TO 50
C
C  Compute the baselevel for groups not present this step.
C
      DO 40 J=1,NUECOG
      L1 = IT2(J,1)
      L2 = IT2(J,2)
      ROOTMA = -1.0
      ISKMAX = L1
      DO 30 K=L1,L2
      IF (AROOT(2*K) .LT. ROOTMA) GO TO 30
      ROOTMA = AROOT(2*K)
      ISKMAX = K
   30 CONTINUE
      BASE = (BIOBAS * (AROOT(2*ISKMAX) - EXTB))  / A(NUABCO,ISKMAX)
      XINIT(J)=DMAX1(BASE,BASELV,X(J))
   40 CONTINUE
      RETURN
C
C  Use constant baselevel for species not present this step.
C
   50 DO 60 I=1,NUECOG
   60 XINIT(I)=DMAX1(BIOBAS,X(I))
      RETURN
      END
