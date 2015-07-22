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
C  *         SUBROUTINE TO PERFORM SPECIAL INTERPOLATION               *
C  *********************************************************************
C
      SUBROUTINE EBCALC(X,F,FPR,NUMGR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'arran.inc'
C
C  CHECK WHETHER X IS TOO LOW OR TOO HIGH
C
      IF (X .GT. ZVEC(1)) GO TO 20
      F=FUN(1,NUMGR)
      FPR=0.0
      RETURN
   20 IF (X .LT. ZVEC(NZ)) GO TO 40
      F=FUN(NZ,NUMGR)
      FPR=0.0
      RETURN
   40 DO 60 I=2,NZ
      IF (X .LE. ZVEC(I)) GO TO 80
   60 CONTINUE
   80 EX=DEXP(-X)
      EI=DEXP(-ZVEC(I))
      EI1=DEXP(-ZVEC(I-1))
      ALAM=(EX-EI)/(EI1-EI)
      FPR=ALAM*DER(I-1,NUMGR)+(1.0-ALAM)*DER(I,NUMGR)
      C0=((EX/EI1)-1.0+X-ZVEC(I-1))/((EX/EI1)-1.0)
      C1=((EI1/EX)-1.0+ZVEC(I-1)-X)/((EI1/EX)-1.0)
      F=FUN(I-1,NUMGR)-C0*FPR+C1*DER(I-1,NUMGR)
      RETURN
      END
