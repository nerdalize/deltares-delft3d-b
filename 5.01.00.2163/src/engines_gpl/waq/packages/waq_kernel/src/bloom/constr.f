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
C  *    SUBROUTINE TO DETERMINE LIMITS ON THE EXTINCTION COEFFICIENT   *
C  *********************************************************************
C
      SUBROUTINE CONSTR(SURF,DMIX,EMIN,ROOT,NUMGR)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(2),B(2),ROOT(2)
      INCLUDE 'blmdim.inc'
      INCLUDE 'arran.inc'
C
C  DETERMINES LIMITS ON THE EXTINCTION COEFFICIENT
C
    1 ROOT(1)=-1.0
      ROOT(2)=-1.0
      S0=DEXP(-ZVEC(NZ))
      IF (SURF .LE. S0) RETURN
C
C  FIND INTERVAL CONTAINING ALL ROOTS
C
      X=-DLOG(SURF)
      CALL EBCALC(X,FX,DERX,NUMGR)
      PHI=(FUN(NZ,NUMGR)-FX)/EMIN
      Y=X+PHI
      IF (DERX .GE. EMIN) ROOT(1)=0.0
      IF (Y .GE. ZVEC(NZ)) ROOT(2)=PHI/DMIX
      IF (ROOT(1) .GE. 0.0 .AND. ROOT(2) .GE. 0.0) RETURN
C
C  SPLIT INTERVAL TO ISOLATE EACH ROOT.
C
      B(1)=X
      B(2)=Y
      DO 110 K=1,10
      A(1)=0.5*(B(1)+B(2))
      CALL EBCALC(A(1),FM,FPR,NUMGR)
      EBAR=(FM-FX)/(A(1)-X)
      IF (EBAR .GE. EMIN) GO TO 120
      DERIV=(FPR-EBAR)/(A(1)-X)
      IF (DERIV .LT. 0.0) B(2)=A(1)
  110 IF (DERIV .GE. 0.0) B(1)=A(1)
C
C  NO ROOTS EXIST
C
      RETURN
C
C  WE HAVE SEPARATED THE ROOTS.
C
  120 A(2)=A(1)
      DO 130 I=1,2
      IF (ROOT(I) .GE. 0.0) GO TO 130
      DO 125 K=1,10
      XM=0.5*(A(I)+B(I))
      CALL EBCALC(XM,FM,FP,NUMGR)
      EBAR=(FM-FX)/(XM-X)
      IF (EBAR .GT. EMIN) A(I)=XM
  125 IF (EBAR .LE. EMIN) B(I)=XM
      ROOT(I)=(0.5*(A(I)+B(I))-X)/DMIX
  130 CONTINUE
      RETURN
      END
