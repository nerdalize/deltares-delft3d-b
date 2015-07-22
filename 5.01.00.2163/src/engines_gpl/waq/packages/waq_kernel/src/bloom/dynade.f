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
C  *    SUBROUTINE TO CALCULATE DEAD ALGAL POOLS DYNAMICALLY           *
C  *********************************************************************
C
      SUBROUTINE DYNADE(X,DEATH,INFEAS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      DIMENSION X(MX)
C
C  CHECK FOR FEASIBILITY
C
      IF (INFEAS .EQ. 0) GO TO 8
      DO 5 I=1,NUNUCO
    5 DETRIT(I)=DETRIT(I)*REMEXP(I)
      RETURN
C
C  FEASIBILE SOLUTION
C
    8 CONTINUE
      AVAND=DEATH*AVAILN
      DO 20 I=1,NUNUCO
      SUM=0.0
      DO 10 J=1,NUSPEC
   10 SUM=SUM+AA(I,J)*X(J+NUROWS)
      DETEQ=AVAND*SUM/REMINU(I)
      DETRIT(I)=DETEQ+(DETRIT(I)-DETEQ)*REMEXP(I)
   20 CONTINUE
      RETURN
      END
