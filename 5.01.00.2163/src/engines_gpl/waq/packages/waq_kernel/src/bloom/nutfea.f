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
C  *    SUBROUTINE TO CHECK FEASIBILITY OF NUTRIENT CONSTRAINTS        *
C  *********************************************************************
C
      SUBROUTINE NUTFEA(INFEAS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'ioblck.inc'
      DIMENSION X(MX)
      DATA SLACK/1.D-12/
C
C  If a negative righthand side is determined, check whther all species
C  have a positive A-coefficient or not; introduce slack to avoid
C  negative concentrations.
C
      INFEAS=0
      DO 40 I=1,NUNUCO
      IF (B(I) .GE. 0.0) GO TO 40
      DO 10 J=1,NUSPEC
      IF (AA(I,J) .LT. 1.0D-6) GO TO 20
   10 CONTINUE
      IF (IDUMP .EQ. 1) WRITE (IOU(6),99999) CSTRA(I),B(I)
      INFEAS=1
      GO TO 30
   20 CONTINUE
      IF (IDUMP .EQ. 1) WRITE (IOU(6),99990) CSTRA(I),B(I)
   30 B(I)=SLACK
   40 CONTINUE
99999 FORMAT (2X,'The nutrient constraint',1X,A8,1X,'has a negative',
     1        ' right hand side =',2X,F8.3,/,1X,'and positive',
     2        ' A-coefficients for all species; problem is infeasible.')
99990 FORMAT (2X,'The nutrient constraint',1X,A8,1X,'has a negative',
     1        ' right hand side =',2X,F8.3,/,1X,'and zero',
     2        ' A-coefficients for at least one species;',
     3        ' slack introduced.')
      RETURN
      END
