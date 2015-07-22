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
C  *        SUBROUTINE TO PRINT MAXIMUM SOLUTION ON TAPE IOU(6)        *
C  *********************************************************************
C
      SUBROUTINE PRINMA(X,BIO2,TOTAL,NI,NIN,INT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      DIMENSION X(MX)
C
C  PRINT MAXIMUM SOLUTION ON UNIT IOU(6)
C
      IF (IOFLAG .EQ. 1) CALL MORESC
      WRITE (IOU(6),10)
   10 FORMAT (12X,'******* MAXIMUM SOLUTION *******')
      WRITE(IOU(6),20)
   20 FORMAT (2X,'Species',34X,'Types',/,26X,'1',13X,'2',13X,'3',
     1        13X,'4')
      DO 60 J=1,NUECOG
      L1=IT2(J,1)
      L2=IT2(J,2)
      WRITE (IOU(6),50) GRNAME(J), (X(K+NUROWS),K=L1,L2)
   50 FORMAT (2X,A8,11X,4(F11.4,3X))
   60 CONTINUE
      BIOPOS = BIO2
      IF (BIOPOS .LT. 0.0) BIOPOS = 0.0
      WRITE (IOU(6),70) BIOPOS
   70 FORMAT (2X,'Total biomass',6X,F11.4,3X,'mgm/m3')
      WRITE (IOU(6),90) TOTAL
   90 FORMAT (2X,'Chlorophyll',8X,F11.4,3X,'mgm/m3',/)
C
C  PRINT NUTRIENT CONCENTRATIONS
C
      WRITE (IOU(6),100)
  100 FORMAT (2X,'Nutrient',14X,'Total',9X,'Slacks')
      WRITE (IOU(6),110) (CSTRA(K),CONCEN(K),X(K),K=1,NUNUCO)
  110 FORMAT (6(2X,A8,11X,F11.4,3X,F11.4,/))
      WRITE(IOU(6),120) NI,NIN,INT
  120 FORMAT ('  Number of intervals:',I3,2X,'Infeasible:',I3,
     1        2X,'Maximum interval:',I3,//)
      IF (IOFLAG .EQ. 1) CALL MORESC
      RETURN
      END
