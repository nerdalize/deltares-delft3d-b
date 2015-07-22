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
C  *         SUBROUTINE SPINDI TO DETERMINE TYPE INDICES               *
C  *********************************************************************
C
C
      SUBROUTINE SPINDI(LSPIND)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'ioblck.inc'
C
C  Determine the number of the first and last type of each species.
C  Check whether the number of different names agrees with the total
C  number of species which has previously been read by the program.
C
      LSPIND = 0
      NGR = 0
      J = 0
   5  I = J + 1
  10  J = J + 1
      IF (J .GT. NUSPEC) GO TO 15
      IF (SPNAME(J) .EQ. SPNAME(I)) GO TO 10
  15  NGR = NGR + 1
      J=J-1
      IT2(NGR,1)=I
      IT2(NGR,2)=J
      GRNAME (NGR) = SPNAME (I)
      IF (J .EQ. NUSPEC) GO TO 20
      GO TO 5
C
C  Check the number of species.
C
  20  IF (NGR .EQ. NUECOG) RETURN
      LSPIND = 1
      WRITE (IOU(10),30) NGR,NUECOG
  30  FORMAT (' The number of different species names ',I2,/,' is not',
     1        ' consistent with the number of species ',I2,/,
     2        ' Execution terminates.')
      RETURN
      END
