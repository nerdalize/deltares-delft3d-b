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

C    Date:       23 Oct 1989
C    Time:       13:24
C    Program:    SETUNI   FORTRAN
C    Version:    1.0
C    Programmer: Hans Los
C    (c) 1989 Deltares Sektor W&M
C    Previous versions:
C    0.0 -- 28 Sep 1989 -- 20:01
C
C  *********************************************************************
C  *          SUBROUTINE TO SET I/O UNIT NUMBERS FOR BLOOM II          *
C  *********************************************************************
C
C  *********************************************************************
C  *      SPECIAL NZBLOOM PROGRAM VERSION                              *
C  *********************************************************************
C
C  This module determines ALL I/O units for BLOOM II.
C  For historic reasons this subroutine
C  is configured such that BLOOM II units will be number 51 and up,
C  with the exception of units 5 and 6, which are the default console
C  units.
C
      SUBROUTINE SETUNI
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'ioblck.inc'
C
C In ECOLUMN/NZBLOOM version:
C
      IOUX = 50
      DO 10 I = 1, 30
         IOU (I) = I + IOUX
10    CONTINUE
      IOU(5) = 5
      IOU(6) = 6
C
C NZBLOOM: change IOU(10) to 70.
C
      IOU(10) = 70
C
C In ECOLUMN/NZBLOOM version:
C
      IOUX = 40
      DO 20 I = 41, 45
         IOU (I) = I + IOUX
20    CONTINUE
C
C In ECOLUMN/NZBLOOM version:
C
      IOUX = 30
      DO 30 I = 61, 69
         IOU (I) = I + IOUX
30    CONTINUE
C
C  Initialize (old) unit names previously set in various other
C  subroutines of BLOOM II.
C
      INUNI  = IOU(9)
      OUUNI  = IOU(10)
      IPL1 = IOU(41)
      IPL2 = IOU(42)
      IPL3 = IOU(43)
      OPL  = IOU(45)
C
C In PC version: use the standard BLOOM II file OUUNI also for EKOBLM
C BLOOM II and DLWQWQ, which use IOU(61), IOU(62), IOU(6) and
C IOU(3).
C
      IOU(61) = OUUNI
      IOU(62) = OUUNI
      IOU(6)  = OUUNI
      IOU(3)  = OUUNI
      RETURN
      END
