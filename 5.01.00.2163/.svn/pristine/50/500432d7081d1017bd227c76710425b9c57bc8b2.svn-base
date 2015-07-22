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

C    Date:       14 Dec 1989
C    Time:       08:35
C    Program:    PRPLOT.FOR
C    Version:    1.2
C    Programmer: ??????
C    Previous version(s):
C    1.1 -- 14 Dec 1989 -- 08:02 -- Operating System: DOS
C    1.0 -- 12 Dec 1989 -- 12:18 -- Operating System: DOS
C    0.0 -- 12 Dec 1989 -- 10:19 -- Operating System: DOS
C
C  *********************************************************************
C  *      SUBROUTINE PRPLOT: INTERFACE BETWEEN BLOOM AND PRTPLOT       *
C  *********************************************************************
C
      SUBROUTINE PRPLOT(IPL,IPL3,OPL)
      INTEGER OPL
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt2.inc'
C
C  Rewind IPL1 or IPL2 and IPL3 to enable reading by the plot routines.
C  Put the number of variables (NV) to 10.
C
C
      NV = 2*NUNUCO+4
      REWIND IPL
      REWIND IPL3
C     CALL GRAPH(NV,NREP,IPL,IPL3,OPL)
      RETURN
      END
