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

C    Date:       7 Dec 1989
C    Time:       13:05
C    Program:    HDRBLM   FORTRAN
C    Version:    1.3
C    Programmer: Hans Los
C    Previous version(s):
C    1.2 -- 24 Oct 1989 -- 08:27 -- Operating System: CMS
C    1.1 -- 24 Oct 1989 -- 08:25
C    1.0 -- 23 Oct 1989 -- 13:20
C    0.0 -- 3 Oct 1989 --  8:22
C
C  *********************************************************************
C  *  SUBROUTINE TO CONVERT UNITS BETWEEN BLOOM II AND ECOLUMN         *
C  *********************************************************************
C
C  *********************************************************************
C  *      SPECIAL ECOLUMN - BLOOM II PROGRAM VERSION                   *
C  *********************************************************************
C
C  This module writes the headers for BLOOM II output files if
C  LPRINT = 1. Otherwise no BLOOM II specific output should be
C  produced.
C
      SUBROUTINE HDRBLM
      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'sumout.inc'
C
C  Write heading for output on units IOU(6), OUUNI, IOU(15) and
C  optionally IOU(21).
C
      CALL FORMFE (OUUNI)
      WRITE (OUUNI,99999) IYEAR,CASE
      WRITE (OUUNI,99990) COM
      CALL FORMFE (IOU(14))
      WRITE (IOU(14),99999) IYEAR,CASE
      WRITE (IOU(14),99990) COM
      IF (IPERM .LE. 1) GO TO 50
      CALL FORMFE (IOU(15))
      WRITE (IOU(15),99999) IYEAR,CASE
      WRITE (IOU(15),99990) COM
   50 IF ( IOFLAG .EQ. 0) GO TO 60
      CALL FORMFE (IOU(21))
      WRITE (IOU(21),99999) IYEAR,CASE
      WRITE (IOU(21),99990) COM
   60 IF ( IDUMP .EQ. 0) GO TO 70
      CALL FORMFE (IOU(6))
      WRITE (IOU(6),99999) IYEAR,CASE
      WRITE (IOU(6),99990) COM
   70 CONTINUE
C
C Formats this subroutine.
C
99999 FORMAT (1X,'YEAR',1X,I4,3X,13A8)
99990 FORMAT (3X,9A8)
      RETURN
      END
