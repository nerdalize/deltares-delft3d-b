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

C    Date:       27 Dec 1989
C    Time:       09:32
C    Program:    INPTNM.FOR
C    Version:    1.1
C    Programmer: Hans Los
C    Previous version(s):
C    1.0 -- 12 Dec 1989 -- 11:58 -- Operating System: DOS
C    0.0 -- 12 Dec 1989 -- 10:19 -- Operating System: DOS
C
C  *********************************************************************
C  *  INTEGER FUNCTION INPTNM TO READ VARIABLES USING INTERACTIVE      *
C  *              ROUTINES IN "INTERACT TXTLIB"                        *
C  *********************************************************************
C
      INTEGER FUNCTION INPTNM(PRMPT,DNUM,INUM,TYPE)
      INCLUDE 'ioblck.inc'
      REAL*8 DNUM
      INTEGER PRMPT,TYPE,STOI,STOR,STOD,UPRCAS
C
C  TYPE INDICATES TYPE OF VARIABLE:
C    TYPE = 1: DOUBLE PRECISSION REAL
C    TYPE = 2: INTEGER
C    TYPE = 3: SINGLE PRECISSION REAL
C
      INPTNM = 0
1     CONTINUE
      GO TO (10,20,30), TYPE
      STOP
   10 IF (STOD(LINE,POSIT,80,DNUM) .EQ. 0) RETURN
      GO TO 2
   20 IF (STOI(LINE,POSIT,80,INUM) .EQ. 0) RETURN
      GO TO 2
   30 IF (STOR(LINE,POSIT,80,RNUM) .EQ. 0) RETURN
    2 IF (IOFLAG .EQ. 1) CALL BLPROMPT(PRMPT,0)
    5 READ(INUNI,100,END=3) LINE
100   FORMAT (10A8)
      IRC = UPRCAS (LINE,LINE,80)
      POSIT=1
      GO TO 1
    3 CONTINUE
      IF (INUNI.NE.5) GO TO 999
      REWIND (INUNI, ERR = 999)
      GO TO 5
 999  WRITE (IOU(6),1004) INUNI
1004  FORMAT (' Hit end of file on unit ',I5)
      STOP
      END
