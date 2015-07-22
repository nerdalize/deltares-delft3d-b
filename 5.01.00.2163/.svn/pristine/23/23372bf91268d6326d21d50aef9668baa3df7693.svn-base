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
C    Time:       09:33
C    Program:    INPTDT.FOR
C    Version:    1.2
C    Programmer: Hans Los
C    Previous version(s):
C    1.1 -- 27 Dec 1989 -- 09:32 -- Operating System: DOS
C    1.0 -- 12 Dec 1989 -- 11:59 -- Operating System: DOS
C    0.0 -- 12 Dec 1989 -- 10:19 -- Operating System: DOS
C
C  *********************************************************************
C  *  INTEGER FUNCTION INPTDT TO READ CHARACTER STRINGS USING          *
C  *          INTERACTIVE ROUTINES IN "INTERACT TXTLIB"                *
C  *********************************************************************
C
      INTEGER FUNCTION INPTDT(PRMPT,TOKEN,LEN)
      INCLUDE 'ioblck.inc'
      CHARACTER*8 TOKEN
      INTEGER PRMPT,GETS,LEN,UPRCAS
C
      INPTDT = 0
    1 CONTINUE
      IF (GETS(LINE,POSIT,80,8,TOKEN,LEN) .EQ. 0) RETURN
      IF (IOFLAG .EQ. 1) CALL BLPROMPT(PRMPT,0)
    4 READ (INUNI,10,END=2) LINE
   10 FORMAT (10A8)
      IRC = UPRCAS (LINE,LINE,80)
      POSIT=1
      GO TO 1
    2 CONTINUE
      IF (INUNI.NE.5) GO TO 999
      REWIND (INUNI, ERR = 999)
      GO TO 4
 999  WRITE (IOU(6),1004) INUNI
1004  FORMAT (' Hit end of file on unit ',I5)
      STOP
      END
