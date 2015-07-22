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
C Fortran version of INTERACT function TOKEN.
C
      INTEGER FUNCTION TOKEN (SOURCE, LENGTH, MAXOUT, RESULT, NTOKEN)
      CHARACTER*(*) SOURCE, RESULT
      character*(1) BLANK
      CHARACTER*132 CTOKEN
      INTEGER LENGTH, MAXOUT, NTOKEN, GETS, STOSH, POS, RPOS
      DATA BLANK /' '/
C
C Perform initial checks. Initialize variables.
C
      IF (LENGTH .LE. 0) GO TO 300
      IF (MAXOUT .LE. 0 .OR. MAXOUT .GT. 132) GO TO 400
      POS = 1
C
C Major program loop. Use GETS to obtain the next token. Store the
C result at the appropriate positions of RESULT using function STOSH.
C
      DO 10 I = 1,NTOKEN
      IF (GETS (SOURCE, POS, LENGTH, MAXOUT, CTOKEN, LENOUT) .NE. 0)
     *    GO TO 20
      RPOS = 1 + MAXOUT*(I-1)
      DO 5 J = 1,MAXOUT
      RESULT(RPOS+J:rpos+j) = BLANK
5     CONTINUE
      IRC = STOSH (CTOKEN, 1, LENOUT, RESULT, RPOS, LENRES)
10    CONTINUE
C
C The number of tokens equals its maximum NTOKEN. If there are still
C more tokens, exit with a non-zero return code.
C
      IF (GETS (SOURCE, POS, LENGTH, MAXOUT, CTOKEN, LENOUT) .EQ. 0)
     *    GO TO 100
20    CONTINUE
C
C All tokens have been handeled. Perform checks for error conditions.
C If all is ok, write blanks into the remaining part of RESULT, store
C the length of the original string and set the return code to 0.
C
      IF (I .EQ. 1 .AND. POS .EQ. LENGTH) GO TO 200
      NTOKEN = I - 1
      IF (NTOKEN .EQ. 0) GO TO 500
      RPOS = RPOS + LENRES
      DO 30 J = RPOS,LENGTH
      RESULT(J:j) = BLANK
30    CONTINUE
      TOKEN = 0
      RETURN
100   CONTINUE
      NTOKEN = 0
      TOKEN = -2
      RETURN
200   CONTINUE
      NTOKEN = 0
      TOKEN = -1
      RETURN
300   CONTINUE
      NTOKEN = 0
      TOKEN = 2
      RETURN
400   CONTINUE
      NTOKEN = 0
      TOKEN = 3
      RETURN
500   CONTINUE
      NTOKEN = 0
      TOKEN = 4
      RETURN
      END
