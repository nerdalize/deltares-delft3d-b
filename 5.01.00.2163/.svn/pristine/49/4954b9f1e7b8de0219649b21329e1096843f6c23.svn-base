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
C Alternative version of INTERACT function TOKEN.
C This functions returns additional parameters: the datatype
C of each token and the total length of the original string.
C Datatype = 0 for numeric tokens and 1 for character (and mixed)
C tokens.
C
      INTEGER FUNCTION ARGS (SOURCE, LENGTH, MAXOUT, RESULT, DATYPE,
     *                       NTOKEN, LENSTR)
      CHARACTER*(*) SOURCE, RESULT
      character*(1) BLANK
      CHARACTER*255 CTOKEN
      INTEGER LENGTH, MAXOUT, NTOKEN, GETS, STOR, STOSH, POS, SPOS,
     *        RPOS, DATYPE(1), LENSTR
      DATA BLANK /' '/
C
C Perform initial checks. Initialize variables.
C
      IF (LENGTH .LE. 0) GO TO 300
      IF (MAXOUT .LE. 0 .OR. MAXOUT .GT. 255) GO TO 400
      POS = 1
      LENSTR = 0
C
C Major program loop. Use GETS to obtain the next token. Use STOR
C to determine whether it is numeric or not. Store the result at
C in the appropriate positions of RESULT using function STOSH.
C
      DO 10 I = 1,NTOKEN
      IF (GETS (SOURCE, POS, LENGTH, MAXOUT, CTOKEN, LENOUT) .NE. 0)
     *    GO TO 20
      SPOS = 1
      IF (STOR (CTOKEN,SPOS,LENOUT,RNUM) .EQ. 0) THEN
          DATYPE(I) = 0
      ELSE
          DATYPE(I) = 1
      END IF
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
      LENSTR = POS - 1
      ARGS = 0
      RETURN
100   CONTINUE
      NTOKEN = 0
      ARGS = -2
      RETURN
200   CONTINUE
      NTOKEN = 0
      ARGS = -1
      RETURN
300   CONTINUE
      NTOKEN = 0
      ARGS = 2
      RETURN
400   CONTINUE
      NTOKEN = 0
      ARGS = 3
      RETURN
500   CONTINUE
      NTOKEN = 0
      ARGS = 4
      RETURN
      END
