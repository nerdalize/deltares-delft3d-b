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

C-----------------------------------------------------------------------
C Function to parse full filenames. Split after the final path
C delimiter. Split after "." for extension.
C-----------------------------------------------------------------------
      INTEGER FUNCTION PARSFN (LINEIN,PATH,NAME,EXT,LPATH,LNAM,LEXT)
      CHARACTER LINEIN*(*), PATH*(*), NAME*(*), EXT*(*)
      CHARACTER*80 TOKEN
      CHARACTER*2 DELIM
      CHARACTER*1 DELIM1
      CHARACTER*1 CLAST  , dot
      INTEGER GETS,SETDLM,STOS,POSIT,WIPE,LPOS(2),LPATH,LNAM,LEXT
      INTEGER LOCATE
      DATA DELIM /' \\'/
      DATA DELIM1/'\\'/
C
C Get (maximum) lengths of character variables as declared in the main
C program.
C
      MAXIN = LEN (LINEIN)
      IF (MAXIN .EQ. 1 .OR. MAXIN .GT. 80) THEN
         PARSFN = 1
         GO TO 1000
      END IF
      MAXP  = LEN (PATH)
      IF (MAXP .EQ. 1 .OR. MAXP .GT. 80) THEN
         PARSFN = 2
         GO TO 1000
      END IF
      MAXN  = LEN (NAME)
      IF (MAXN .EQ. 1 .OR. MAXN .GT. 80) THEN
         PARSFN = 3
         GO TO 1000
      END IF
      MAXE  = LEN (EXT)
C
C Wipe PATH and NAME (fill with blanks).
C
      IRC = WIPE (PATH,1,MAXP)
      IRC = WIPE (NAME,1,MAXN)
      IRC = WIPE (EXT,1,MAXE)
C
C Set delimiter to blank and backslash.
C
      IRC = SETDLM (DELIM,2,DELIM,1)
C
C Get token from line. Continue until there are no tokens left.
C Get the location of the delimiter BEFORE the last token: this
C signals the end of the path name.
C
      POSIT = 1
      LPOS(2) = POSIT
   10 CONTINUE
      LPOS(1) = LPOS(2)
      LPOS(2) = POSIT
      IF (GETS (LINEIN,POSIT,MAXIN,80,TOKEN,LENTOK) .EQ. 0) GO TO 10
      IRC = SETDLM (DELIM,1,DELIM,1)
      CLAST = LINEIN (POSIT-1:POSIT-1)
      IF (CLAST .EQ. DELIM1) THEN
         LPOS2 = LPOS(1)
         LNAM = 0
      ELSE
         LPOS2 = LPOS(1) - 1
      END IF
C
C Copy first part of LINEIN to PATH and the remaining part to NAME.
C Use GETS to remove initial and trailing blanks.
C
      IRC = STOS (LINEIN,1,LPOS2,PATH,LPATH)
      IRC = STOS (LINEIN,LPOS2+1,MAXIN,TOKEN,LNAM)
      IPOS = 1
      IF (GETS (PATH,IPOS,MAXP,MAXP,PATH,LPATH) .NE. 0) LPATH = 0
C
C Get name and extension, if there is one. Get the lengths.
C
      dot = '.'
      LOC = LOCATE (TOKEN,dot,LNAM,1)
      IF (LOC .EQ. 0) THEN
         LEXT = 0
         IPOS = 1
         IF (GETS (TOKEN,IPOS,80,8,NAME,LNAM) .NE. 0) LNAM = 0
      ELSE
         LOC = LOC - 1
         NAME = TOKEN (1:LOC)
         EXT = TOKEN (LOC+2:LNAM)
         LEXT = LNAM - LOC - 1
         IPOS = 1
         IRC = GETS (EXT,IPOS,MAXE,MAXE,EXT,LEXT)
         LNAM = LOC
      END IF
      PARSFN = 0
1000  CONTINUE
      RETURN
      END
