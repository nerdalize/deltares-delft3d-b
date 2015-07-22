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
C Function PASTFN to get full file name and check its existence.
C-----------------------------------------------------------------------
      INTEGER FUNCTION PASTFN (PATH,NAME,EXT,FLNAM)
      INTEGER STOST,STOSH,GETS,WIPE,LENSTR,POSIT
      LOGICAL LEXIST
      CHARACTER FLNAM*(*),PATH*(*),NAME*(*),EXT*(*)
      CHARACTER*80 CNAME
C
C Get (maximum) lengths of character variables as declared in the main
C program. Get actual lengths.
C
      IRC = WIPE (CNAME,1,80)
      MAXF  = LEN (FLNAM)
      IF (MAXF .EQ. 1 .OR. MAXF .GT. 80) THEN
         PASTFN = 1
         GO TO 1000
      END IF
      MAXP  = LEN (PATH)
      IF (MAXP .EQ. 1 .OR. MAXP .GT. 80) THEN
         PASTFN = 2
         GO TO 1000
      END IF
      MAXN  = LEN (NAME)
      IF (MAXN .EQ. 1 .OR. MAXN .GT. 80) THEN
         PASTFN = 3
         GO TO 1000
      END IF
      MAXE  = LEN (EXT)
      LENPAT = LENSTR (PATH,MAXP)
      LENNAM = LENSTR (NAME,MAXN)
      LENEXT = LENSTR (EXT,MAXE)
C
C Construct (new) full file name.
C Use STOSH to concat the various parts of the full name.
C Use STOST to remove trailing characters.
C
      IRC = STOSH (PATH,1,LENPAT,CNAME,1,LEN1)
      IRC = STOSH (NAME,1,LENNAM,CNAME,LENPAT+1,LEN1)
      LEN1 = LENPAT + LENNAM
      LENNEW = 0
      IF (LENEXT .GT. 0) THEN
         IRC = STOSH ('.',1,1,CNAME,LEN1+1,LENNEW)
         IRC = STOSH (EXT,1,LENEXT,CNAME,LEN1+2,LENNEW)
      END IF
      IRC = STOST (CNAME,1,MAXF,FLNAM,LENNEW)
C
C Is this a valid file name?
C
      INQUIRE (FILE = CNAME, EXIST = LEXIST)
      IF (LEXIST) THEN
         PASTFN = 0
      ELSE
         PASTFN = 28
      END IF
1000  CONTINUE
      RETURN
      END
