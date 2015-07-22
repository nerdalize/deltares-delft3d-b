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

C    Date:       14 Jan 1990
C    Time:       15:28
C    Program:    DELSTR.FOR
C    Version:    2.2
C    Programmer: Hans Los
C    Previous version(s):
C    2.1 -- 14 Jan 1990 -- 15:04 -- Operating System: DOS
C    2.0 -- 12 Jan 1990 -- 12:14 -- Operating System: DOS
C    2.0 -- 12 Jan 1990 -- 12:14 -- Operating System: DOS
C    1.0 -- 16 Aug 1988 -- 10:36 -- Operating System: DOS
C
C FORTRAN function to delimit a character string. This function is
C is used in Microsoft fortran routines calling HALO.
C Update 2.0: check if the string is already delimited. If so, leave
C alone and exit.
C
      INTEGER FUNCTION DELSTR (STRING, NEWSTR, MAXLEN)
      CHARACTER*(*)STRING, NEWSTR
      CHARACTER*80 TMPSTR
      CHARACTER*1  CHAR
      INTEGER MAXLEN, LENSTR, MAXOUT, WIPE, STOS
      DATA MAXOUT /78/
C
      DELSTR = 0
      LENS = LENSTR (STRING, MAXLEN)
      IF (LENS .GE. MAXOUT) THEN
         DELSTR = 1
         LENS = MAXOUT - 2
      END IF
      IF (LENS .GE. MAXLEN-2) THEN
         DELSTR = 2
         LENS = MAXLEN - 2
      END IF
      IRC = STOS (STRING, 1, 1, CHAR, LENC)
      IF (CHAR .EQ. '|') THEN
         NEWSTR (1:LENS) = STRING (1:LENS)
         RETURN
      END IF
C
C  Delimite the string.
C
      TMPSTR (1:1) = '|'
      TMPSTR (2:LENS+1) = STRING (1:LENS)
      TMPSTR (LENS+2:LENS+2) = '|'
      NEWSTR (1:LENS+2) = TMPSTR (1:LENS+2)
      IRC = WIPE (NEWSTR,LENS+3, MAXLEN)
      RETURN
      END
