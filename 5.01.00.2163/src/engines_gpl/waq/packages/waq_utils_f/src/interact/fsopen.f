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

C Integer function to open a series of files whose names are stored
C in a file caled FSOPEN.FIL. The current program version can handle
C only three tokens:
C 1. The unit number,
C 2. The file name,
C 3. The format of the file (optional).
C
      INTEGER FUNCTION FSOPEN ()
      CHARACTER*80  LINE
      CHARACTER*40  NAME, OPTIONS
      INTEGER FSUNIT, STOI, GETS, POSIT
      LOGICAL LEXIST
C
C Check if the file with the filedef information exists.
C
      FSOPEN = 0
      INQUIRE (FILE = 'FSOPEN.FIL', EXIST = LEXIST)
      IF ( .NOT. LEXIST) THEN
         FSOPEN = 1
         GO TO 100
      END IF
C
C Read a record form the filedefinition file. Tokenize it.
C
      OPEN (99, FILE = 'FSOPEN.FIL')
5     CONTINUE
      POSIT = 1
      READ (99, 10, END=100) LINE
10    FORMAT (A80)
C
C Get unit number.
C
      IF (STOI (LINE,POSIT,80,FSUNIT) .NE. 0) THEN
         FSOPEN = 2
         GO TO 100
      END IF
C
C Get file name.
C
      IF (GETS (LINE,POSIT,80,40,NAME,LENNAM) .NE. 0) THEN
         FSOPEN = 3
         GO TO 100
      END IF
C
C Get format (optional)
C Open the file.
C
      IF (GETS (LINE,POSIT,80,40,OPTIONS,LENOP) .EQ. 0) THEN
         OPEN (UNIT=FSUNIT, FILE= NAME, FORM=OPTIONS)
      ELSE
         OPEN (UNIT=FSUNIT, FILE= NAME)
      END IF
      GO TO 5
100   CONTINUE
      CLOSE (99, STATUS = 'DELETE')
      RETURN
      END
