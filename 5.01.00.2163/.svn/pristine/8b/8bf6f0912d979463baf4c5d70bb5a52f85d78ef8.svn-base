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
C Character function to concat two strings into a new string.
C The arguments of this function are similar to the EXEC2 function
C &CONCAT OF.
C
      CHARACTER*(*) FUNCTION CONCAT (WORD1,WORD2,MAXLE1,MAXLE2)
      CHARACTER*(*) WORD1,WORD2
      character*1   DUMMY
      INTEGER MAXLE1, MAXLE2, LENSTR, STOSH
C
C Obtain the lengths of WORD1 and WORD2.
C Use STOSH to write all significant characters into CONCAT.
C
      CONCAT = DUMMY
      LEN1 = LENSTR (WORD1,MAXLE1)
      LEN2 = LENSTR (WORD2,MAXLE2)
      IRC = STOSH(WORD1, 1, LEN1, CONCAT, 1, LENOUT)
      IRC = STOSH(WORD2, 1, LEN2, CONCAT, LENOUT+1, LENTOT)
      RETURN
      END
