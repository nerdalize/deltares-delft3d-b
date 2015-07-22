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
C Character function to obtain part of one string and put it into
C another string.
C The arguments of this function are similar to the EXEC2 function
C &PIECE OF.
C
      CHARACTER*(*) FUNCTION PIECE (SOURCE,FIRST,LENGTH)
      CHARACTER*1 SOURCE(1),BLANK
      INTEGER FIRST,LAST,LENGTH,STOS,WIPE
      DATA BLANK /' '/
C
C Use STOS the put the significant part of SOURCE into PIECE.
C The remainder of the string is filled with blanks by function WIPE.
C
      PIECE = BLANK
      LAST = FIRST + LENGTH - 1
      LENPI = LEN (PIECE)
      IF (LAST .GT. LENPI) LAST = LENPI
      IRC = STOS(SOURCE, FIRST, LAST, PIECE, LENOUT)
      IRC = WIPE(PIECE,LENOUT+1,LENPI)
      RETURN
      END
