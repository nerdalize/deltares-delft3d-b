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
C  *********************************************************************
C  *     SUBROUTINE CLRSCR TO REFRESH THE TERMINAL SCREEN              *
C  *********************************************************************
C
      SUBROUTINE CLRSCR
      CHARACTER*1 CLEAR(4)
      INTEGER ICL
      DATA ICL /27/
      DATA CLEAR /' ','[','2','J'/
C
C Note: the data in ICL corresponds with HEX 1B.
C     DATA CLEAR2 /'1B','5B','32','4A'/
C
      CLEAR(1) = CHAR(ICL)
      WRITE (*,10) CLEAR
10    FORMAT (1X,4A1)
      RETURN
      END
