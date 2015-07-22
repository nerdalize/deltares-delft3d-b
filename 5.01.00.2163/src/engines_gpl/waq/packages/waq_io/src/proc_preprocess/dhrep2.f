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

      SUBROUTINE DHREP2( NAME1 , NAME2 , CARRAY, NOCAR , NCHAR ,
     +                   RARRAY, RVALUE)
C
      use timers       !   performance timers

      INTEGER            NOCAR , NCHAR
      CHARACTER*(*)      NAME1 , NAME2
      CHARACTER*(*)      CARRAY(NOCAR)
      REAL               RVALUE
      REAL               RARRAY(NOCAR)
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dhrep2", ithndl )
C
C     Search and replace all occurences of NAME1 in CARRAY with NAME2
C     Also replace RARRAY with the same indexes with RVALUE
C
      DO I = 1 , NOCAR
         CALL ZOEK(NAME1,1,CARRAY(I),NCHAR,ICAR)
         IF ( ICAR .GT. 0 ) THEN
            CARRAY(I) = NAME2
            RARRAY(I) = RVALUE
         ENDIF
      ENDDO
C
      if (timon) call timstop( ithndl )
      RETURN
      END
