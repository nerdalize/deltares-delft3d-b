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

      SUBROUTINE CURSOR (CURLIN,CURCOL)
      INTEGER IESCAP,CURLIN,CURCOL
      CHARACTER*1 ESCAPE
      CHARACTER*2 HBSLSH
      DATA IESCAP /27/
C
      ESCAPE = CHAR (IESCAP)
      HBSLSH = 'H\\'
      IF (CURLIN .LT. 10) THEN
         IF (CURCOL .LT. 10) THEN
            WRITE (*,10) ESCAPE,CURLIN,CURCOL,HBSLSH
   10       FORMAT (1X,A1,'[',I1,';',I1,A2)
         ELSE
            WRITE (*,20) ESCAPE,CURLIN,CURCOL,HBSLSH
   20       FORMAT (1X,A1,'[',I1,';',I2,A2)
          END IF
      ELSE
         IF (CURCOL .LT. 10) THEN
            WRITE (*,30) ESCAPE,CURLIN,CURCOL,HBSLSH
   30       FORMAT (1X,A1,'[',I2,';',I1,A2)
         ELSE
            WRITE (*,40) ESCAPE,CURLIN,CURCOL,HBSLSH
   40       FORMAT (1X,A1,'[',I2,';',I2,A2)
         END IF
      END IF
      RETURN
      END
