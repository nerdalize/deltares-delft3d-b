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

      SUBROUTINE DHSTOP ( IEXIT )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : June  '91  BY  Jan van Beek
C
C     FUNCTION           : stops execution if possible with return value
C
C*********************************************************************
C     System dependent routine
C     configuration
C
C     UNIX systems
C*********************************************************************
C
C     LOGICAL UNITS      : -
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     IEXIT   INTEGER    1         INPUT   return value
C     ---------------------------------------------------------
C
      INTEGER   IEXIT
C
C     Local
C
C     INTEGER*2 IEXI2
C
C     Close all files
C
      DO 10 ILUN = 10 , 99
         CLOSE ( ILUN )
 10   CONTINUE
C
C     Call C routine
C
      IF ( IEXIT .NE. 0 ) THEN
         WRITE(*,*) ' EXIT LEVEL: ',IEXIT
      ENDIF
      IEXI2 = IEXIT
      CALL STOP ( IEXI2 )
C
      END
