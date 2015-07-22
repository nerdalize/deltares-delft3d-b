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

      LOGICAL FUNCTION DHRMIS (VALUE)
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED       : june  1993 BY J.K.L. van Beek
C
C     FUNCTION      : Checks if a real value is "missing"
C
C     LOGICAL UNITS      : none
C
C     PARAMETERS         : 1
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     VALUE   REAL        1       INPUT   Value to be checked
C
      REAL    VALUE
C
C     Local declaration
C
      PARAMETER ( RMISS = -999. )
C
C     Check
C
      IF ( VALUE .EQ. RMISS ) THEN
         DHRMIS = .TRUE.
      ELSE
         DHRMIS = .FALSE.
      ENDIF
C
      RETURN
      END
