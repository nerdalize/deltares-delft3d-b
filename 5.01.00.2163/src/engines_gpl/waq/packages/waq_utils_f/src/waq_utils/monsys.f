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

      SUBROUTINE MONSYS(STRING,ILEVEL)
C
C     Arguments
C
      INTEGER       ILEVEL
      CHARACTER*(*) STRING
C
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
C
C
      IF ( ILEVEL .LE. MONLEV ) THEN
         WRITE( LUNMON , '(A)' ) STRING
      ENDIF
C
      RETURN
      END
      SUBROUTINE SETMMO (ILEVEL)
C
C     Arguments
C
      INTEGER       ILEVEL
C
C     Settings
C
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
C
C     Set monitoring level
C
      MONLEV = ILEVEL
C
      RETURN
      END
      SUBROUTINE GETMMO (ILEVEL)
C
C     Arguments
C
      INTEGER    ILEVEL
C
C     Settings
C
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
C
      ILEVEL = MONLEV
C
      RETURN
      END
      SUBROUTINE SETMLU (ILUNMO)
C
C     Arguments
C
      INTEGER       ILUNMO
C
C     Settings
C
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
C
C     Set monitoring level
C
      LUNMON = ILUNMO
C
      RETURN
      END
      SUBROUTINE GETMLU (ILUNMO)
C
C     Arguments
C
      INTEGER    ILUNMO
C
C     Settings
C
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
C
      ILUNMO = LUNMON
C
      RETURN
      END
      BLOCK DATA BDMON
C
      INTEGER         LUNMON,MONLEV
      COMMON / CMON / LUNMON,MONLEV
      SAVE   / CMON /
C
      DATA   LUNMON /   0 /
      DATA   MONLEV /   5 /
C
      END
