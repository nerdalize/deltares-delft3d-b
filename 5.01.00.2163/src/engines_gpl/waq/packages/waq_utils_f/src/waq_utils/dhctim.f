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

      SUBROUTINE DHCTIM ( ITIME , ITIME2, DTFLG , DTFLG3 )
C
C
C     Deltares
C
C     CREATED       : 1996 BY Jan van Beek
C
C     FUNCTION      : Conversion of an integer variable in seconds to
C                     DD:HH:MM:SS or YY:DDD:HH format
C
C     LOGICAL UNITS : none
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     ITIME   INTEGER  1           INPUT   TIME in seconds
C     ITIME2  INTEGER  1           OUTPUT  TIME in format
C     DTFLG   LOGICAL  1           INPUT   if .TRUE. then 'date'-format
C     DTFLG3  LOGICAL  1           INPUT   (F;ddmmhhss,T;yydddhh)
C
C     Declaration of arguments
C
      INTEGER   ITIME , ITIME2
      LOGICAL   DTFLG , DTFLG3
C
      IF ( DTFLG ) THEN
         IF ( DTFLG3 ) THEN
            IYEAR   = ITIME/31536000
            IHLP    = ITIME - IYEAR*31536000
            IDAY    = IHLP/86400
            IHLP    = IHLP  - IDAY*86400
            IHOUR   = IHLP/3600
            ITIME2  = IYEAR*100000 + IDAY*100 + IHOUR
         ELSE
            IDAY    = ITIME/86400
            IHLP    = ITIME  - IDAY*86400
            IHOUR   = IHLP/3600
            IHLP    = IHLP   - IHOUR*3600
            IMIN    = IHLP/60
            ISEC    = IHLP   - IMIN*60
            ITIME2  = IDAY*1000000 + IHOUR*10000 + IMIN*100 + ISEC
         ENDIF
      ENDIF
C
      RETURN
      END
