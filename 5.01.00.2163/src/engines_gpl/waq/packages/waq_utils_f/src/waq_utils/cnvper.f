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

      SUBROUTINE CNVPER ( CHULP  , IHULP  , DTFLG1 , DTFLG3 , IERR   )
C
C
C     Deltares        SECTOR MARINE AND COASTAL MANAGEMENT
C
C     CREATED            : August '02 by Jan van Beek
C
C     MODIFIED           :
C
C     FUNCTION           : Detects standard timer string period
C                          converts to system timer
C
C     SUBROUTINES CALLED : none
C
C     LOGICAL UNITS      : none
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     CHULP   CHAR*(*)   1         INPUT   string to be analised
C     IHULP   INTEGER    1         OUTPUT  system timer to get out
C     DTFLG1  LOGICAL    1         INPUT   TRUE if date format
C     DTFLG3  LOGICAL    1         INPUT   TRUE if HH instead of SS
C     IERR    INTEGER    1         OUTPUT  = 1 string is no timer
C
C     IN THE COMMON BLOCK:
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     block sysi.inc
C     ITSTRT  INTEGER    1         INPUT   Simulation start time ( scu )
C     ITSTOP  INTEGER    1         INPUT   Simulation stop time ( scu )
C     ISFACT  INTEGER    1         INPUT   system clock in seconds
C     OTIME   REAL*8     1         INPUT   Julian offset of the real time
C
C
      CHARACTER*(*) CHULP
      REAL*8        OTIM2  , OTIM3  , JULIAN , AFACT
      LOGICAL       DTFLG1 , DTFLG3
      CHARACTER*20  KEY
C
C     COMMON  /  SYSI   /   System timers
C
      INCLUDE 'sysi.inc'
C
      IERR = 1
      IF ( CHULP( 5: 5) .NE. '/' .OR. CHULP( 8: 8) .NE. '/' .OR.
     *     CHULP(11:11) .NE. '-' .OR. CHULP(14:14) .NE. ':' .OR.
     *     CHULP(17:17) .NE. ':' ) RETURN
      READ ( CHULP( 1: 4) , '(I4)' ) IYEAR
      READ ( CHULP( 6: 7) , '(I2)' ) IMONTH
      READ ( CHULP( 9:10) , '(I2)' ) IDAY
      READ ( CHULP(12:13) , '(I2)' ) IHOUR
      READ ( CHULP(15:16) , '(I2)' ) IMINUT
      READ ( CHULP(18:19) , '(I2)' ) ISECND
C
      ISEC   = IYEAR*31536000 + IMONTH*2592000+IDAY*86400+
     +         IHOUR*3600+IMINUT*60+ISECND
      IF ( ISFACT .LT. 0 ) THEN
         IHULP = -ISEC*ISFACT
      ELSE
         IHULP = ISEC/ISFACT
      ENDIF
C
      IF ( DTFLG3 ) THEN
         IHULP =  IHULP/3600
         IHULP = (IHULP/8760)*100000 + (MOD(IHULP,8760)/24)*100
     *                               +  MOD(IHULP,24)
      ELSE IF ( DTFLG1 ) THEN
         IHULP = (IHULP/86400)*1000000 + (MOD(IHULP,86400)/3600)*10000
     *         + (MOD(IHULP,3600)/60)*100 + MOD(IHULP,60)
      ENDIF
      IERR = 0
      RETURN
      END
