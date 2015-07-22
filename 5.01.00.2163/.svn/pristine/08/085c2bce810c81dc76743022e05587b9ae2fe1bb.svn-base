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

      SUBROUTINE DHUCAS( STR1 , STR2 , NOCHR )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED       : aug  1993  by Jan van Beek (DELWAQ style, origin?)
C
C     FUNCTION      : Subroutine to set a string in uppercase.
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     STR1    CHAR(*)   1         INPUT   string to be converted
C     STR2    CHAR(*)   1         OUTPUT  string in uppercase
C     NOCHR   INTEGER   1         INPUT   length string
C
C     Declaration of arguments
C
      INTEGER       NOCHR
      CHARACTER*(*) STR1  , STR2
C
C     Local declaration
C
      CHARACTER*1   CC
      CHARACTER*26  ALFABH, ALFABK
      SAVE          ALFABH, ALFABK
      DATA          ALFABH, ALFABK /
     &              'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
     &              'abcdefghijklmnopqrstuvwxyz'/
C
      STR2 = STR1
C
      DO 100 IC = 1,NOCHR
         CC = STR2(IC:IC)
         NN = INDEX( ALFABK , CC )
         IF ( NN .EQ. 0 ) GOTO 100
         STR2(IC:IC) = ALFABH(NN:NN)
  100 CONTINUE
C
      RETURN
      END
