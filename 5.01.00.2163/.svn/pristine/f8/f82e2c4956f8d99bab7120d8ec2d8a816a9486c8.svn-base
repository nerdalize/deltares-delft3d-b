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

      SUBROUTINE RDWRK3 ( LUN   , LCHAR ,ITOTA , ITOTI , ITOTC  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : march 93 by Jan van Beek
C
C     FUNCTION            : Reads DELWAQ boot file
C
C     LOGICAL UNITNUMBERS : LUN(1)   - DELWAQ boot file
C
C     SUBROUTINES CALLED  : DHOPNF, opens files
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUN     INTEGER    *         OUT     Logical unit numbers
C     LCHAR   CHAR*(*)   *         OUT     File names
C     ITOTA   INTEGER    1         OUT     Dimension real array
C     ITOTI   INTEGER    1         OUT     Dimension integer array
C     ITOTC   INTEGER    1         OUT     Dimension character array
C
C     declarations
C
      INTEGER       ITOTA , ITOTI , ITOTC
      INTEGER       LUN(*)
      CHARACTER*(*) LCHAR(*)
C
C     COMMON  /  SYSN   /   System characteristics
C
      INCLUDE 'sysn.inc'
C
C     COMMON  /  SYSI  /    Timer characteristics
C
      INCLUDE 'sysi.inc'
C
C     input structure for boot-file
C
      INTEGER             LUNIN
      DIMENSION           IN(INSIZE)       , II(IISIZE)
      EQUIVALENCE       ( IN(1)  , NOSEG ) , ( II(1), ITSTRT  )
C
C         boot the system
C
      LCHMAX = LEN(LCHAR(1))
      CALL DHGNAM(LCHAR(1),' ')
      INDX = INDEX ( LCHAR(1) , ' ' )
      IF ( INDX .EQ. 0 ) INDX = LCHMAX + 1
      LCHAR(1) = LCHAR(1)(1:INDX-1)//'-delwaq03.wrk'
      LUNIN    = 14
      CALL DHOPNF ( LUNIN , LCHAR(1), 1     , 2     , IERR  )
C
      READ  ( LUNIN )   IN
      READ  ( LUNIN )   II
      READ  ( LUNIN )  ITOTA  , ITOTI  , ITOTC
      READ  ( LUNIN ) ( LUN(K) , K = 1,NOLUN )
      READ  ( LUNIN ) ( LCHAR(K) , K=1 , NOLUN )
C
      CLOSE ( LUNIN )
C
      RETURN
      END
