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

      SUBROUTINE WRWRK3 ( LUN   , LCHAR ,ITOTA , ITOTI , ITOTC  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : march 93 by Jan van Beek
C
C     FUNCTION            : Writes DELWAQ2 boot file
C
C     LOGICAL UNITNUMBERS : LUN(1)  - DELWAQ boot file
C
C     SUBROUTINES CALLED  : SRSTOP, stops execution
C                           DHOPNF, opens files
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     LUN     INTEGER    *         IN      Logical unit numbers
C     LCHAR   CHAR*(*)   *         IN      File names
C     ITOTA   INTEGER    1         IN      Dimension real array
C     ITOTI   INTEGER    1         IN      Dimension integer array
C     ITOTC   INTEGER    1         IN      Dimension character array
C
C     declarations
C
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
      DIMENSION           IN(INSIZE)       , II(IISIZE)
      EQUIVALENCE       ( IN(1)  , NOSEG ) , ( II(1), ITSTRT  )
C
C     Local declarations
C
      INTEGER       IERR
C
C     write the boot file
C
      CALL DHOPNF ( LUN(1) , LCHAR(1), 1     , 1     , IERR  )
C
      WRITE ( LUN(1) )   IN
      WRITE ( LUN(1) )   II
      WRITE ( LUN(1) )   ITOTA , ITOTI , ITOTC
      WRITE ( LUN(1) ) ( LUN  (K) , K = 1, NOLUN )
      WRITE ( LUN(1) ) ( LCHAR(K) , K = 1, NOLUN )
C
      CLOSE ( LUN(1) )
C
      RETURN
      END
