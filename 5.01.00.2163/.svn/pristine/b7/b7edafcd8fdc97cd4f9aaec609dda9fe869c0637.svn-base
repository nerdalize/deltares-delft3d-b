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

      SUBROUTINE DHISYS ( ISYSI , ISYSN )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : march 97 by Jan van Beek
C
C     FUNCTION            : Initialise constant array from common block
C
C     LOGICAL UNITNUMBERS : -
C
C     SUBROUTINES CALLED  : -
C
C     PARAMETERS          : -
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ISYSI   INTEGER       *     OUTPUT  copy of the SYSI common block
C     ISYSN   INTEGER       *     OUTPUT  copy of the SYSI common block
C
C     declarations
C
      INTEGER       ISYSI(*), ISYSN(*)
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
      INTEGER             IN(INSIZE)       , II(IISIZE)
      EQUIVALENCE       ( IN(1)  , NOSEG ) , ( II(1), ITSTRT  )
C
C     Fill the array's
C
      CALL DHIMOV( II    , ISYSI , IISIZE )
      CALL DHIMOV( IN    , ISYSN , INSIZE )
C
      RETURN
      END
