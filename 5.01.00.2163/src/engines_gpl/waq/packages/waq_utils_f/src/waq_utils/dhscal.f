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

      SUBROUTINE DHSCAL ( ARRAY1, NOVAL , RSCALE )
C
C     Deltares           SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     Created : June 1994 by Jan van Beek
C
C     Function            : Scales a real array.
C
C     Parameters          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ARRAY1  REAL     *          IN/OUT  Array to be scaled
C     NOVAL   INTEGER  1          INPUT   Number of value's to be scaled
C     RSCALE  REAL     1          INPUT   Scale factor
C
C     Declaration of arguments
C
      INTEGER    NOVAL
      REAL       RSCALE
      REAL       ARRAY1(*)
C
C     Local declaration
C
C     IVAL    INTEGER  1          LOCAL   Loop counter on NOVAL
C
      INTEGER    IVAL
C
      DO 100 IVAL = 1 , NOVAL
         ARRAY1(IVAL) = ARRAY1(IVAL) * RSCALE
  100 CONTINUE
C
      RETURN
      END
