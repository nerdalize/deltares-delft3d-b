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

      SUBROUTINE DHCMOV ( CARRA1 , CARRA2 , NOTOT )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : march 1998 by Jan van Beek
C
C     FUNCTION            : moves NOTOT values from CARRA1 to CARRA2
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     CARRA1  CH*(*)    NOTOT     INPUT   array to be copied
C     CARRA2  CH*(*)    NOTOT     OUTPUT  array to copy to
C     NOTOT   INTEGER     1       INPUT   total number of entries
C
      INTEGER       NOTOT
      CHARACTER*(*) CARRA1(*) , CARRA2(*)
C
      DO I = 1,NOTOT
         CARRA2(I) = CARRA1(I)
      ENDDO
C
      RETURN
      END
