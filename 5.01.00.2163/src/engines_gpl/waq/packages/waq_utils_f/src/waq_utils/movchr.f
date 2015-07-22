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

      SUBROUTINE MOVCHR ( CAR    , NSTRT  , NOTOT  )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : May '97    BY L. Postma
C
C     FUNCTION           : Shifts an array of characters one location
C
C     SUBROUTINES CALLED : none
C
C     LOGICAL UNITS      : none
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     CAR     CHAR*(*) NOMAX      IN/OUT  array to be shifted
C     NSTRT   INTEGER  1          INPUT   start of shift
C     NOTOT   INTEGER  1          IN/OUT  stop of shift
C
C
      CHARACTER*(*) CAR(*)
C
      DO 10 I=NOTOT,NSTRT,-1
         CAR(I+1) = CAR(I)
   10 CONTINUE
C
      RETURN
      END
