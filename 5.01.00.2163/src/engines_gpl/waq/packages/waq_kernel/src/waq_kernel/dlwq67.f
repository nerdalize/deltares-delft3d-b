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

      SUBROUTINE DLWQ67 ( AMAT   , NOSEG  , JTRACK )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: june 1988 by L.Postma
C
C     FUNCTION            : updates the diagonal if zero
C
C     LOGICAL UNITNUMBERS : none
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND       LENGTH       FUNCT.  DESCRIPTION
C     ----    -----      ------       ------- -----------
C     AMAT    REAL (JTRACK*2+1)*NOSEG IN/OUT  matrix to invert
C     NOSEG   INTEGER       1         INPUT   number of segments
C     JTRACK  INTEGER       1         INPUT   number of codiagonals
C
      use timers

      DIMENSION   AMAT(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq67", ithandl )
C
C         set the diagonal
C
      ISTEP = JTRACK*2 + 1
      ISET  = JTRACK + 1
      DO 10 ISEG = 1 , NOSEG
      IF ( ABS(AMAT(ISET)) .LT. 1.0E-35 ) AMAT(ISET) = 1.0
   10 ISET = ISET+ISTEP
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
