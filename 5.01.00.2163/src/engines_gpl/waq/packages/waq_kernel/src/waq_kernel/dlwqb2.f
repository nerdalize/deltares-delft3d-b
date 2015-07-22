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

      SUBROUTINE DLWQB2 ( CONC   , RHS    , NOSEG  , NOTOT  , ISYS   ,
     *                    NSYS   )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: june 1988 by L.Postma
C
C     FUNCTION            : puts solution from RHS in CONC, zeros RHS
C
C     LOGICAL UNITNUMBERS : none
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND       LENGTH       FUNCT.  DESCRIPTION
C     ----    -----      ------       ------- -----------
C     CONC    REAL     NOTOT*NOSEG    IN/OUT  old/new concentration
C     RHS     REAL        NOSEG       IN/OUT  right hand side matrix
C     NOSEG   INTEGER       1         INPUT   number of segments
C     NOTOT   INTEGER       1         INPUT   total number of systems
C     ISYS    INTEGER       1         INPUT   system considered
C     NSYS    INTEGER       1         INPUT   number of systems
C
      DIMENSION   CONC(*) , RHS(*)
C
C         put result in concentration array
C
      ISET = 1
      DO 10 ISEG = 1 , NOSEG
      I1 = (ISEG-1)*NOTOT
      DO 10 K1 = ISYS, ISYS+NSYS-1
      CONC (I1+K1) = RHS ( ISET )
      RHS ( ISET ) = 0.0
      ISET = ISET + 1
   10 CONTINUE
C
      RETURN
      END
