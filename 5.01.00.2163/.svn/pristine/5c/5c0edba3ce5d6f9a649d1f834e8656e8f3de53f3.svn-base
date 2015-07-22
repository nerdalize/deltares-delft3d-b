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

      SUBROUTINE DLWQB6 ( CONC   , DERIV  , NOSEG  , NOTOT  , VOLOLD ,
     *                    IDT    , ISYS   , RHS    , NSYS   )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: august 1992 by L. Postma
C
C     FUNCTION            : define right hand side
C
C     LOGICAL UNITNUMBERS : none
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND       LENGTH       FUNCT.  DESCRIPTION
C     ----    -----      ------       ------- -----------
C     CONC    REAL     NOTOT*NOSEG    INPUT   concentrations
C     DERIV   REAL     NOTOT*NOSEG    INPUT   derivatives
C     NOSEG   INTEGER       1         INPUT   number of segments
C     NOTOT   INTEGER       1         INPUT   total number of systems
C     VOLOLD  REAL        NOSEG       INPUT   volumes at beginning of step
C     IDT     INTEGER       1         INPUT   timestep in scu's
C     ISYS    INTEGER       1         INPUT   first substance
C     RHS     REAL        NOSEG       OUTPUT  right hand side vector
C     NSYS    INTEGER       1         INPUT   number of substances
C
      use timers

      DIMENSION   CONC (*)  ,  DERIV(*)  ,  VOLOLD(*) ,  RHS(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqb6", ithandl )
C
C         set the right hand side
C
      DT = REAL(IDT)
      ISET = 1
      DO 10 ISEG = 1 , NOSEG
         I1 = NOTOT*(ISEG-1)
         DO 10 K1 = ISYS, ISYS-1+NSYS
            RHS(ISET) = DERIV(I1+K1) + VOLOLD(ISEG)*CONC(I1+K1)/DT
            ISET = ISET + 1
   10 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
