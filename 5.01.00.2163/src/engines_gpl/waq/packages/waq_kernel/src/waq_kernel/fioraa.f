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

      SUBROUTINE FIORAA (OUTVAL, NRVAR , TRRAAI, NORAAI, NOSYS )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : september 1995 by Jan van Beek
C
C     FUNCTION            : Fills output buffer OUTVAL for raaien
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     OUTVAL  REAL    NRVAR,*     OUTPUT  Values for vars on output grid
C     NRVAR   INTEGER       1     INPUT   Number of output vars
C     TRRAAI  REAL    NOSYS,*     INPUT   Tranport over raai for active substanc
C     NORAAI  INTEGER       1     INPUT   Number of raaien
C     NOSYS   INTEGER       1     INPUT   Number of parameters in TRRAAI
C
C     Declaration of arguments
C
      use timers

      INTEGER    NRVAR , NORAAI, NOSYS
      REAL       OUTVAL(NRVAR,*), TRRAAI(NOSYS,*)
C
C     Local
C
      PARAMETER ( RMISS = -999. )
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "fioraa", ithandl )
C
C     Copy values into output buffer
C
      DO 30 IRAAI = 1 , NORAAI
         DO 10 ISYS = 1 , NOSYS
            OUTVAL(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)
   10    CONTINUE
         DO 20 ISYS = NOSYS + 1 , NRVAR
            OUTVAL(ISYS,IRAAI) = RMISS
   20    CONTINUE
   30 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
