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

      SUBROUTINE DLWQJ2 ( LUNWR  , NOBRK  , NOTOT  , ITAL   , IAR    ,
     *                                      RAR    , IFILSZ , JFILSZ )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : May '96  by L. Postma
C
C     MODIFIED           :
C
C     FUNCTION           : Writes blocks of breakpoint data
C
C     SUBROUTINES CALLED : none
C
C     LOGICAL UNITS      : LUNWR   = binary/unformatted work file
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     LUNWR   INTEGER     1       INPUT   unit number output work file
C     NOBRK   INTEGER     1       INPUT   nr of breakpoints to write
C     NOTOT   INTEGER     1       INPUT   size of one matrix of data
C     ITAL    INTEGER     1       INPUT   nr of integers per breakpoint
C     IAR     INTEGER     *       INPUT   breakpoint timers
C     RAR     REAL*4      *       INPUT   matrix storage
C     IFILSZ  INTEGER     1       IN/OUT  cumulative integer space count
C     JFILSZ  INTEGER     1       IN/OUT  cumulative real space count
C
C
      use timers       !   performance timers

      DIMENSION     IAR(*) , RAR(*)
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "dlwqj2", ithndl )
C
C           Write nr of breakpoints first
C
      WRITE ( LUNWR ) NOBRK
C
C           Initialize counters for the loop
C
      ITEL = 0
      JTEL = 0
      DO 10 I = 1 , NOBRK
         WRITE ( LUNWR ) ( IAR(ITEL+K) , K=1,ITAL  ) ,
     *                   ( RAR(JTEL+K) , K=1,NOTOT )
         ITEL = ITEL + ITAL
         JTEL = JTEL + NOTOT
   10 CONTINUE
C
C           Update the space count
C
      IFILSZ = IFILSZ + NOBRK*ITAL  + 1
      JFILSZ = JFILSZ + NOBRK*NOTOT
C
      if (timon) call timstop( ithndl )
      RETURN
      END
