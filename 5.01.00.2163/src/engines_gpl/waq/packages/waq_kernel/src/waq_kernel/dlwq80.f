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

      SUBROUTINE DLWQ80 ( DERIV  , NOTOT  , NOSEG  , ITFACT , TIMER  ,
     *                    AMASS  , AMASS2 , IAFLAG , DMPS   , INTOPT ,
     *                    ISDMP  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 3, 1988 by L.Postma
C
C     FUNCTION            : utility that scales the DERIV array after
C                           the user quality processes routine and
C                           accumulates if needed.
C
C     LOGICAL UNITNUMBERS : none
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     DERIV   REAL   NOTOT*NOSEG  IN/OUT  derivatives to be scaled
C     NOTOT   INTEGER     1       INPUT   total number of systems
C     NOSEG   INTEGER     1       INPUT   number of computational elems
C     ITFACT  INTEGER     1       INPUT   scale factor
C     TIMER   REAL   NOTOT*NOSEG  IN/OUT  timer accumulator
C     AMASS   REAL   NOTOT*NOSEG  INPUT   mass in the system
C     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
C     IAFLAG  INTEGER     1       INPUT   if 1 then accumulation
C     DMPS    REAL        *       IN/OUT  dumped segment fluxes
C                                         if INTOPT > 7
C     INTOPT  INTEGER     1       INPUT   Integration suboptions
C     ISDMP   INTEGER  NOSEG      INPUT   pointer dumped segments
C
      use timers

      INTEGER     ISDMP(*)
      DIMENSION   DERIV(*)  ,  TIMER(*) , AMASS(*) , AMASS2(*) ,
     *            DMPS(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq80", ithandl )
C
C         loop accross deriv
C
      DO  10 I1 = 1 , NOTOT*NOSEG
      DERIV(I1) = DERIV(I1)/ITFACT
      IF ( DERIV(I1) .LT. 0.0 ) TIMER(I1)=TIMER(I1)-DERIV(I1)/AMASS(I1)
   10 CONTINUE
      IF ( IAFLAG .EQ. 0 ) RETURN
C
C     total mass balance
C
      DO  20  I1 = 1 , NOTOT*NOSEG
      I2         = MOD   (I1-1,NOTOT) + NOTOT + 1
      AMASS2(I2) = AMASS2(I2) + DERIV(I1)
   20 CONTINUE
C
C     segment mass balance
C
      IF ( MOD(INTOPT,16) .GE. 8  ) THEN
         DO  40  ISEG = 1 , NOSEG
            IF ( ISDMP(ISEG) .GT. 0 ) THEN
               IP = (ISDMP(ISEG)-1) * NOTOT
               I1 = NOTOT*(ISEG-1)
               DO 30 ISYS = 1 , NOTOT
                  DMPS(IP+ISYS)=DMPS(IP+ISYS) + DERIV(I1+ISYS)
   30          CONTINUE
            ENDIF
   40    CONTINUE
      ENDIF
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
