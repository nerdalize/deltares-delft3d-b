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

      SUBROUTINE DLWQ60 ( DERIV  , CONC   , NOTOT  , NOSEG  , ITFACT ,
     *                    AMASS2 , ISYS   , NSYS   , DMPS   , INTOPT ,
     *                    ISDMP  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 3, 1988 by L.Postma
C
C     FUNCTION            : utility that scales the DERIV array after
C                           the user quality processes routine,
C                           especially for steady state computation.
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
C     CONC    REAL   NOTOT*NOSEG  IN/OUT  first order to be scaled
C     NOTOT   INTEGER     1       INPUT   total number of systems
C     NOSEG   INTEGER     1       INPUT   number of computational elems
C     ITFACT  INTEGER     1       INPUT   scale factor
C     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
C     ISYS    INTEGER     1       INPUT   system considered
C     NSYS    INTEGER     1       INPUT   number of systems to consider
C     IAFLAG  INTEGER     1       INPUT   if 1 then accumulation
C     DMPS    REAL        *       IN/OUT  dumped segment fluxes
C                                         if INTOPT > 7
C     INTOPT  INTEGER     1       INPUT   Integration suboptions
C     ISDMP   INTEGER  NOSEG      INPUT   pointer dumped segments
C
      use timers

      INTEGER     ISDMP(*)
      DIMENSION   DERIV(NOTOT,*), CONC(NOTOT,*), AMASS2(NOTOT,*),
     *            DMPS(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq60", ithandl )
C
C         loop accross deriv and conc
C
      DO 10 ISEG = 1    , NOSEG
         CONC  (ISYS,ISEG) = CONC  (ISYS,ISEG)/ITFACT
         DO 10 I    = ISYS , ISYS +NSYS-1
            DERIV (I,ISEG) = DERIV (I,ISEG)/ITFACT
            AMASS2(I,   2) = AMASS2(I,   2) + DERIV(I,ISEG)
   10 CONTINUE
C
      IF ( MOD(INTOPT,16) .GE. 8  ) THEN
         DO 30  ISEG = 1 , NOSEG
            IP = ISDMP(ISEG)
            IF ( IP .GT. 0 ) THEN
               I4 = (IP-1)*NOTOT
               DO 20 I    = ISYS , ISYS +NSYS-1
                  I5 = I4 + I
                  DMPS(I5)= DMPS(I5) + DERIV(I,ISEG)
   20          CONTINUE
            ENDIF
   30    CONTINUE
      ENDIF
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
