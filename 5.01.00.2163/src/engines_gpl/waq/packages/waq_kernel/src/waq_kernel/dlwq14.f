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

      SUBROUTINE DLWQ14 ( DERIV  , NOTOT  , NOSEG  , ITFACT , AMASS2 ,
     *                    IDT    , IAFLAG , DMPS   , INTOPT , ISDMP  ,
     *                    OWNERS , MYPART )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : april 1988 by L.Postma
C
C     FUNCTION            : utility that scales the DERIV array after
C                           the user quality processes routine and
C                           accumulates if needed.
C
C     LOGICAL UNITS       : none
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
C     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
C     IDT     INTEGER     1       INPUT   integration time step size
C     IAFLAG  INTEGER     1       INPUT   if 1 then accumulation
C     DMPS    REAL        *       IN/OUT  dumped segment fluxes
C                                         if INTOPT > 7
C     INTOPT  INTEGER     1       INPUT   Integration suboptions
C     ISDMP   INTEGER  NOSEG      INPUT   pointer dumped segments
C     OWNERS  INTEGER  NOSEG      INPUT   ownership of segments
C     MYPART  INTEGER     1       INPUT   number of current part/subdomain
C
      use timers
      INTEGER     ISDMP(*)
      DIMENSION   DERIV(*)  ,  AMASS2(*)  , DMPS(*)
      INTEGER     OWNERS(NOSEG)
      INTEGER     MYPART
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq14", ithandl )
C
C         loop accross deriv
C
      ATFAC = 1.0/ITFACT
      DTFAC = IDT
      IF ( IAFLAG .EQ. 1 ) THEN
         K = 1
         DO  20  ISEG = 1 , NOSEG
            IF (OWNERS(ISEG).EQ.MYPART) THEN
               DO  10  I2 = NOTOT+1 , 2*NOTOT
                  DERIV (K ) = DERIV (K )*ATFAC
                  AMASS2(I2) = AMASS2(I2)+DERIV(K)*DTFAC
                  K = K + 1
   10          CONTINUE
            ELSE
               K = K + NOTOT
            ENDIF
   20    CONTINUE
      ELSE
         K = 1
         DO  40 ISEG = 1 , NOSEG
            IF (OWNERS(ISEG).EQ.MYPART) THEN
               DO  30 I2 = 1 , NOTOT
                  DERIV(K) = DERIV(K) * ATFAC
                  K = K + 1
   30          CONTINUE
            ELSE
               K = K + NOTOT
            ENDIF
   40    CONTINUE
      ENDIF
C
      IF ( MOD(INTOPT,16) .GE. 8  ) THEN
C
         I1 = 0
         I3 = 0
         DO 60 ISEG = 1 , NOSEG
            IP = ISDMP(ISEG)
            IF (OWNERS(ISEG).EQ.MYPART .AND. IP.GT.0) THEN
               I3 = (IP-1)*NOTOT
               DO 50 ISYS = 1 , NOTOT
                  I1 = I1 + 1
                  I3 = I3 + 1
                  DMPS(I3) = DMPS(I3) + DERIV(I1) * DTFAC
   50          CONTINUE
            ELSE
               I1 = I1 + NOTOT
            ENDIF
   60    CONTINUE
C
      ENDIF
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

