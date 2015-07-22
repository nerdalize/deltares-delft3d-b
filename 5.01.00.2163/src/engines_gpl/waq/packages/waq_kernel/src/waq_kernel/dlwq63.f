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

      SUBROUTINE DLWQ63 ( CONC   , DERIV  , AMASS2 , NOSEG  , NOTOT  ,
     *                    ISYS   , NSYS   , DMPS   , INTOPT , ISDMP  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: june 1988 by L.Postma
C
C     FUNCTION            : derives concentrations from deriv
C                           zeros DERIV
C
C     LOGICAL UNITNUMBERS : none
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND       LENGTH       FUNCT.  DESCRIPTION
C     ----    -----      ------       ------- -----------
C     CONC    REAL     NOTOT*NOSEG    INPUT   first order term
C     DERIV   REAL     NOTOT*NOSEG    IN/OUT  right hand side matrix
C     AMASS2  REAL     NOTOT*5        IN/OUT  mass accumulation array
C     NOSEG   INTEGER       1         INPUT   number of segments
C     NOTOT   INTEGER       1         INPUT   total number of systems
C     ISYS    INTEGER       1         INPUT   system considered
C     NSYS    INTEGER       1         INPUT   number of systems to take
C     DMPS    REAL          *         IN/OUT  dumped segment fluxes
C                                             if INTOPT > 7
C     INTOPT  INTEGER     1       INPUT   Integration suboptions
C
C     ISDMP   INTEGER  NOSEG      INPUT   pointer dumped segments
C
      use timers

      INTEGER     ISDMP(*)
      DIMENSION   CONC(NOTOT,*) , DERIV(*) , AMASS2(NOTOT,*) ,
     *            DMPS(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq63", ithandl )
C
C         gets concentrations
C
      ISET = 1
      IF ( MOD(INTOPT,16) .LT. 8  ) THEN
         DO 10 ISEG = 1 , NOSEG
         DO 10 I = ISYS , ISYS+NSYS-1
            AMASS2(I,   2) = AMASS2( I, 2 ) + CONC (I,ISEG)*DERIV(ISET)
            CONC  (I,ISEG) = DERIV ( ISET )
            ISET = ISET+1
   10    CONTINUE
      ELSE
         DO 20 ISEG = 1 , NOSEG
            IP = ISDMP(ISEG)
            I4 = (IP-1)*NOTOT
            DO 20 I = ISYS , ISYS+NSYS-1
               AMASS2(I,   2) = AMASS2(I, 2) + CONC(I,ISEG)*DERIV(ISET)
               IF ( IP .GT. 0 ) THEN
                  DMPS(I4+I) = DMPS(I4+I) + CONC(I,ISEG)*DERIV(ISET)
               ENDIF
               CONC  (I,ISEG) = DERIV (ISET)
               ISET = ISET+1
   20    CONTINUE
      ENDIF
C
C         zero the derivative
C
      NTOT = NOTOT*NOSEG
      DO 30 I = 1 , NTOT
   30 DERIV(I) = 0.0
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
