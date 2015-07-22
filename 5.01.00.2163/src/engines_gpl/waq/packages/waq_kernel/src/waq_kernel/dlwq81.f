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

      SUBROUTINE DLWQ81 ( WASTE  , IWASTE , NOWST  , NOTOT  , CONC   ,
     *                    DERIV  , TIMER  , VOLUME , AMASS2 , IAFLAG ,
     *                    DMPS   , NDMPS  , INTOPT , ISDMP  , NOSYS  )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: april 3, 1988 by L.Postma
C
C     FUNCTION            : Adds the wasteloads to DERIV.
C
C     LOGICAL UNITNUMBERS : none
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     WASTE   REAL  NOTOT+1*NOWST INPUT   waste masses per system clock
C     IWASTE  INTEGER    NOWST    INPUT   segment numbers of the wastes
C     NOWST   INTEGER     1       INPUT   number of wastes
C     NOTOT   INTEGER     1       INPUT   number of substances
C     CONC    REAL     NOTOT*?    INPUT   concentrations for withdrawals
C     DERIV   REAL     NOTOT*?    IN/OUT  derivative to be updated
C     TIMER   REAL     NOTOT*?    IN/OUT  time step accumulator
C     VOLUME  REAL      NOSEG     INPUT   segment volumes
C     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
C     IAFLAG  INTEGER     1       INPUT   if 1 then accumulation
C     DMPS    REAL  NOTOT*NDMPAR*?IN/OUT  dumped segment fluxes
C                                         if INTOPT > 7
C     NDMPS   INTEGER     1       INPUT   number of dumped segments
C     INTOPT  INTEGER     1       INPUT   Integration suboptions
C     ISDMP   INTEGER     *       INPUT   pointer dumped segments
C
      use timers

      INTEGER     INTOPT, NDMPS
      INTEGER     ISDMP(*)
      DIMENSION   WASTE (*) , IWASTE(*) , CONC(*)  , DERIV(*) ,
     *            VOLUME(*) , AMASS2(*) , TIMER (*), DMPS(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq81", ithandl )
      IF ( NOWST .EQ. 0 ) RETURN
C
C     pointers in mass balance array
C
      IF ( MOD(INTOPT,16) .GE. 8  ) THEN
         IBFLAG = 1
      ELSE
         IBFLAG = 0
      ENDIF
C
      I4 = NOTOT*NDMPS
      I5 = NOTOT*NDMPS*2
C
      ITEL = 1
      DO 40 I = 1 , NOWST
C
      I3 = ( IWASTE(I) - 1 ) * NOTOT
C
      IF ( IBFLAG .EQ. 1 ) THEN
         ISEG = IWASTE(I)
         IF ( ISDMP(ISEG) .GT. 0 ) THEN
            IPB = ISDMP(ISEG)
            IPQ = (ISDMP(ISEG)-1)*NOTOT
         ELSE
            IPB = 0
         ENDIF
      ELSE
         IPB = 0
      ENDIF
C
C         a load or a withdrawal with a flow?
C
      ALOAD = WASTE(ITEL)
      IF (     ALOAD  .LT.-1.0E-30 ) GOTO 20
      IF ( ABS(ALOAD) .LE. 1.0E-30 ) ALOAD = 1.0
C
      ITEL = ITEL + 1
      DO 10 I1=1,NOTOT
         AHLP = WASTE(ITEL)*ALOAD
         DERIV(I3+I1) = DERIV (I3+I1) + AHLP
C
C        accumulation ?
C
         IF ( IAFLAG .EQ. 1 ) THEN
            AMASS2(I1+2*NOTOT) = AMASS2(I1+2*NOTOT) + AHLP
            IF ( IPB .GT. 0  ) THEN
               IF ( AHLP .LT. 0.0 ) THEN
                  DMPS(IPQ+I1+I5)=DMPS(IPQ+I1+I5) - AHLP
               ELSE
                  DMPS(IPQ+I1+I4)=DMPS(IPQ+I1+I4) + AHLP
               ENDIF
            ENDIF
         ENDIF
C
         ITEL = ITEL + 1
   10 CONTINUE
      GOTO 40
C
   20 ITEL  = ITEL + 1
      DO 30 I1 = 1,NOTOT
         IF ( ABS(WASTE(I3+I1)) .LT. 1.0E-30 ) THEN
            IF ( I1 .LE. NOSYS ) THEN
               AHLP = CONC (I3+I1)*ALOAD
            ELSE
               AHLP = 0.0
            ENDIF
         ELSE
            AHLP = WASTE(I3+I1)*ALOAD
         ENDIF
         C = CONC(I3+I1)
         IF ( C .LT. 1.0E-30 ) C = 1.0
         TIMER (I3+I1) = TIMER (I3+I1)      - AHLP/C/VOLUME(IWASTE(I))
         DERIV (I3+I1) = DERIV (I3+I1)      + AHLP
C
C        accumulation ?
C
         IF ( IAFLAG .EQ. 1 ) THEN
            AMASS2(I1+2*NOTOT) = AMASS2(I1+2*NOTOT) + AHLP
            IF ( IPB .GT. 0  ) THEN
               IF ( AHLP .LT. 0.0 ) THEN
                  DMPS(IPQ+I1+I5)=DMPS(IPQ+I1+I5) - AHLP
               ELSE
                  DMPS(IPQ+I1+I4)=DMPS(IPQ+I1+I4) + AHLP
               ENDIF
            ENDIF
         ENDIF
         ITEL = ITEL + 1
   30 CONTINUE
C
   40 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
