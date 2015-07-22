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

      SUBROUTINE RAATRA (NOSYS , NDMPQ , NORAAI, NTRAAQ, IORAAI,
     +                   NQRAAI, IQRAAI, IQDMP , DMPQ  , TRRAAI)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : march 1995 by Jan van Beek
C
C     FUNCTION            : Fills transport terms for raaien
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOSYS   INTEGER       1     INPUT   Total number of active substances
C     NDMPQ   INTEGER       1     INPUT   Number of dump exchanges
C     NORAAI  INTEGER       1     INPUT   Number of raaien
C     NTRAAQ  INTEGER       1     INPUT   Total number of exch. in raaien
C     IORAAI  INTEGER       *     INPUT   Output option for raai
C     NQRAAI  INTEGER       *     INPUT   Number of exchanges in raai
C     IQRAAI  INTEGER       *     INPUT   Exchanges in raai
C     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
C     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
C     TRRAAI  REAL NOTOT*NDMPAR*6 IN/OUT  Cummulative transport over raai
C
C     Declaration of arguments
C
      use timers

      INTEGER       NOSYS , NDMPQ , NORAAI, NTRAAQ
      INTEGER       IORAAI(*)             , NQRAAI(*)       ,
     +              IQRAAI(*)             , IQDMP(*)
      REAL          DMPQ(NOSYS,NDMPQ,*)   , TRRAAI(NOSYS,*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "raatra", ithandl )
C
C     Local declarations
C
C
C     Loop over the raaien
C
      ITEL1 = 0
      DO 100 IRAAI = 1 , NORAAI
C
C        the exchange contributes
C
         NQC  = NQRAAI(IRAAI)
         IOPT = IORAAI(IRAAI)
         DO 30 IQC = 1 , NQC
            ITEL1 = ITEL1 + 1
            IQ    = IQRAAI(ITEL1)
            IF ( IQ .GT. 0 ) THEN
               IPQ  = IQDMP(IQ)
               DO 10 ISYS = 1 , NOSYS
                  IF ( IOPT .EQ. 1 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)+
     +                                    DMPQ(ISYS,IPQ,1)  -
     +                                    DMPQ(ISYS,IPQ,2)
                  ELSEIF ( IOPT .EQ. 2 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)+
     +                                    DMPQ(ISYS,IPQ,1)
                  ELSEIF ( IOPT .EQ. 3 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)-
     +                                    DMPQ(ISYS,IPQ,2)
                  ENDIF
   10          CONTINUE
            ELSE
               IPQ  = IQDMP(-IQ)
               DO 20 ISYS = 1 , NOSYS
                  IF ( IOPT .EQ. 1 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)-
     +                                    DMPQ(ISYS,IPQ,1)  +
     +                                    DMPQ(ISYS,IPQ,2)
                  ELSEIF ( IOPT .EQ. 2 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)+
     +                                    DMPQ(ISYS,IPQ,2)
                  ELSEIF ( IOPT .EQ. 3 ) THEN
                     TRRAAI(ISYS,IRAAI) = TRRAAI(ISYS,IRAAI)-
     +                                    DMPQ(ISYS,IPQ,1)
                  ENDIF
   20          CONTINUE
            ENDIF
   30    CONTINUE
C
  100 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
