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

      SUBROUTINE BALDMP (NOTOT , NOSYS , NOFLUX, NDMPAR, NDMPQ ,
     +                   NDMPS , NTDMPQ, IQDMP , ISDMP , IPDMP ,
     +                   DMPQ  , MASS  , DMPS  , FLXDMP, ASMASS,
     +                   FLXINT)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : march 1995 by Jan van Beek
C
C     FUNCTION            : Fills balances for sub-area's
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          : 15
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOSYS   INTEGER       1     INPUT   Total number of active substances
C     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
C     NDMPAR  INTEGER       1     INPUT   Number of dump areas
C     NDMPQ   INTEGER       1     INPUT   Number of dump exchanges
C     NDMPS   INTEGER       1     INPUT   Number of dump segments
C     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
C     IQDMP   INTEGER       *     INPUT   Exchange to dumped exchange pointer
C     ISDMP   INTEGER       *     INPUT   Segment to dumped segment pointer
C     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
C     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
C     DMPS    REAL  NOTOT*NDMPS*? INPUT   mass balance dumped segments
C     FLXDMP  REAL  NOFLUX*NDMPS  INPUT   Integrated fluxes
C     ASMASS  REAL NOTOT*NDMPAR*6 OUTPUT  Mass balance terms
C     FLXINT  REAL  NOFLUX*NDMPAR OUTPUT  Integrated fluxes
C
C     Declaration of arguments
C
      use timers

      INTEGER       NOTOT , NOSYS , NOFLUX, NDMPAR, NDMPQ ,
     +              NDMPS , NTDMPQ
      INTEGER       IQDMP(*)              , ISDMP(*)        ,
     +              IPDMP(*)
      REAL          DMPQ(NOSYS,NDMPQ,*)   , MASS(NOTOT,*)   ,
     +              DMPS(NOTOT,NDMPS,*)   , FLXDMP(NOFLUX,*),
     +              ASMASS(NOTOT,NDMPAR,*), FLXINT(NOFLUX,*)
C
C     Local declarations
C
      INTEGER       ITEL1 , ITEL2 , IP1   , IDUMP , NQC   ,
     +              IQC   , IQ    , IPQ   , ISYS  , NSC   ,
     +              ISC   , ISEG  , IPS
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "baldmp", ithandl )

C
C     Loop over the dump area's
C
      ITEL1 = NDMPAR
      IP1   = NDMPAR + NTDMPQ
      ITEL2 = NDMPAR + NTDMPQ + NDMPAR
      DO 100 IDUMP = 1 , NDMPAR
C
C        the exchange contributes
C
         NQC = IPDMP(IDUMP)
         DO 30 IQC = 1 , NQC
            ITEL1 = ITEL1 + 1
            IQ    = IPDMP(ITEL1)
            IF ( IQ .GT. 0 ) THEN
               IPQ  = IQDMP(IQ)
               DO 10 ISYS = 1 , NOSYS
                  ASMASS(ISYS,IDUMP,5) = ASMASS(ISYS,IDUMP,5) +
     +                                   DMPQ(ISYS,IPQ,2)
                  ASMASS(ISYS,IDUMP,6) = ASMASS(ISYS,IDUMP,6) +
     +                                   DMPQ(ISYS,IPQ,1)
   10          CONTINUE
            ELSE
               IPQ  = IQDMP(-IQ)
               DO 20 ISYS = 1 , NOSYS
                  ASMASS(ISYS,IDUMP,5) = ASMASS(ISYS,IDUMP,5) +
     +                                   DMPQ(ISYS,IPQ,1)
                  ASMASS(ISYS,IDUMP,6) = ASMASS(ISYS,IDUMP,6) +
     +                                   DMPQ(ISYS,IPQ,2)
   20          CONTINUE
            ENDIF
   30    CONTINUE
C
C        the segment contributes
C
         DO ISYS = 1 , NOTOT
            ASMASS(ISYS,IDUMP,1) = 0.0
         ENDDO
         NSC = IPDMP(IP1+IDUMP)
         DO 60 ISC = 1 , NSC
            ITEL2 = ITEL2 + 1
            ISEG  = IPDMP(ITEL2)
            IF ( ISEG .GT. 0 ) THEN
               IPS   = ISDMP(ISEG)
               DO 40 ISYS = 1 , NOTOT
                  ASMASS(ISYS,IDUMP,1) = ASMASS(ISYS,IDUMP,1) +
     +                                   MASS(ISYS,ISEG)
                  ASMASS(ISYS,IDUMP,2) = ASMASS(ISYS,IDUMP,2) +
     +                                   DMPS(ISYS,IPS,1)
                  ASMASS(ISYS,IDUMP,3) = ASMASS(ISYS,IDUMP,3) +
     +                                   DMPS(ISYS,IPS,2)
                  ASMASS(ISYS,IDUMP,4) = ASMASS(ISYS,IDUMP,4) +
     +                                   DMPS(ISYS,IPS,3)
   40          CONTINUE
            ENDIF
C
   60    CONTINUE
C
  100 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
