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

      SUBROUTINE ZERCUM (NOTOT , NOSYS , NOFLUX, NDMPAR, NDMPQ ,
     +                   NDMPS , ASMASS, FLXINT, AMASS2, FLXDMP,
     +                   DMPQ  , DMPS  , NORAAI, IMFLAG, IHFLAG,
     +                   TRRAAI, IBFLAG, NOWST , WSTDMP)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : march 1993 by Jan van Beek
C
C     FUNCTION            : Zero's the accumulated balance array's
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          : 7
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     NOTOT   INTEGER       1     INPUT   Total number of substances
C     NOSYS   INTEGER       1     INPUT   Total number of active substances
C     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
C     NDMPAR  INTEGER       1     INPUT   Number of dump areas
C     NDMPQ   INTEGER       1     INPUT   Number of dump segments
C     NDMPS   INTEGER       1     INPUT   Number of dump exchanges
C     ASMASS  REAL NOTOT*NDMPAR*6 OUTPUT  Mass balance terms
C     FLXINT  REAL  NOFLUX*NDMPAR OUTPUT  Integrated fluxes
C     AMASS2  REAL     NOTOT*5    OUTPUT  mass balance whole system
C     FLXDMP  REAL  NOFLUX*NDMPS  INPUT   Integrated fluxes
C     DMPQ    REAL  NOSYS*NDMPQ*? INPUT   mass balance dumped exchange
C     DMPS    REAL  NOTOT*NDMPS*? INPUT   mass balance dumped segments
C     NORAAI  INTEGER       1     INPUT   Number of raaien
C     IMFLAG  LOGICAL       1     INPUT   If .T. then monitor step
C     IHFLAG  LOGICAL       1     INPUT   If .T. then history step
C     TRRAAI  REAL NOTOT*NDMPAR*6 IN/OUT  Cummulative transport over raai
C
C     Declaration of arguments
C
      use dlwq_mt3d_data
      use timers

      INTEGER       NOTOT , NOSYS , NOFLUX, NDMPAR, NDMPQ ,
     +              NDMPS , NORAAI
      REAL          ASMASS(*), FLXINT(*),
     +              AMASS2(*), FLXDMP(*),
     +              DMPQ(*)  , DMPS(*)  ,
     +              TRRAAI(*)
      LOGICAL       IMFLAG, IHFLAG
      integer                    :: nowst                 ! number of wasteloads
      real                       :: wstdmp(notot,nowst,2) ! accumulated wasteloads 1/2 in and out
C
C     Local declarations
C
      INTEGER      NOZERO
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "zercum", ithandl )
C
C     Zero all monitor ( and balance ) realted cummulative array's
C
      IF ( IMFLAG ) THEN
C
C        Zero ASMASS
C
         NOZERO = 6 * NOTOT * NDMPAR * IBFLAG
         CALL ZERO(ASMASS,NOZERO)
C
C        Zero FLXINT
C
         NOZERO = NDMPAR * NOFLUX * IBFLAG
         CALL ZERO(FLXINT,NOZERO)
C
C        Zero AMASS2
C
         NOZERO = 5 * NOTOT
         CALL ZERO(AMASS2,NOZERO)
C
C        Zero WSTDMP
C
         NOZERO = NOTOT * NOWST * 2
         CALL ZERO(WSTDMP,NOZERO)
C
      ENDIF
C
C     Zero FLXDMP
C
C     NOZERO = NDMPS  * NOFLUX
C     CALL ZERO(FLXDMP,NOZERO)
C
C     Zero all monitor .or. history realted
C
      IF ( IMFLAG .OR. IHFLAG ) THEN
C
C        Zero DMPQ
C
         NOZERO = NDMPQ  * 2 * NOSYS
         CALL ZERO(DMPQ  ,NOZERO)
         if (allocated(gsl_prev_inf)) gsl_prev_inf = 0.0
         if (allocated(gsl_prev_upw)) gsl_prev_upw = 0.0
C
C        Zero DMPS
C
         NOZERO = NDMPS  * 3 * NOTOT
         CALL ZERO(DMPS  ,NOZERO)
C
      ENDIF
C
C     Zero all history realted
C
      IF ( IHFLAG ) THEN
C
C        Zero TRRAAI
C
         NOZERO = NOSYS*NORAAI
         CALL ZERO(TRRAAI,NOZERO)
C
      ENDIF
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
