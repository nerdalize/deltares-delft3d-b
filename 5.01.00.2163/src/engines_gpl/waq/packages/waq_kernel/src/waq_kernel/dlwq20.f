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

      SUBROUTINE DLWQ20 ( CONC   , AMASS  , DERIV  , VOLUME , IDT    ,
     *                    NOSYS  , NOTOT  , NOSEG  , LUN    , IVFLAG )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:    april 1988 by L.Postma
C
C     FUNCTION            : Sets an preliminary step from DERIV.
C
C     LOGICAL UNITNUMBERS : LUN = number of monitoring file
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     CONC    REAL   NOTOT*NOSEG  OUTPUT  new concentrations
C     AMASS   REAL   NOTOT*NOSEG  IN/OUT  mass array to be updated
C     DERIV   REAL   NOTOT*NOSEG  INPUT   derivetives for time step
C     VOLUME  REAL      NOSEG     INPUT   segment volumes
C     IDT     INTEGER     1       INPUT   time step size in clock units
C     NOSYS   INTEGER     1       INPUT   number of active substances
C     NOTOT   INTEGER     1       INPUT   number of total substances
C     NOSEG   INTEGER     1       INPUT   number of computational elts.
C     IVFLAG  INTEGER     1       INPUT   = 1 then computed volumes
C
      use timers

      DIMENSION  CONC(*) , AMASS(*) , DERIV(*) , VOLUME(*)
      SAVE       IVMESS
      DATA       IVMESS /0/
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq20", ithandl )
C
C         loop accross the number of computational elements
C
      ITEL = 1
      DO 30 ISEG=1,NOSEG
C
C         compute volumes if necessary
C
      IF ( IVFLAG .EQ. 1 ) VOLUME(ISEG) = AMASS(ITEL) + IDT*DERIV(ITEL)
      V1 = VOLUME(ISEG)
      IF ( ABS(V1).LT.1.0E-25 ) THEN
         IF ( IVMESS .LT. 25 ) THEN
            IVMESS = IVMESS + 1
            WRITE ( LUN, 1000 ) ISEG  , V1
         ELSEIF ( IVMESS .EQ. 25 ) THEN
            IVMESS = IVMESS + 1
            WRITE ( LUN, 1001 )
         ENDIF
         VOLUME (ISEG) = 1.0
         V1            = 1.0
      ENDIF
C
C         active substances first
C
      DO 10 I=1,NOSYS
      A           = AMASS(ITEL) + IDT*DERIV(ITEL)
      CONC (ITEL) = A / V1
      DERIV(ITEL) = 0.0
      ITEL = ITEL+1
   10 CONTINUE
C
C         then the inactive substances
C
      DO 20 I=NOSYS+1,NOTOT
      A           = AMASS(ITEL) + IDT*DERIV(ITEL)
      CONC (ITEL) = A
      DERIV(ITEL) = 0.0
      ITEL = ITEL+1
   20 CONTINUE
C
C         end of the loop
C
   30 CONTINUE
      if ( timon ) call timstop ( ithandl )
      RETURN
C
C        output formats
C
 1000 FORMAT ( 'Volume of segment:', I7, ' is:',
     &          E15.6, ' 1.0 assumed.' )
 1001 FORMAT ('25 or more zero volumes , further messages surpressed')
C
      END
