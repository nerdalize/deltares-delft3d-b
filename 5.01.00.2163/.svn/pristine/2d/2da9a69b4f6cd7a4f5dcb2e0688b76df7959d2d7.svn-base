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

      SUBROUTINE DLWQB7 ( DISP   , DISPER , AREA   , FLOW   , ALENG  ,
     *                    VELO   , BOUND  , IPOINT , NOTOT  , ISYS   ,
     *                    NSYS   , NOQ1   , NOQ2   , NOQ    , NODISP ,
     *                    NOVELO , IDPNT  , IVPNT  , DERIV  , IOPT   ,
     *                                                        ILFLAG )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED: august  1992 by L.Postma
C
C     FUNCTION            : Fills only derivative for transport
C
C     LOGICAL UNITNUMBERS : none
C
C     SUBROUTINES CALLED  : none
C
C     PARAMETERS          :
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     DISP    REAL        3       INPUT   dispersion in 3 directions
C     DISPER  REAL   NODISP*NOQ   INPUT   additional dispersion array
C     AREA    REAL       NOQ      INPUT   exchange surface area
C     FLOW    REAL       NOQ      INPUT   flows accross exchange surfs
C     ALENG   REAL      2*NOQ     INPUT   from- and to lengthes
C     VELO    REAL   NOVELO*NOQ   INPUT   additional velocity array
C     BOUND   REAL     NOTOT*?    INPUT   boundary concentrations
C     IPOINT  INTEGER   4*NOQ     INPUT   exchange pointers
C     NOTOT   INTEGER     1       INPUT   number  of active substances
C     ISYS    INTEGER     1       INPUT   system number considered
C     NSYS    INTEGER     1       INPUT   number of systems considered
C     NOQ1    INTEGER     1       INPUT   nr of exchanges in first dir.
C     NOQ2    INTEGER     1       INPUT   nr of exchanges in second dir.
C     NOQ     INTEGER     1       INPUT   total number of exchanges
C     NODISP  INTEGER     1       INPUT   number of additional dispers.
C     NOVELO  INTEGER     1       INPUT   number of additional velos.
C     IDPNT   INTEGER   NOSYS     INPUT   pointer systems to dispersions
C     IVPNT   INTEGER   NOSYS     INPUT   pointer systems to velocities
C     DERIV   REAL   NOTOT*NOSEG  OUTPUT  derivatives
C     IOPT    INTEGER     1       INPUT   = 0 or 2 DISP at zero flow
C                                         = 1 or 3 no DISP at zero flow
C                                         = 0 or 1 DISP over boundary
C                                         = 2 or 3 no DISP over boundary
C     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
C
      use timers

      DIMENSION  DISP  (  3) , DISPER(*) , AREA (*) , FLOW (*) ,
     *           ALENG (  *) , VELO  (*) , BOUND(*) , DERIV(*) ,
     *           IPOINT(4,*) , IDPNT(*)  , IVPNT(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqb7", ithandl )
C
      NOQ12 = NOQ1 + NOQ2
      DO 40 IQ = 1 , NOQ
C
C         initialisations , check for transport anyhow
C
      I    = IPOINT(1,IQ)
      J    = IPOINT(2,IQ)
      IF ( I .GT. 0 .AND. J .GT. 0 ) GOTO 40
      IF ( I .EQ. 0 .OR.  J .EQ. 0 ) GOTO 40
      IF ( I .LT. 0 .AND. J .LT. 0 ) GOTO 40
      A    = AREA(IQ)
      Q    = FLOW(IQ)

      ! thin dam check, only in horizontal

      IF ( MOD(IOPT,2) .EQ. 1 .AND. IQ .LE. NOQ12 .AND. ABS(Q) .LT. 10.0E-25 ) GOTO 40
           IF ( A .LT. 1.0E-25 )  A = 1.0
      E  = DISP(1)
      AL = ALENG(1)
      IF ( IQ .GT. NOQ1      ) THEN
           E  = DISP (2)
           AL = ALENG(2)
      ENDIF
      IF ( IQ .GT. NOQ1+NOQ2 ) THEN
           E  = DISP (3)
           AL = ALENG(3)
      ENDIF
      IF ( ILFLAG .EQ. 1 ) THEN
           DL = A / (ALENG(2*IQ-1) + ALENG(2*IQ))
      ELSE
           DL = A / AL
      ENDIF
      E  = E*DL
      IF (IDPNT(ISYS).GT.0) E = E + DISPER((IQ-1)*NODISP+IDPNT(ISYS))*DL
      IF (IVPNT(ISYS).GT.0) Q = Q + VELO  ((IQ-1)*NOVELO+IVPNT(ISYS))*A
      IF ( Q .GT. 0.0 ) THEN
           Q1 =   Q
           Q2 = 0.0
      ELSE
           Q1 = 0.0
           Q2 =   Q
      ENDIF
      IF ( J .LT. 0 ) GOTO 20
C
C        The 'from' segment is a boundary
C
      IF ( MOD(IOPT,4) .GT. 1 ) E = 0.0
      K1 = (-I-1)*NOTOT
      I4 = ( J-1)*NSYS  + 1
      DO 10 I3=ISYS,ISYS+NSYS-1
      DERIV(I4) = DERIV(I4) + ( Q1+E) * BOUND(K1+I3)
   10 I4=I4+1
      GOTO 40
C
C        The 'to' element was a boundary.
C
   20 IF ( MOD(IOPT,4) .GT. 1 ) E = 0.0
      K2 = (-J-1)*NOTOT
      I4 = ( I-1)*NSYS  + 1
      DO 30 I3=ISYS,ISYS+NSYS-1
      DERIV(I4) = DERIV(I4) + (-Q2+E) * BOUND(K2+I3)
   30 I4=I4+1
C
C        end of the loop over exchanges
C
   40 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
