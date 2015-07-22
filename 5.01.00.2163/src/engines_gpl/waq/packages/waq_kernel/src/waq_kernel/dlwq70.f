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

      SUBROUTINE DLWQ70 ( DISP   , DISPER , AREA   , FLOW   , ALENG  ,
     *                    VELO   , BOUND  , IPOINT , NOTOT  , ISYS   ,
     *                    NSYS   , NOQ1   , NOQ2   , NOQ    , NODISP ,
     *                    NOVELO , IDPNT  , IVPNT  , DERIV  , AMAT   ,
     *                                      JTRACK , IOPT   , ILFLAG )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : june 1988 by L.Postma
C
C     FUNCTION            : Fills matrix according to central
C                                       differencing in space.
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
C     NOTOT   INTEGER     1       INPUT   number of total substances
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
C     AMAT    REAL      large     IN/OUT  matrix to be updated
C     JTRACK  INTEGER     1       INPUT   number of codiagonals of AMAT
C     IOPT    INTEGER     1       INPUT   = 0 or 2 DISP at zero flow
C                                         = 1 or 3 no DISP at zero flow
C                                         = 0 or 1 DISP over boundary
C                                         = 2 or 3 no DISP over boundary
C     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
C
      use timers

      DIMENSION  DISP  (  3) , DISPER(*) , AREA (*) , FLOW (*) ,
     *           ALENG (  *) , VELO  (*) , BOUND(*) , AMAT (*) ,
     *           IPOINT(4,*) , IDPNT(*)  , IVPNT(*) , DERIV(*)
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq70", ithandl )
C
      IBAND = 2*JTRACK + 1
      DO 50 IQ = 1 , NOQ
C
C         initialisations , check for transport anyhow
C
      I    = IPOINT(1,IQ)
      J    = IPOINT(2,IQ)
      IF ( I .EQ. 0 .OR. J .EQ. 0 ) GOTO 50
      A    = AREA(IQ)
      Q    = FLOW(IQ)
      IF ( MOD(IOPT,2) .EQ. 1 .AND. ABS(Q) .LT. 10.0E-25 ) GOTO 50
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
           DL = A/(ALENG(2*IQ-1) + ALENG(2*IQ))
           F1 = ALENG(2*IQ  )*DL/A
           F2 = ALENG(2*IQ-1)*DL/A
      ELSE
           DL = A/AL
           F1 = 0.5
           F2 = 0.5
      ENDIF
      E  = E*DL
      IF (IDPNT(ISYS).GT.0) E = E + DISPER((IQ-1)*NODISP+IDPNT(ISYS))*DL
      IF (IVPNT(ISYS).GT.0) Q = Q + VELO  ((IQ-1)*NOVELO+IVPNT(ISYS))*A
      Q1 = F1*Q
      Q2 = F2*Q
      IF ( I .LT. 0 ) GOTO 10
      IF ( J .LT. 0 ) GOTO 30
C
C        the regular case
C
      JT = (I-1)*IBAND + JTRACK + 1
      KT = JT + (J-I)
      AMAT(JT) = AMAT(JT) + Q1 + E
      AMAT(KT) = AMAT(KT) + Q2 - E
      IT = (J-1)*IBAND + JTRACK + 1
      KT = IT + (I-J)
      AMAT(IT) = AMAT(IT) - Q2 + E
      AMAT(KT) = AMAT(KT) - Q1 - E
      GOTO 50
C
C        The 'from' segment is a boundary
C
   10 IF ( J    .LT. 0 ) GOTO 50
      IF ( MOD(IOPT,4) .GT. 1 ) E = 0.0
      IF ( MOD(IOPT,8) .GE. 4 ) THEN
           IF ( Q .GT. 0.0 ) THEN
                Q1 = Q
                Q2 = 0.0
           ELSE
                Q1 = 0.0
                Q2 = Q
           ENDIF
      ENDIF
      K1 = (-I-1)*NOTOT
      I4 = ( J-1)*NSYS  + 1
      DO 20 I3=ISYS,ISYS+NSYS-1
      DERIV(I4) = DERIV(I4) + ( Q1+E) * BOUND(K1+I3)
   20 I4=I4+1
      IT = (J-1)*IBAND + JTRACK + 1
      AMAT(IT) = AMAT(IT) - Q2 + E
      GOTO 50
C
C        The 'to' element was a boundary.
C
   30 IF ( MOD(IOPT,4) .GT. 1 ) E = 0.0
      IF ( MOD(IOPT,8) .GE. 4 ) THEN
           IF ( Q .GT. 0.0 ) THEN
                Q1 = Q
                Q2 = 0.0
           ELSE
                Q1 = 0.0
                Q2 = Q
           ENDIF
      ENDIF
      K2 = (-J-1)*NOTOT
      I4 = ( I-1)*NSYS  + 1
      DO 40 I3=ISYS,ISYS+NSYS-1
      DERIV(I4) = DERIV(I4) + (-Q2+E) * BOUND(K2+I3)
   40 I4=I4+1
      JT = (I-1)*IBAND + JTRACK + 1
      AMAT(JT) = AMAT(JT) + Q1 + E
C
C        end of the loop over exchanges
C
   50 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
