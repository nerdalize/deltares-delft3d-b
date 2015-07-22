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

      SUBROUTINE DLWQ46 ( DISP   , DISPER , AREA   , FLOW   , ALENG  ,
     *                    VELO   , CONC   , BOUND  , IPOINT , NOSYS  ,
     *                    NOTOT  , NOQ    , NODISP , NOVELO , IDPNT  ,
     *                    IVPNT  , IOPT   , IDT    , ILFLAG , DMPQ   ,
     *                    NDMPQ  , IQDMP  , IBACKW , NOQW   , OWNERS ,
     *                    MYPART )
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED             : october 1991 by J. van Beek
C
C     FUNCTION            : makes mass balance for segments for the past
C                           implicit step with the new concentrations
C                           and the old flow's
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
C     CONC    REAL   NOTOT*NOSEG  INPUT   concentrations
C     BOUND   REAL     NOSYS*?    INPUT   boundary concentrations
C     IPOINT  INTEGER    4*NOQ    INPUT   exchange pointers
C     NOSYS   INTEGER     1       INPUT   number  of active substances
C     NOTOT   INTEGER     1       INPUT   number  of total substances
C     NOQ     INTEGER     1       INPUT   total number of exchanges
C     NODISP  INTEGER     1       INPUT   number  of additional dispers.
C     NOVELO  INTEGER     1       INPUT   number  of additional velos.
C     IDPNT   INTEGER   NOSYS     INPUT   pointer systems to dispersions
C     IVPNT   INTEGER   NOSYS     INPUT   pointer systems to velocities
C     IOPT    INTEGER     1       INPUT   = 0 or 2 DISP at zero flow
C                                         = 1 or 3 no DISP at zero flow
C                                         = 0 or 1 DISP over boundary
C                                         = 2 or 3 no DISP over boundary
C     IDT     INTEGER     1       INPUT   time step size
C     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
C     DMPQ    REAL  NOTOT*NDMPQ*? IN/OUT  mass balance dumped exchange
C                                         if INTOPT > 7
C     NDMPQ   INTEGER     1       INPUT   number of dumped exchanges
C     IQDMP   INTEGER     *       INPUT   pointer dumped exchanges
C     IBACKW  INTEGER     *       INPUT   flag = 0 central differences
C                                              = 1 backward differences
C     NOQW    INTEGER     1       INPUT   number of water exchanges
C     OWNERS  INTEGER   NOSEG     INPUT   ownership of segments
C     MYPART  INTEGER     1       INPUT   number of current part/subdomain
C
      use timers
      INTEGER    NDMPQ         , IBACKW
      INTEGER    IQDMP   (*)
      DIMENSION  DISP  (  3)   , DISPER(*) , AREA (*) , FLOW (*) ,
     *           ALENG (  *)   , VELO  (*) , CONC (*) , BOUND(*) ,
     *           IPOINT( 4,* ) , IDPNT(*)  , IVPNT(*) , DMPQ(*)
      integer noqw                      !  input   number of exchanges waterphase
      INTEGER    OWNERS(*), MYPART

      logical disp0q0, disp0bnd, loword !  logical representation of options

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq46a", ithandl )

      disp0q0  = btest( iopt , 0 )
      disp0bnd = btest( iopt , 1 )
      loword   = btest( iopt , 2 )
C
C     offsets in mass balance array
C
      I6 = NOSYS*NDMPQ
C
C     loop accross the number of exchanges
C
      DO 60 IQ = 1 , NOQ
C
C        initialisations , check for transport anyhow
C
         I    = IPOINT(1,IQ)
         J    = IPOINT(2,IQ)
         IF ( I .EQ. 0 .OR. J .EQ. 0 ) GOTO 60
         IF ( I .LT. 0) THEN
            IF (OWNERS(J).NE.MYPART) GOTO 60
         ELSEIF ( J .LT. 0) THEN
            IF (OWNERS(I).NE.MYPART) GOTO 60
         ELSE
            IF (OWNERS(I).NE.MYPART .AND. OWNERS(J).NE.MYPART) GOTO 60
         ENDIF
         A    = AREA(IQ)
         Q    = FLOW(IQ)
!jvb     IF ( MOD(IOPT,2) .EQ. 1 ) THEN
!             IF ( ABS(Q) .LT. 10.0E-25 )  GOTO 60
!jvb     ENDIF
C
C     Check if exchange is dump exchange, set IPB
C
         IF ( IQDMP(IQ) .GT. 0 ) THEN
            IPB = (IQDMP(IQ)-1)*NOSYS
         ELSE
            GOTO 60
         ENDIF
         E  = DISP(1)
         AL = ALENG(1)
         IF ( ILFLAG .EQ. 1 ) THEN
              AL = ALENG(2*IQ-1) + ALENG(2*IQ)
              F1 = ALENG(2*IQ  ) / AL
              F2 = ALENG(2*IQ-1) / AL
         ELSE
              F1 = 0.5
              F2 = 0.5
         ENDIF
         G1 = F1
         G2 = F2
         DL = A / AL
         E  = E*DL
         if ( iq .gt. noqw ) e = 0.0      !  no water diffusion in the bottom
         IF ( I .LT. 0 ) GOTO 20
         IF ( J .LT. 0 ) GOTO 40
C
C            The regular case
C
         K1 = (I-1)*NOTOT
         K2 = (J-1)*NOTOT
         DO 10 I3=1,NOSYS
C
C           dispersion
C
            IF ( IDPNT(I3) .GT. 0 ) THEN
               D = E + DISPER((IQ-1)*NODISP+IDPNT(I3))*DL
            ELSE
               D = E
            ENDIF
C
C           flow
C
            IF ( IVPNT(I3) .GT. 0 ) THEN
               V = Q + VELO((IQ-1)*NOVELO+IVPNT(I3))  *A
            ELSE
               V = Q
            ENDIF
            if ( disp0q0 ) then
               if ( a .le. 0.0 .or. abs(v) .lt. 10.0E-25 ) d = 0.0
            endif
            IF ( IQ .GT. NOQW .OR. IBACKW .EQ. 1 ) THEN  ! in the bottom backward differences
               IF ( V .GT. 0 ) THEN
                  G1 = 1.0
                  G2 = 0.0
               ELSE
                  G1 = 0.0
                  G2 = 1.0
               ENDIF
            ENDIF
C
C           transport
C
            DQ = (V*G1+D)*CONC(K1+I3) + (V*G2-D)*CONC(K2+I3)
C
C           update mass balance array ( internal transport )
C
            IF ( DQ .GT. 0.0 ) THEN
               DMPQ(IPB+I3)=DMPQ(IPB+I3) + DQ*IDT
            ELSE
               DMPQ(IPB+I3+I6)=DMPQ(IPB+I3+I6) - DQ*IDT
            ENDIF
   10    CONTINUE
         GOTO 60
C
C           The 'from' element was a boundary. Note the 2 options.
C
   20    IF ( J .LT. 0 ) GOTO 60
         K1 = (-I-1)*NOSYS
         K2 = ( J-1)*NOTOT
         DO 30 I3=1,NOSYS
C
C           flow
C
            IF ( IVPNT(I3) .GT. 0 ) THEN
               V = Q + VELO((IQ-1)*NOVELO+IVPNT(I3))*A
            ELSE
               V = Q
            ENDIF
            IF ( IQ .GT. NOQW .OR. MOD(IOPT,8) .GE. 4 .OR. IBACKW .EQ. 1 ) THEN ! in the bottom backward differences + etc
               IF ( V .GT. 0 ) THEN
                  G1 = 1.0
                  G2 = 0.0
               ELSE
                  G1 = 0.0
                  G2 = 1.0
               ENDIF
            ENDIF
C
C           dispersion
C
            IF ( MOD(IOPT,4) .LT.  2 ) THEN
               IF ( IDPNT(I3).GT.0 ) THEN
                  D = E + DISPER((IQ-1)*NODISP+IDPNT(I3))*DL
               ELSE
                  D = E
               ENDIF
            ELSE
               D = 0.0
            ENDIF
            if ( disp0q0 ) then
               if ( a .le. 0.0 .or. abs(v) .lt. 10.0E-25 ) d = 0.0
            endif
C
C           tranport
C
            DQ = (V*G1+D)*BOUND(K1+I3) + (V*G2-D)*CONC(K2+I3)
C
C           update mass balance array ( boundaries in / out )
C
            IF ( DQ .GT. 0.0 ) THEN
               DMPQ(IPB+I3)=DMPQ(IPB+I3) + DQ*IDT
            ELSE
               DMPQ(IPB+I3+I6)=DMPQ(IPB+I3+I6) - DQ*IDT
            ENDIF
   30    CONTINUE
         GOTO 60
C
C        The 'to' element was a boundary.
C
   40    K1 = ( I-1)*NOTOT
         K2 = (-J-1)*NOSYS
         DO 50 I3=1,NOSYS
C
C           flow
C
            IF ( IVPNT(I3) .GT. 0 ) THEN
               V = Q + VELO((IQ-1)*NOVELO+IVPNT(I3))*A
            ELSE
               V = Q
            ENDIF
            IF ( IQ .GT. NOQW .OR. MOD(IOPT,8) .GE. 4 .OR. IBACKW .EQ. 1 )THEN ! in the bottom backward differences + etc
               IF ( V .GT. 0 ) THEN
                  G1 = 1.0
                  G2 = 0.0
               ELSE
                  G1 = 0.0
                  G2 = 1.0
               ENDIF
            ENDIF
C
C           dispersion
C
            IF ( MOD(IOPT,4) .LT.  2 ) THEN
               IF ( IDPNT(I3).GT.0 ) THEN
                  D = E + DISPER((IQ-1)*NODISP+IDPNT(I3))*DL
               ELSE
                  D = E
               ENDIF
            ELSE
               D  = 0.0
            ENDIF
            if ( disp0q0 ) then
               if ( a .le. 0.0 .or. abs(v) .lt. 10.0E-25 ) d = 0.0
            endif
C
C           transport
C
            DQ = (V*G1+D)*CONC(K1+I3) + (V*G2-D)*BOUND(K2+I3)
C
C           update mass balance array ( boundaries in / out )
C
            IF ( DQ .GT. 0.0 ) THEN
               DMPQ(IPB+I3)=DMPQ(IPB+I3) + DQ*IDT
            ELSE
               DMPQ(IPB+I3+I6)=DMPQ(IPB+I3+I6) - DQ*IDT
            ENDIF
   50    CONTINUE
C
C        end of the loop over exchanges
C
   60 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
      END
