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

      SUBROUTINE DLWQD2 ( LUNUT , NOSYS , NOTOT , NOSEG , NOQ3 ,
     *                    KMAX  , CONC  , ALENG , NOWARN, OWNERS,
     *                    MYPART )
C
C
C       Forester filter for the vertical.
C
C       Then loops over the number of horizontal segments en over the
C       the number of active substances are made. All these verticals
C       are filtered at most MAXFIL times.
C
C       Everything starts with layer 2 (from the top in WAQ). IL is
C       the counter here with starting values ISEG+NOSEG. ILU points
C       to the layer upper (IL-NOSEG) en ILD points to the layer down-
C       wards (IL+NOSEG).
C
C       The from- and to- lengthes are in the ALENG array. The third
C       direction last. Than means that NOQT-NOQ3+ISEG is the pointer
C       for the exchange with the layer above and that value plus NOSEG
C       is the pointer to the exchange downward. The from- value is
C       the first one that is in the higher layer, The to- value is in
C       the lower layer. You can check that the to- value for the upper
C       exchange should equal the from- value for the downward exchange.
C
C       The filter starts action if the layer value is over DD larger
C       than or over DD smaller than both bordering values. If that
C       the IFIL flag signals a filter step.
C       The filter corrects the difference that is largest. It does so
C       by taking half of that difference, or the other difference
C       which one is smallest. It multiplies this with the smallest
C       thickness (and unknown A thus with the smallest volume). It
C       corrects with this mass and divides by the volume (the unknown
C       A and the thickness) to get a concentration again. This means
C       that per step at most 0.5 times the largest difference is
C       corrected.
C
C       Because WAQ has halflengthes, you must read "half the volume"
C       but that does not differ because 0.5/0.5 = 1.0. There is an
C       upperbound to the coorection that is at 1.0 m thickness in the
C       original code. Because we work with half distances, it is 0.5
C       here.
C
C       A maximum/minimum of DD remains. This value is a somewhat
C       strange construct. If you got 0.01 mg Cadmium per liter in
C       your water you better call on an environmental specialist.
C
      use timers
C
      INTEGER     LUNUT , NOSYS , NOTOT , NOSEG , NOQ3  , NOWARN
      REAL        CONC(NOTOT,NOSEG) , ALENG(2,NOQ3)
      INTEGER     OWNERS(NOSEG), MYPART
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwqd2", ithandl )
C
      DD     = 1.0E-02
      MAXFIL = 100
C
C     Only for structured 3D
C
      IF ( KMAX .LE. 1 ) goto 9999
C
      NHOR = NOSEG/KMAX
C          for all horizontal segments
      DO 40 ISEG = 1 , NHOR
         IF (OWNERS(ISEG).EQ.MYPART) THEN
C          for all active substances
            DO 30 ISYS = 1 , NOSYS
C          do until maximum iteration or untill satisfied
               DO 20 IFILT = 1 , MAXFIL
                  IFIL = 0
                  IL  = ISEG
                  ILU = IL - NHOR
                  ILD = IL + NHOR
                  DO 10 ILAY = 2,KMAX-1
                     IL  = IL  + NHOR
                     ILU = ILU + NHOR
                     ILD = ILD + NHOR
                     DR1 = CONC(ISYS,IL) - CONC(ISYS,ILU)
                     DR2 = CONC(ISYS,IL) - CONC(ISYS,ILD)
C                    test for local maximum
                     IF ( DR1 .GT.  DD .AND. DR2 .GT.  DD ) THEN
                        IFIL = 1
                        IF ( DR1 .GT. DR2 ) THEN
                           DR   = MIN (0.5*DR1,DR2)
                           DZ1  = ALENG(1,ILU)
                           DZ2  = ALENG(2,ILU)
                           COEF = MIN ( DZ1, DZ2, 0.5) * DR
                           CONC(ISYS,ILU) = CONC(ISYS,ILU) + COEF/DZ1
                           CONC(ISYS,IL ) = CONC(ISYS,IL ) - COEF/DZ2
                        ELSE
                           DR   = MIN (DR1,0.5*DR2)
                           DZ1  = ALENG(1,IL)
                           DZ2  = ALENG(2,IL)
                           COEF = MIN ( DZ1, DZ2, 0.5) * DR
                           CONC(ISYS,IL ) = CONC(ISYS,IL ) - COEF/DZ1
                           CONC(ISYS,ILD) = CONC(ISYS,ILD) + COEF/DZ2
                        ENDIF
                     ENDIF
C     test for local minimum
                     IF ( DR1 .LT. -DD .AND. DR2 .LT. -DD ) THEN
                        IFIL = 1
                        IF ( DR1 .LT. DR2 ) THEN
                           DR   = MAX (0.5*DR1,DR2)
                           DZ1  = ALENG(1,ILU)
                           DZ2  = ALENG(2,ILU)
                           COEF = MIN ( DZ1, DZ2, 0.5) * DR
                           CONC(ISYS,ILU) = CONC(ISYS,ILU) + COEF/DZ1
                           CONC(ISYS,IL ) = CONC(ISYS,IL ) - COEF/DZ2
                        ELSE
                           DR   = MAX (DR1,0.5*DR2)
                           DZ1  = ALENG(1,IL)
                           DZ2  = ALENG(2,IL)
                           COEF = MIN ( DZ1, DZ2, 0.5) * DR
                           CONC(ISYS,IL ) = CONC(ISYS,IL ) - COEF/DZ1
                           CONC(ISYS,ILD) = CONC(ISYS,ILD) + COEF/DZ2
                        ENDIF
                     ENDIF
   10             CONTINUE
                  IF ( IFIL .EQ. 0 ) GOTO 30
   20          CONTINUE
               IF ( IFIL .EQ. 1 ) THEN
                  IF ( NOWARN .LT. 1000 ) WRITE ( LUNUT , 1010 ) ISYS,ISEG,ILAY
                  NOWARN = NOWARN + 1
               ENDIF
   30       CONTINUE
         END IF  ! (owners(iseg.eq.mypart)
   40 CONTINUE
C
 9999 if ( timon ) call timstop ( ithandl )
      RETURN
C
 1010 FORMAT ( ' WARNING: Forester filter max. iterations reached for substance: ',
     *         I2,'; segment: ', I6,'; layer: ',I2,' !' )
C
      END
