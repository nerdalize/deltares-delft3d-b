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

C
C  *********************************************************************
C  *         SUBROUTINE TO CALCULATE GRAZING RATE CONSTANTS            *
C  *                  MULTIPLE SPECIES                                 *
C  *********************************************************************
C
      SUBROUTINE CONSB2(X,GRAMOR,GRADET,ZOOD,ZOOC,ITNUM,LGRAZ,T,
     1                  PERIOD,GRAMX,LCOUPL,ZMAX)

C**** Name   Type Size   I/O Description
C**** ------ ---- ------ --- -------------------------------------------------
C**** AA     R*8  NUNUCO*I   BLOOM algae stoichiometry matrix excluding
C****             NUSPEC     carbon
C**** ALGFLX R*8  NUSPEC Total algal flux (mg C/l.period) [POSITIVE = DECREASE ]
C**** ALGSTC R*8  NTONUT*    Algae stoichiometry matrix including carbon
C****             NUSPEC
C**** ALTFLX R*8  NTONUT Total algal flux (mg/l.d) [POSITIVE = ALGAE DECREASE ]
C**** BOTFLX R*8  NTONUT Total bottom detritus flux (mg/l.period)
C**** CDETR  R*8  NTONUT     detritus concentration for the nutrients
C****                        including carbon (g/m3)
C**** CTODRY R*8  NUSPEC I   dry weigth to carbon ratio of the algae species
C**** DETRIT R*8  NUNUCO I   BLOOM detritus concentration for the
C****                        nutrients excluding carbon (mg/m3)
C**** DETFLX R*8  NTONUT     Flux of nutrients from detritus
C**** DISFLX R*8  NTONUT Total dissolved nutrients flux (mg/l.period)
C**** GDETPR R*8  NUGRAZ I   Preference for detritus
C**** GDETFF R*8  NUGRAZ I   Feacal fraction for detritus
C**** GDETO  R*8  NUNUCO     First order decay rate of detritus due to grazing
C****                        for the previous iteration step
C**** GFECFR R*8  NUSPEC*I   Feacal fraction for algae types
C****             NUGRAZ
C**** GMORO  R*8  NUSPEC     First order decay rate of algae due to grazing
C****                        for the previous iteration step
C**** GRADET R*8  NUNUCO O   First order decay rate of detritus due to grazing
C**** GRAMOR R*8  NUSPEC O   First order decay rate of algae due to grazing
C**** GRAMX  R*8  NUSPEC I   Maximum sustainable grazing rate
C**** GRZBRU R*8         Bruto growth rate grazer (mg C/l.d)
C**** GRZFIL R*8             Realized filtration rate
C**** GRZFOO R*8         Available food (mg C/l)
C**** GRZFM  R*8  NUGRAZ I   Maximum clearance rate grazer (l/mg C.d)
C**** GRZGM  R*8  NUGRAZ I   Maximum relative growth grazer (1/d)
C**** GRZGRZ R*8         Grazing rate (1/d)
C**** GRZMET R*8         Respiration grazer (1/d)
C**** GRZMM  R*8  NUGRAZ I   Maximum relative mortality grazer (1/d)
C**** GRZMO  R*8  NUGRAZ I   Monod term filtr.r. in rel. to foodconc. (mg C/l)
C**** GRZNEW R*8  NUGRAZ     biomass of the grazers provided for the current
C****                        timestep (gC/m3) - can be adapted
C**** GRZOLD R*8  NUGRAZ     biomass of the grazers during the previous
C****                        timestep (gC/m3)
C**** GRZPMX R*8         Max. sustainable bruto growth rate (mg C/l.d)
C**** GRZRM  R*8  NUGRAZ I   Maximum daily ration grazer (mg C/mg C.d)
C**** GRZRAT R*8             Realized feeding rate
C**** GRZRE  R*8  NUGRAZ     Routine respiration coefficient (fraction)
C**** GRZSE  R*8  NUGRAZ I   Standard respiration coefficient (1/d)
C**** GRZST  R*8  NTONUT*    Grazer stoichiometry matrix including carbon
C****             NUGRAZ
C**** GRZUPT R*8  NTONUT Total intake per element (mg/l.d)
C**** GTMPFM R*8  NUGRAZ I   Temperature coefficients filtration rate
C**** GTMPGM R*8  NUGRAZ I   Temperature coefficients maximum growth rate
C**** GTMPMM R*8  NUGRAZ I   Temperature coefficients maximum mortality rate
C**** GTMPRE R*8  NUGRAZ I   Temperature coefficients routine metabolism
C**** GTMPRM R*8  NUGRAZ I   Temperature coefficients feeding rate
C**** GTMPSE R*8  NUGRAZ I   Temperature coefficients standard metabolism
C**** GTODET R*8  NUGRAZ I   Fraction of excretion of grazers to the water colum
C**** IFILSP I*4             Counter in the loop over the grazer types
C**** IPERM  I*4         I   Maximum number of steps in grazing iteration loop
C**** ITNUM  I*4         I   Step number in grazing iteration loop
C**** LCOUPL I*4         I   Switch for stand alone (0) or coupled (1)
C**** LGRAZ  I*4         IO  Switch for start of next iteration step (1)
C**** LZERO  I*4             Switch for zero biomass of food
C**** NTONUT I*4             Number of nutrients for CONSBL, NUNUCO + 1
C**** NUGRAZ I*4         I   Number of grazers
C**** NUNUCO I*4         I   Number of nutrients for BLOOM, excluding carbon
C**** NUSPEC I*4         I   Total number of algae species
C**** PERIOD R*8         I   Duration of period (d)
C**** T      R*8         I   Temperature (degrees Celsius)
C**** TOTGRA R*8             Total grazing rate (1/d)
C**** X      R*8  MX+1   I   A.o. algae biomass (mgC/m3)
C**** ZOOC   R*8  NUGRAZ I   biomass of the grazers during the previous
C****                        timestep (gC/m3)
C**** ZOOD   R*8  NUGRAZ I   biomass of the grazers provided for the current
C****                        timestep (gC/m3)
C**** ZOONUT R*8  NUNUCO*I   BLOOM grazer stoichiometry matrix excluding
C****             NUGRAZ     carbon
C**** ZOOPR  R*8  NUSPEC*I   Preference for algae types
C****             NUGRAZ
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'ioblck.inc'
      DIMENSION X(MX),GRAMOR(MT),ZOOD(0:MG),ZOOC(MG),GRZOLD(MG),
     1          GRZNEW(MG),DISFLX(MN+1),DETFLX(MN+1),BOTFLX(MN+1),
     2          ALGFLX(MT),GRZUPT(MN+1),ALGSTC(MN+1,MT),GRZST(MN+1,MG),
     3          DFLUX(MN+1),ALTFLX(MN+1),GRZFLX(MN+1),TOTFLX(MN+1),
     4          CDETR(MN+1),GRADET(MN),GMORO(MT),GDETO(MN)
      SAVE GMORO,GDETO,LZERO

C****
C*  1 Make a copy of the algal and grazers stochiometry
C****
      ZMAX = 1.D7
      IF (ITNUM.LE.1) THEN
        LZERO = 0
      ELSEIF (LZERO.EQ.1) THEN
        LZERO = 0
        LGRAZ = 0
        RETURN
      ENDIF

      NTONUT = NUNUCO + 1
      DO 280 I=1,NUNUCO
        GRADET(I) = 0.0D0
  280 CONTINUE

      IF ((ITNUM.EQ.1).AND.(ITNUM.LT.IPERM)) THEN
        LGRAZ = 1
      ELSE
        LGRAZ = 0
      ENDIF
      DO 210 J = 1, NUSPEC
          GRAMOR(J) = 0.0D0
          ALGSTC(1,J)=1.0
          DO 220 I = 2, NTONUT
              ALGSTC(I,J) = AA(I-1, J) * CTODRY ( J)
  220     CONTINUE
  210 CONTINUE

      DO 230 J = 1, NUGRAZ
          GRZST(1,J)=1.0
          DO 240 I = 2, NTONUT
              GRZST(I,J) = ZOONUT(I-1, J)
  240     CONTINUE
  230 CONTINUE

C     The amount of carbon in detritus is not known in BLOOM
C     Here we estimate the amount from the amount of nitrogen
C     in detritus. The ratio is taken is the average ratio in the
C     current algae species composition

C     Calcuate the average C/N ratio in the algae
      K = NUROWS
      ALGC = 0.0D0
      ALGN = 0.0D0
      DO 260 J=1,NUSPEC
        K = K + 1
        ALGC = ALGC + X(K)
        ALGN = ALGN + X(K) * ALGSTC(2,J)
  260 CONTINUE

C     Convert DET-N to DET-C and fill the rest of the CDETR array
      IF (ALGN.GT.1.0D-6) THEN
        CDETR(1) = 1.D-3 * DETRIT(1) * ALGC / ALGN
      ELSEIF (CDETR(2).GT.1.0D-6) THEN
        CDETR(1) = 1.D-3 * DETRIT(1) * CDETR(1) / CDETR(2)
      ELSEIF (DETRIT(1).LT.1.0D-6) THEN
        CDETR(1) = 0.0
      ELSE
        CDETR(1) = 10.0D-3 * DETRIT(1)
        WRITE(*,*) 'CONSBL: Could not establish C/N ratio detritus!',
     1             ' Used 10.'
      ENDIF
      DO 270 I=2,NTONUT
        CDETR(I) = 1.D-3 * DETRIT(I-1)
  270 CONTINUE

C
C     The routine does not produce if biomass in GRZNE1 (segfun)
C     evers becomes 0.0, so safeguard added 940524 AH
C
      DO 10 J=1,NUGRAZ
        IF (ZOOC(J).LT.1.0D-20) THEN
          GRZOLD(J) = 1.0D-20
        ELSE
          GRZOLD(J) = ZOOC(J)
        ENDIF
        IF (ZOOD(J).LT.1.0D-20) THEN
          GRZNEW(J) = 1.0D-20
        ELSE
          GRZNEW(J) = ZOOD(J)
        ENDIF
   10 CONTINUE

C     IF ( DEBUG ) THEN
C        WRITE ( IDBG, 9000) ( I, GRZOLD(I), GRZNEW(I), I=1,NUGRAZ)
C     ENDIF

C****
C*  2 Loop over the grazers
C****
      DO 200 IFILSP = 1, NUGRAZ

C****
C*  3 Initialize output variables
C****

          DO 40 I = 1, NTONUT
              DISFLX(I) = 0.0D0
              DETFLX(I) = 0.0D0
              BOTFLX(I) = 0.0D0
   40     CONTINUE
          DO 50 I = 1, NUSPEC
              ALGFLX(I) = 0.0D0
   50     CONTINUE

C****
C*  4 Check if grazer growth or mortality exceeds constraints
C****

          IF ((GRZOLD(IFILSP) .GT. 0.0).AND.(LCOUPL.NE.0)) THEN
              IF ((GRZNEW(IFILSP) - GRZOLD(IFILSP)) .GE. 0.0) THEN
                  IF (GRZNEW(IFILSP) .GT. GRZOLD(IFILSP) *
     &            (1.0 + GRZGM(IFILSP) *
     &            EXP(GTMPGM(IFILSP) *
     &            (T - 20.0)) * PERIOD)) THEN
                      GRZNEW(IFILSP) = GRZOLD(IFILSP) *
     &                (1.0 + GRZGM(IFILSP) *
     &                EXP(GTMPGM(IFILSP) *
     &                (T - 20.0)) * PERIOD)
                  ENDIF
              ELSE
                  IF (GRZNEW(IFILSP) .LT. GRZOLD(IFILSP) *
     &            (1.0 - GRZMM(IFILSP) *
     &            EXP(GTMPMM(IFILSP) *
     &            (T - 20.0)) * PERIOD)) THEN
                      GRZNEW(IFILSP) = GRZOLD(IFILSP) *
     &                (1.0 - GRZMM(IFILSP) *
     &                EXP(GTMPMM(IFILSP) *
     &                (T - 20.0)) * PERIOD)
                  ENDIF
              ENDIF
          ENDIF

C****
C*  5 Calculate total available amount of food (mg C/l)
C****
          GRZFOO = CDETR(1) * GDETPR(IFILSP)
          K = NUROWS
          DO 60 I = 1, NUSPEC
              K = K + 1
              GRZFOO = GRZFOO + X(K) * ZOOPR(I,IFILSP) * 1.D-3
   60     CONTINUE

C****
C*  6 Calculate grazing (1/d) rate and optionally maximum grazing rate
C****
          IF (GRZFOO .GT. 0.0) THEN
              IF (GRZFOO .LT. ((EXP(GTMPRM(IFILSP) *
     &        (T - 20.0)) * GRZRM(IFILSP)) /
     &        (EXP(GTMPFM(IFILSP) * (T - 20.0)) *
     &        GRZFM(IFILSP) *
     &        (GRZFOO / (GRZFOO + GRZMO(IFILSP)))))) THEN
                  GRZGRZ = ((GRZOLD(IFILSP) *
     &            EXP(GTMPFM(IFILSP) * (T - 20.0)) *
     &            GRZFM(IFILSP)) *
     &            (GRZFOO / (GRZFOO + GRZMO(IFILSP))))
              ELSE
                  GRZGRZ = ((GRZOLD(IFILSP) *
     &            EXP(GTMPRM(IFILSP) * (T - 20.0)) *
     &            GRZRM(IFILSP)) / GRZFOO)
              ENDIF
          ELSE
              GRZGRZ = 0.0
              LZERO = 2
          ENDIF
C         IF (GRZGRZ.GT.GRAMX) THEN
C           GRZGRZ=GRAMX
C           LZERO = 1
C         ENDIF

C****
C*  7 Calculate realized feeding (mg C/mg C.d) and filtration rate (l/mg C.d)
C*    GRZRAT and GRZFIL are output variables, not used at the moment
C****
          IF (GRZOLD(IFILSP) .GT. 0.0) THEN
              GRZRAT = GRZFOO * GRZGRZ / GRZOLD(IFILSP)
              GRZFIL = GRZGRZ / GRZOLD(IFILSP)
          ELSE
              GRZRAT = 0.0
              GRZFIL = 0.0
          ENDIF

C****
C*  8 Calculate flux and biomass for detritus and phytoplankton groups
C****
          DO 70 I = 1, NTONUT
              DFLUX(I) = -(CDETR(I) * GRZGRZ * GDETPR(IFILSP))
   70     CONTINUE
          K = NUROWS
          DO 80 I = 1, NUSPEC
              K = K + 1
              ALGFLX(I) = -(X(K) * GRZGRZ * ZOOPR(I,IFILSP)) * 1.D-3
              GRAMOR(I) = GRAMOR(I) + GRZGRZ * ZOOPR(I,IFILSP)
   80     CONTINUE

C****
C*  9 Calculate total intake per element (mg/d)
C****
          DO 100 J = 1, NTONUT
              GRZUPT(J) = -(DFLUX(J) * (1.0 - GDETFF(IFILSP)))
              DETFLX(J) = DFLUX(J) - DFLUX(J) *
     &            GDETFF(IFILSP) * GTODET(IFILSP)
              BOTFLX(J) = BOTFLX(J) - DFLUX(J) *
     &            GDETFF(IFILSP) * (1.0-GTODET(IFILSP))
              DO 90 I = 1, NUSPEC
                  GRZUPT(J) = GRZUPT(J) - ALGFLX(I) * ALGSTC(J,I) *
     1            (1.0 - GFECFR(I,IFILSP))
                  DETFLX(J) = DETFLX(J) - ALGFLX(I) * ALGSTC(J,I) *
     1                GFECFR(I,IFILSP) * GTODET(IFILSP)
                  BOTFLX(J) = BOTFLX(J) - ALGFLX(I) * ALGSTC(J,I) *
     1                GFECFR(I,IFILSP) * (1.0 - GTODET(IFILSP))
   90         CONTINUE
  100     CONTINUE

C****
C* 10 Calculate limiting element for growth
C****
          GRZPMX = 10.0E20
          DO 110 I = 1, NTONUT
              IF (GRZST(I,IFILSP) .GT. 0.0) THEN
                  GRZPRT = GRZUPT(I) /
     &            GRZST(I,IFILSP)
                  IF (GRZPRT .LT. GRZPMX) THEN
                      GRZPMX = GRZPRT
                  ENDIF
              ENDIF
  110     CONTINUE

C****
C* 11 Calculate routine respiration (mgC/l.d)
C****
          DO 120 I = 1, NTONUT
              DISFLX(I) = (GRZPMX * EXP(GTMPRE(IFILSP) *
     &        (T - 20.0)) * GRZRE(IFILSP) *
     &        GRZST(I,IFILSP))
              GRZUPT(I) = GRZUPT(I) - DISFLX(I)
  120     CONTINUE
          GRZPMX = GRZPMX * (1.0 - EXP(GTMPRE(IFILSP) *
     &    (T - 20.0)) * GRZRE(IFILSP))

C****
C* 12 Calculate the standard respiration (1/d)
C****
          GRZMET = EXP(GTMPSE(IFILSP) * (T - 20.0)) *
     &             GRZSE(IFILSP)

C****
C* 13 Correct for length of period (d)
C****
          DO 130 I = 1, NUSPEC
              ALGFLX(I) = ALGFLX(I) * PERIOD
  130     CONTINUE
          DO 140 I = 1, NTONUT
              GRZUPT(I) = GRZUPT(I) * PERIOD
              DETFLX(I) = DETFLX(I) * PERIOD
              BOTFLX(I) = BOTFLX(I) * PERIOD
              DISFLX(I) = DISFLX(I) * PERIOD
  140     CONTINUE
          GRZPMX = GRZPMX * PERIOD
          GRZMET = GRZMET * PERIOD

C****
C* 14 Calculate bruto growth (mg C/period)
C****
          GRZBRU = GRZNEW(IFILSP) - GRZOLD(IFILSP) * (1.0 - GRZMET)

C****
C* 15 Correct bruto growth if intake can not sustain respiration and growth
C****
          IF ((GRZBRU .GT. GRZPMX).AND.(LCOUPL.NE.0)) THEN
              GRZBRU = GRZPMX
              GRZNEW(IFILSP) = GRZOLD(IFILSP) * (1.0 - GRZMET) + GRZPMX
          ENDIF

C****
C* 16 Add respiration to dissolved nutrients
C****
          DO 150 I = 1, NTONUT
              DISFLX(I) = DISFLX(I) + GRZOLD(IFILSP) * GRZMET *
     &        GRZST(I,IFILSP)

C****
C* 17 If there is bruto growth, subtract nutrients from the intake
C****
              IF (GRZBRU .GE. 0.0) THEN
                  GRZUPT(I) = GRZUPT(I) - GRZBRU *
     &            GRZST(I,IFILSP)
C****
C* 18 If their is mortality, add the nutrients to the detritus pool
C****
              ELSE
                  DETFLX(I) = DETFLX(I) - GRZBRU *
     &                GRZST(I,IFILSP) * GTODET(IFILSP)
                  BOTFLX(I) = BOTFLX(I) - GRZBRU *
     &                GRZST(I,IFILSP) * (1.0 - GTODET(IFILSP))
              ENDIF

C****
C* 19 Convert the intake not used for bruto growth to the detritus pool
C****
              DETFLX(I) = DETFLX(I) + GRZUPT(I) * GTODET(IFILSP)
              BOTFLX(I) = BOTFLX(I) + GRZUPT(I) * (1.0 - GTODET(IFILSP))
  150     CONTINUE

C****
C* 20 Check massbalans
C****
          DO 170 J=1,NTONUT
              ALTFLX(J) = 0.0E0
              DO 160 I=1,NUSPEC
                  ALTFLX(J) = ALTFLX(J) + ALGFLX(I) * ALGSTC(J,I)
  160         CONTINUE
              GRZFLX(J) = (GRZNEW(IFILSP) - GRZOLD(IFILSP)) *
     &        GRZST(J,IFILSP)
              TOTFLX(J) = DISFLX(J) + DETFLX(J) + BOTFLX(J) +
     &        ALTFLX(J) + GRZFLX(J)
C             IF ( DEBUG ) WRITE ( IDBG), 9030) J, TOTFLX(J)
C
  170     CONTINUE

C****
C* 21 Update total fluxes detritus
C****
          DO 290 I=1,NUNUCO
            IF (CDETR(1).GT.1.D-12) THEN
C             GRADET will be used for calculation of the change in
C             detritus concentration according to
C             DETR = DETR * EXP(-GRADET*DT)
C             uit de flux  DD=(DETFLX + BOTFLX) kan GRADET afgeleid worden
C             GRADET = - LOG(1+DD/DETR) / DT
              IF ((DETFLX(I+1)+BOTFLX(I+1))/CDETR(I+1).LE.-1.D0) THEN
                WRITE(*,*) 'CONSBL: Grazing of detritus becomes ',
     1                     'unstable!'
                WRITE(*,*) '        This is caused by an unrealistic ',
     1                     'high grazing pressure.'
                WRITE(*,*) '        Please adapt the grazing pressure ',
     1                     'in the input.'
                STOP 1
              ENDIF
              GRADET(I) = GRADET(I) - DLOG(1.0D0 + (DETFLX(I+1)+
     1                    BOTFLX(I+1))/(CDETR(I+1))) / PERIOD
C           ELSEIF (ABS(DETFLX(1)+BOTFLX(1)).GT.1.D-12) THEN
C             WRITE(*,*) 'CONSBL: No detritus to add feacal fraction ',
C    1                   'grazers to!'
C             WRITE(*,*) '        Nutients will become available ',
C    1                   'immediately as dissolved.'
            ENDIF
  290     CONTINUE
C         WRITE(*,*) 'CDETR  = ',CDETR(1)
C         WRITE(*,*) 'DFLUX  = ',DFLUX(1)
C         WRITE(*,*) 'DETFLX = ',DETFLX(1)
C         WRITE(*,*) 'BOTFLX = ',BOTFLX(1)
C         WRITE(*,*) 'ALGFLX = ',ALGFLX(1)

C****
C* 22 End loop over filter feeders
C****
  200 CONTINUE

C****
C* 23 Save biomass grazers for next time step
C****
      TOTGRA = 0.0
      DO 250 J=1,NUGRAZ
          TOTGRA=TOTGRA+GRAMOR(J)
  250 CONTINUE

      IF ((ITNUM.GT.1).AND.(ITNUM.LT.IPERM).AND.(LGRAZ.NE.2)) THEN
        DO 300 I=1,NUNUCO
          IF (ABS(GRADET(I)-GDETO(I)).GT.1.0D-06) LGRAZ=1
  300   CONTINUE
        IF (LGRAZ.EQ.0) THEN
          DO 310 J=1,NUSPEC
            IF (ABS(GRAMOR(J)-GMORO(J)).GT.1.0D-06) LGRAZ=1
  310     CONTINUE
        ENDIF
      ENDIF

      IF ((ITNUM.LT.IPERM).AND.(LGRAZ.EQ.1)) THEN
        DO 320 I=1,NUNUCO
          GDETO(I)=GRADET(I)
  320   CONTINUE
        DO 330 J=1,NUSPEC
          GMORO(J)=GRAMOR(J)
  330   CONTINUE
      ENDIF

      IF (LZERO.EQ.2) THEN
        LZERO = 0
        LGRAZ = 0
      ENDIF


C     WRITE(244,'(I2,5(1PE12.4))') ITNUM,GRZOLD(1),GRZNEW(1),GRZFOO,
C    1                             TOTGRA,GRAMX

      RETURN

 9000 FORMAT ( ' CONSBL: ' / ( ' Biomass grazer ', I2, ' old ', E12.5,
     1         '   wanted ', E12.5))
 9010 FORMAT ( '   FLUX nut ',I2, ' dis, det, bot   ', 3E12.5)
 9020 FORMAT ( '   FLUX alg ',I2, '    ', E12.5)
 9030 FORMAT ( '   TOTFLX   ',I2, '    ', E12.5)
 9040 FORMAT ( '   TOT alg  ',I2, '    ', E12.5)

      END
