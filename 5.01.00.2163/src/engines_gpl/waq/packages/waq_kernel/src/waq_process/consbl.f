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

      subroutine consbl ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Grazing module

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES DBS T1449.15
C     Author  : Marnix van der Vat
C     Date    : 951204             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     951204  Marnix v.d.Vat  First Version
C     981130  Marnix v.d.Vat  Fixed bug division faecal fraction over
C                             bottom sediment and water
C                             POC1, PON1, POP1 and POSi1 added as edible
C                             organic matter for GEM
C                             flux POC1, etc. converted to velocity
c     990326  A. Blauw and J. van Beek: various bugs related to 2d to 3d
c     000410  J. van Gils     Add trick to reserve memory, =/ STATVAR
c                             Output velocities shifted over 5 positions
c     000412  J. van Gils     Make grazers bottom species if they produce
c                             bottom detritus, biomass unit becomes g/m2
c     000413  J. van Gils     Bug fixed: limit biomass to zero
c                             not noticed since default pref=0
c     000413  J. van Gils     Bug fixed: NFLUX = 37 and not 33
c                             not used, so removed
c     000413  J. van Gils     Bug fixed: NIN was NTONUT too high
c                             probably no effect
c     000414  J. van Gils     Switch for GEM introduced
c     000517  J. van Gils     Additional switch for biomass unit
c     000517  J. van Gils     Switch for GEM from input!
C     000905  Jan van Beek    Check on dummy exchanges (0->0)
c     070625  J. van Gils     Division by zero in item 6 removed
C     070801  J. van Gils     Apparent bug in item 18 removed
C     071023  J. van Gils     Optimisation to improve performance
C***********************************************************************
C
C     Description of the module :
C
C     CONSBL ROUTINE FOR CALCULATION OF GRAZING PRESSURE ON ALGAE
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local
C
C     Name    Type  Length   I/O  Description
C     ------  ----  ------   ---  -----------

C     ALGDM   R     1             Dry matter in algae (gDM/m3)
c ZDETFF         0.500000     x Faecal fraction for detritus of Zooplank       (-)
c ZDETPR          1.00000     x Preference of Zooplank for detritus            (-)
c ZGRZFM          1.50000     x Max. filtration velocity Zooplank        (m3/gC/d)
c ZGRZGM         0.500000     x Max. relative growth rate Zooplank           (1/d)
c ZGRZML          1.00000     x Mult. factor for biomass Zooplank              (-)
c ZGRZMM         0.500000     x Max. relative mortality Zooplank             (1/d)
c ZGRZMO         0.100000     x Monod term filtration rate Zooplank        (gC/m3)
c ZGRZRE         0.250000     x Maintenance respiration coefficient Zooplank   (-)
c ZGRZRM          1.50000     x Max. daily uptake Zooplank             (mgC/mgC.d)
c ZGRZSE         0.500000E-01 x Standard respiration coefficient Zooplank    (1/d)
c ZFrDetBot       0.00000     x Fract. produced detritus to bottom Zooplank    (-)
c ZGRZSTC         1.00000     x C:C ratio Zooplank                         (gC/gC)
c ZGRZSTN        0.181800     x N:C ratio Zooplank                         (gN/gC)
c ZGRZSTP        0.263000E-01 x P:C ratio Zooplank                         (gP/gC)
c ZGRZSTSi        0.00000     x Si:C ratio Zooplank                       (gSi/gC)
c ZTMPFM         0.400000E-01 x temperature coefficient Zooplank filtration (1/oC)
c ZTMPGM         0.400000E-01 x temperature coefficient Zooplank growth     (1/oC)
c ZTMPMM         0.400000E-01 x temperature coefficient Zooplank mortality  (1/oC)
c ZTMPRE         0.400000E-01 x temperature coefficient Zooplank routine met(1/oC)
c ZTMPRM         0.400000E-01 x temperature coefficient Zooplank feeding rat(1/oC)
c ZTMPSE         0.400000E-01 x temperature coefficient Zooplank standard me(1/oC)
c ZUnitSW         0.00000     x Use gC/m3 (0) or gC/m2 (1) for Zooplankton     (-)
c Zooplank       -999.000     x input concentration of zooplankton-grazer1 (gC/m3)
c CZooplank      -999.000       calculated concentration of zooplankton-gra(gC/m3)
c DetC            0.00000     x Detritus Carbon  (DetC)                    (gC/m3)
c POC1            0.00000       POC1 (fast decaying fraction)              (gC/m3)
c GREEN           0.00000       Algae (non-Diatoms)                        (gC/m3)
c DIAT            0.00000       Diatoms                                    (gC/m3)
c BLOOMALG01     -101.000       concentration of algae type 1              (gC/m3)
c ..
c BLOOMALG15     -101.000       concentration of algae type 15             (gC/m3)
c NCRatGreen     0.160000       N:C ratio Greens                           (gN/gC)
c PCRatGreen     0.200000E-01   P:C ratio Greens                           (gP/gC)
c SCRatGreen      0.00000       Si:C ratio Greens                         (gSi/gC)
c ZALGPRGrn       1.00000       Preference of Zooplank for Greens              (-)
c ZALGFFGrn      0.500000       Faecal fraction Greens for Zooplank            (-)

      INTEGER NTOGRZ,        NTONUT,          NTOALG,
     1        IFILSP,        I     ,          J     ,
     2        NIN   ,        NINGRZ,
     3        IP    ,        IQ    ,          IPP   ,
     4        IPV   ,        INV   ,          IFROM

      PARAMETER (NTOGRZ =  5, NTONUT =  4, NTOALG = 32,
     1           NINGRZ = 24,
     2           NIN    = 5+(NTONUT+2*NTOGRZ)*NTOALG+2*NTONUT+
     3                    NTOGRZ*NINGRZ)

      DIMENSION  IP(NIN)

      REAL    DETBIO(NTONUT), ALGBIO(NTOALG), ALGSTC(NTONUT ,NTOALG),
     1        GRZML (NTOGRZ), GRZOLD(NTOGRZ), GRZNEW(NTOGRZ),
     2        DISFLX(NTONUT), DETFLX(NTONUT), BOTFLX(NTONUT),
     3        ALGFLX(NTOALG), GRZGM (NTOGRZ), TMPGM (NTOGRZ),
     4        WATEMP        , PERIOD        , GRZMM (NTOGRZ),
     5        TMPMM (NTOGRZ), DETPR (NTOGRZ), GRZFOO        ,
     6        ALGPR (NTOALG , NTOGRZ)       , TMPRM (NTOGRZ),
     7        GRZRM (NTOGRZ), TMPFM (NTOGRZ), GRZFM (NTOGRZ),
     8        GRZMO (NTOGRZ), GRZGRZ        , GRZRAT        ,
     9        GRZFIL        , GRZUPT(NTONUT), DETFF (NTOGRZ)
      REAL    ALGFF (NTOALG , NTOGRZ)       , GRZPMX        ,
     A        GRZST (NTONUT , NTOGRZ)       , GRZPRT        ,
     B        GRZRE (NTOGRZ), GRZMET        , TMPSE (NTOGRZ),
     C        GRZSE (NTOGRZ), GRZBRU        , ALTFLX(NTONUT),
     D        GRZFLX(NTONUT), TOTFLX(NTONUT), VOLUME        ,
     E        TMPRE (NTOGRZ), FRDBOT(NTOGRZ), DETRIT(NTONUT),
     F        POC   (NTONUT),                 DEPTH         ,
     G        DFLUX         , FRDBOT_SAVE(NTOGRZ)
      INTEGER BENTHS(NTOGRZ)
      INTEGER IKMRK1, IKMRK2
      REAL    GEM, MaxFiltration, MaxUptake, POSFLX, GrowthResp,
     j        DetrGrazing

      LOGICAL INIT, active_grazer(ntogrz), problem
      SAVE INIT
      DATA INIT /.TRUE./

C     Segment pointers en incrementen
      DO 10 I=1,NIN
        IP(I)  = IPOINT( I)
   10 CONTINUE
C
c     Check parameters not space dependent
C
      if (init) then
        problem = .false.
        if (increm(1) .gt. 0 ) problem = .true.
        if (increm(5) .gt. 0 ) problem = .true.
        DO IFILSP = 1,NTOGRZ
          if (increm(8+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(9+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(10+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(11+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(12+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(13+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(14+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(15+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(16+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(17+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(18+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          DO I = 1,NTONUT
            if (increm(18+(IFILSP-1)*NINGRZ+I) .gt. 0 ) problem = .true.
          ENDDO
          if (increm(23+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(24+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(25+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(26+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(27+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(28+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
          if (increm(29+(IFILSP-1)*NINGRZ) .gt. 0 ) problem = .true.
        ENDDO
        DO I=1,NTOALG
          DO J=1,NTONUT-1
            if (increm(5+J*NTOALG+2*NTONUT+NTOGRZ*NINGRZ+I) .gt. 0 )
     j          problem = .true.
          ENDDO
          DO IFILSP=1,NTOGRZ
            if (increm(5+(NTONUT+IFILSP-1)*NTOALG+2*NTONUT+
     1                            NTOGRZ*NINGRZ+I) .gt. 0 )
     j          problem = .true.
            if (increm(5+(NTONUT+NTOGRZ+IFILSP-1)*NTOALG+
     1                             2*NTONUT+NTOGRZ*NINGRZ+I) .gt. 0 )
     j          problem = .true.
          ENDDO
        ENDDO
        if (problem)
     j  stop 'Error Memory Management CONSBL - Consult system manager'
      endif

      IFLUX = 0

c     Skip homogeneous grazers at old AND new biomass zero

      DO IFILSP = 1,NTOGRZ
        active_grazer(ifilsp) = .false.
        if ( increm(6+(IFILSP-1)*NINGRZ) .gt. 0 .or.
     j       (PMSA(IP(6+(IFILSP-1)*NINGRZ)).gt.1e-20 .or.
     j        PMSA(IP(7+(IFILSP-1)*NINGRZ)).gt.1e-20)      )
     j   active_grazer(ifilsp) = .true.
      ENDDO

c     Set parameters not space dependent, active grazers only

      PERIOD = PMSA(IP(1))
      GEM    = PMSA(IP(5))
      DO IFILSP = 1,NTOGRZ
       if (active_grazer(ifilsp)) then
        DETFF (IFILSP) = PMSA(IP(8+(IFILSP-1)*NINGRZ))
        DETPR (IFILSP) = PMSA(IP(9+(IFILSP-1)*NINGRZ))
        GRZFM (IFILSP) = PMSA(IP(10+(IFILSP-1)*NINGRZ))
        GRZGM (IFILSP) = PMSA(IP(11+(IFILSP-1)*NINGRZ))
        GRZML (IFILSP) = PMSA(IP(12+(IFILSP-1)*NINGRZ))
        GRZMM (IFILSP) = PMSA(IP(13+(IFILSP-1)*NINGRZ))
        GRZMO (IFILSP) = PMSA(IP(14+(IFILSP-1)*NINGRZ))
        GRZRE (IFILSP) = PMSA(IP(15+(IFILSP-1)*NINGRZ))
        GRZRM (IFILSP) = PMSA(IP(16+(IFILSP-1)*NINGRZ))
        GRZSE (IFILSP) = PMSA(IP(17+(IFILSP-1)*NINGRZ))
        FRDBOT_SAVE(IFILSP) = PMSA(IP(18+(IFILSP-1)*NINGRZ))
        DO I = 1,NTONUT
          GRZST (I,IFILSP) = PMSA(IP(18+(IFILSP-1)*NINGRZ+I))
        ENDDO
        TMPFM (IFILSP) = PMSA(IP(23+(IFILSP-1)*NINGRZ))
        TMPGM (IFILSP) = PMSA(IP(24+(IFILSP-1)*NINGRZ))
        TMPMM (IFILSP) = PMSA(IP(25+(IFILSP-1)*NINGRZ))
        TMPRE (IFILSP) = PMSA(IP(26+(IFILSP-1)*NINGRZ))
        TMPRM (IFILSP) = PMSA(IP(27+(IFILSP-1)*NINGRZ))
        TMPSE (IFILSP) = PMSA(IP(28+(IFILSP-1)*NINGRZ))
        BENTHS(IFILSP) = NINT(PMSA(IP(29+(IFILSP-1)*NINGRZ)))
       endif
      ENDDO
      DO I=1,NTOALG
        ALGSTC(1,I) = 1.0
        DO 71 J=1,NTONUT-1
          ALGSTC(J+1,I) = PMSA(IP(5+J*NTOALG+2*NTONUT+NTOGRZ*NINGRZ+I))
   71   CONTINUE
        DO 81 IFILSP=1,NTOGRZ
         if (active_grazer(ifilsp)) then
          ALGPR(I,IFILSP) = PMSA(IP(5+(NTONUT+IFILSP-1)*NTOALG+2*NTONUT+
     1                             NTOGRZ*NINGRZ+I))
          ALGFF(I,IFILSP) = PMSA(IP(5+(NTONUT+NTOGRZ+IFILSP-1)*NTOALG+
     1                             2*NTONUT+NTOGRZ*NINGRZ+I))
         endif
   81   CONTINUE
      ENDDO

c     Loop over segments

      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN

      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)

C     RESET FLUXES
      DO 31 I=1,5*NTONUT+NTOALG
        FL(I+IFLUX) = 0.0
   31 CONTINUE
C
c     Input items (potentially) dependent on space
      VOLUME = PMSA(IP(2))
      WATEMP = PMSA(IP(3))
      DEPTH  = PMSA(IP(4))
      DO 30 IFILSP = 1,NTOGRZ
       if (active_grazer(ifilsp)) then
        GRZNEW(IFILSP) = MAX(PMSA(IP(6+(IFILSP-1)*NINGRZ)),1.0E-20) *
     1                   GRZML(IFILSP)
        GRZOLD(IFILSP) = MAX(PMSA(IP(7+(IFILSP-1)*NINGRZ)),1.0E-20) *
     1                   GRZML(IFILSP)
c       Correct unit of input concentration for zoobenthos
c       Force concentration zero for zoobenthos segments without bottom
        FRDBOT(IFILSP) = FRDBOT_SAVE(IFILSP)
        IF ( BENTHS(IFILSP) .EQ. 1 ) THEN
C         Species, defined in g/m2 (typically ZOOBENTHOS)
C         Convert input unit to g/m3!!
          GRZNEW(IFILSP) = GRZNEW(IFILSP)/DEPTH
          GRZOLD(IFILSP) = GRZOLD(IFILSP)/DEPTH
C         FRDBOT(IFILSP) = FRDBOT_SAVE(IFILSP)
          CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
          IF ((IKMRK2.EQ.1).OR.(IKMRK2.EQ.2)) THEN
             GRZNEW(IFILSP) = 0.0
             FRDBOT(IFILSP) = 0.0
          ENDIF
        ENDIF
        IF (INIT) GRZOLD(IFILSP) = GRZNEW(IFILSP)
       endif
   30 CONTINUE
      DO 51 I=1,NTONUT
        DETRIT(I) = PMSA(IP(5+NTOGRZ*NINGRZ+I))
        POC(I) =    PMSA(IP(5+NTOGRZ*NINGRZ+NTONUT+I))
        DETBIO(I) = DETRIT(I)*(1.0-GEM) + POC(I)*GEM
   51 CONTINUE
      DO 61 I=1,NTOALG
        ALGBIO(I) = MAX ( PMSA(IP(5+2*NTONUT+NTOGRZ*NINGRZ+I)) ,0.0 )
   61 CONTINUE

C*******************************************************************************
C**** Processes connected to the GRAZING of algae
C***********************************************************************

C****
C*  2 Loop over the grazers
C****
      DO 200 IFILSP = 1, NTOGRZ
       if (active_grazer(ifilsp)) then
C****
C*  3 Initialize output variables
C****
          DO 40 I = 1, NTONUT
              DISFLX(I) = 0.0E0
              DETFLX(I) = 0.0E0
              BOTFLX(I) = 0.0E0
   40     CONTINUE
          DO 50 I = 1, NTOALG
              ALGFLX(I) = 0.0
   50     CONTINUE
C****
C*  4 Check if grazer growth or mortality exceeds constraints
C*    If this is the case, correct new biomass to constraint value
C****
          IF (GRZOLD(IFILSP) .GT. 0.0) THEN
c             There was biomass
              IF ((GRZNEW(IFILSP) - GRZOLD(IFILSP)) .GE. 0.0) THEN
c                 Net growth
                  IF (GRZNEW(IFILSP) .GT. GRZOLD(IFILSP) *
     &            (1.0 + GRZGM(IFILSP) * EXP(TMPGM(IFILSP) *
     &            (WATEMP - 20.0)) * PERIOD)) THEN
                      GRZNEW(IFILSP) = GRZOLD(IFILSP) *
     &                (1.0 + GRZGM(IFILSP) * EXP(TMPGM(IFILSP) *
     &                (WATEMP - 20.0)) * PERIOD)
                  ENDIF
              ELSE
c                 Net mortality
                  IF (GRZNEW(IFILSP) .LT. GRZOLD(IFILSP) *
     &            (1.0 - GRZMM(IFILSP) * EXP(TMPMM(IFILSP) *
     &            (WATEMP - 20.0)) * PERIOD)) THEN
                      GRZNEW(IFILSP) = GRZOLD(IFILSP) *
     &                (1.0 - GRZMM(IFILSP) * EXP(TMPMM(IFILSP) *
     &                (WATEMP - 20.0)) * PERIOD)
                  ENDIF
              ENDIF
          ENDIF
C****
C*  5 Calculate total available amount of food (mg C/l)
C****
          GRZFOO = DETBIO(1) * DETPR(IFILSP)
          DO 60 I = 1, NTOALG
              GRZFOO = GRZFOO + ALGBIO(I) * ALGPR(I,IFILSP)
   60     CONTINUE
C****
C*  6 Calculate grazing (1/d) rate
C****
          IF (GRZFOO .GT. 0.0) THEN

              MaxFiltration = EXP(TMPFM(IFILSP) * (WATEMP - 20.0)) *
     &        GRZFM(IFILSP) * (GRZFOO / (GRZFOO + GRZMO(IFILSP)))
              MaxUptake = EXP(TMPRM(IFILSP) * (WATEMP - 20.0)) *
     j        GRZRM(IFILSP)
              if ( MaxFiltration .lt. 1e-20 ) then
c               grazing rate limited by filtration
                GRZGRZ = GRZOLD(IFILSP) * MaxFiltration
              else
                IF (GRZFOO .LT. (MaxUptake/MaxFiltration) ) THEN
c                 grazing rate limited by filtration
                  GRZGRZ = GRZOLD(IFILSP) * MaxFiltration
                ELSE
c                 grazing rate limited by uptake
                  GRZGRZ = GRZOLD(IFILSP) * MaxUptake / GRZFOO
                ENDIF
              endif
          ELSE
              GRZGRZ = 0.0
          ENDIF
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
C*  8 Add grazing part to fluxes for algae and detritus
C****
          DO 70 I = 1, NTONUT
              DETFLX(I) = -(DETBIO(I) * GRZGRZ * DETPR(IFILSP))
   70     CONTINUE
          DO 80 I = 1, NTOALG
              ALGFLX(I) = -(ALGBIO(I) * GRZGRZ *
     &        ALGPR(I,IFILSP))
   80     CONTINUE
C****
C*  9 Add fecal fraction to fluxes (choose water/sediment detritus)
C****
          DO 100 J = 1, NTONUT
c             JvG Code is not consistent for FRDBOT/=0 or 1!
c              GRZUPT(J) = -(DETFLX(J) * (1.0 - DETFF(IFILSP)))
c              DETFLX(J) = DETFLX(J) - DETFLX(J) * DETFF(IFILSP) *
c     &                    (1. - FRDBOT(IFILSP))
c              BOTFLX(J) = BOTFLX(J) - DETFLX(J) * DETFF(IFILSP) *
c     &                    FRDBOT(IFILSP)
              DetrGrazing = DETFLX(J)
              GRZUPT(J) = -(DetrGrazing * (1.0 - DETFF(IFILSP)))
              DETFLX(J) = DETFLX(J) - DetrGrazing * DETFF(IFILSP) *
     &                    (1. - FRDBOT(IFILSP))
              BOTFLX(J) = BOTFLX(J) - DetrGrazing * DETFF(IFILSP) *
     &                    FRDBOT(IFILSP)
              DO 90 I = 1, NTOALG
                  GRZUPT(J) = GRZUPT(J) - ALGFLX(I) * ALGSTC(J,I) *
     1            (1.0 - ALGFF(I,IFILSP))
                  DETFLX(J) = DETFLX(J) - ALGFLX(I) * ALGSTC(J,I) *
     1                        ALGFF(I,IFILSP) * (1. - FRDBOT(IFILSP))
                  BOTFLX(J) = BOTFLX(J) - ALGFLX(I) * ALGSTC(J,I) *
     1                        ALGFF(I,IFILSP) * FRDBOT(IFILSP)
   90         CONTINUE
  100     CONTINUE
C****
C* 10 Calculate limiting element for growth
C****
          GRZPMX = 10.0E20
          DO 110 I = 1, NTONUT
              IF (GRZST(I,IFILSP) .GT. 0.0) THEN
                  GRZPRT = GRZUPT(I) / GRZST(I,IFILSP)
                  IF (GRZPRT .LT. GRZPMX) THEN
                      GRZPMX = GRZPRT
                  ENDIF
              ENDIF
  110     CONTINUE
C****
C* 11 Calculate routine respiration (mgC/l.d)
C****
          GrowthResp = EXP(TMPRE(IFILSP)*(WATEMP-20.))*GRZRE(IFILSP)
          DO 120 I = 1, NTONUT
              DISFLX(I) = GRZPMX * GrowthResp * GRZST(I,IFILSP)
              GRZUPT(I) = GRZUPT(I) - DISFLX(I)
  120     CONTINUE
          GRZPMX = GRZPMX * (1.0 - GrowthResp)
C****
C* 12 Calculate the standard respiration (1/d)
C****
          GRZMET = EXP(TMPSE(IFILSP) * (WATEMP - 20.0)) *
     &    GRZSE(IFILSP)
C****
C* 13 Correct for length of period (d)
C****
          DO 130 I = 1, NTOALG
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
          IF (GRZBRU .GT. GRZPMX) THEN
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
                  GRZUPT(I) = GRZUPT(I) - GRZBRU * GRZST(I,IFILSP)
C****
C* 18 If their is mortality, add the nutrients to the detritus pool
C****
              ELSE
                DETFLX(I) = DETFLX(I) - (GRZBRU * GRZST(I,IFILSP)*
     &          (1 -FRDBOT(IFILSP)))
                BOTFLX(I) = BOTFLX(I) - (GRZBRU * GRZST(I,IFILSP)*
     &          FRDBOT(IFILSP))
              ENDIF
C****
C* 19 Convert the intake not used for bruto growth to the detritus pool
C****
              DETFLX(I) = DETFLX(I) + GRZUPT(I) * (1. - FRDBOT(IFILSP))
              BOTFLX(I) = BOTFLX(I) + GRZUPT(I) * FRDBOT(IFILSP)
  150     CONTINUE
C****
C* 20 Check massbalans
C****
          DO 170 J=1,NTONUT
              ALTFLX(J) = 0.0E0
              DO 160 I=1,NTOALG
                  ALTFLX(J) = ALTFLX(J) + ALGFLX(I) * ALGSTC(J,I)
  160         CONTINUE
              GRZFLX(J) = (GRZNEW(IFILSP) - GRZOLD(IFILSP)) *
     &        GRZST(J,IFILSP)
              TOTFLX(J) = DISFLX(J) + DETFLX(J) + BOTFLX(J) +
     &        ALTFLX(J) + GRZFLX(J)
C
C             Total nutrients in PHYT:
C
              FL(4 + J + IFLUX) = FL(4 + J + IFLUX) - ALTFLX(J)/PERIOD
  170     CONTINUE
C****
C* 21 Update total fluxes and detritus and algal biomass
C****
          DO 180 I = 1, NTONUT
              FL(I + IFLUX)      = FL(I + IFLUX)      + DISFLX(I)/PERIOD
C             MvdV 981130 added division over Detr and GEM POC
              FL(I+8+IFLUX) = FL(I+8+IFLUX) + DETFLX(I)/PERIOD
     &                      * (1.0-GEM)
              FL(I+12+IFLUX) = FL(I+12+IFLUX) + DETFLX(I)/PERIOD * GEM
              FL(I+16+IFLUX) = FL(I+16+IFLUX) + BOTFLX(I)/PERIOD
  180     CONTINUE
          DO 190 I = 1, NTOALG
              FL(I + 20 + IFLUX) = FL(I + 20 + IFLUX) - ALGFLX(I)/PERIOD
  190     CONTINUE
C****
C* 23 Save biomass grazers for next time step
C****
          IF ( BENTHS(IFILSP) .EQ. 1 ) THEN
C           Zoobenthos species
C           Convert input unit to g/m2!!
            GRZNEW(IFILSP) = GRZNEW(IFILSP)*DEPTH
          ENDIF
          IF (GRZML(IFILSP).GT.0.0) THEN
            PMSA(IP(7+(IFILSP-1)*NINGRZ)) = GRZNEW(IFILSP)/GRZML(IFILSP)
          ELSE
            PMSA(IP(7+(IFILSP-1)*NINGRZ)) = 0.0
          ENDIF
C****
C* 22 End loop over filter feeders
C****
       endif
  200 CONTINUE
C
      ENDIF
      IFLUX = IFLUX + NOFLUX
      DO 20 I=1,NIN
        IP(I) = IP(I) + INCREM (  I )
   20 CONTINUE
C
 9000 CONTINUE

C     MvdV 981130 added velocity
C     Loop over nutrients
c     JvG 11-12-2009
c     Fecal pellets from zooplankton are rapidly sinking
c     to the bootm. In 2D this can be arranged
c     by producing some of the detritus as bottom detritus (FRDBOT)
c     The code below was added to emulate this in 3D
c     it contains a bug (no extra production of water detritus
c     in stead of produced bottom detritus)
c     and it contains a conceptual flaw (POC is shifted only one layer down)
c     Therefore, we omit it, and consequently neglect the rapid
c     sinking of fecal pellets in a 3D environment, they just become
c     POC. A conceptually sound solution would be to create a
c     state variable fecal pellets
c     We also reduce the computational burden in 3D models
c     (especially Z-layer with a lot of dummy exchanges)
c     NOTE also that this version does not support the production of
c     bottom detritus by filterfeeding benthos in a DELWAQ-G
c     context

c     DO 300 I = 1, NTONUT
cC      Determine pointers for velocities
c       IPV = 5+(NTONUT+2*NTOGRZ)*NTOALG+2*NTONUT+NTOGRZ*NINGRZ+NTOGRZ+I
c       INV = INCREM(IPV)
c       IPP = IPOINT(IPV)
c
cC      Exchangeloop over horizontal direction
c       DO 8000 IQ=1,NOQ1+NOQ2
cC        set horizontal velocities to zero
c         PMSA(IPP) = 0.0
c
c         IPP = IPP + INV
c
c8000   CONTINUE
c
cC      Exchangeloop over vertical direction
c       DO 7000 IQ = NOQ1+NOQ2+1 , NOQ1+NOQ2+NOQ3
cC        Calculate vertical velocities
c         IFROM  = IEXPNT(1,IQ)
c         IF ( IFROM .GT. 0 ) THEN
cC           Get total flux
c            DFLUX = FL(I+16+(IFROM-1)*NOFLUX)
cC           Get GEM POC,N.P,Si concentration
c            J = 5+NTOGRZ*NINGRZ+NTONUT+I
c            POC(I) = PMSA(IPOINT(J)+(IFROM-1)*INCREM(J))
cC           divide flux over Detritus and GEM POC
c            FL(I+16+(IFROM-1)*NOFLUX) = DFLUX * (1.0-GEM)
c            IF (GEM*POC(I).LE.1E-20) THEN
c               PMSA(IPP) = 0.
c            ELSE
cC                     convert flux (g/m3/d) to velocity (m/s)
cC             velo=flux/86400 * Cpoc / Ctot * Depth / Cpoc
cC             velo=flux/86400 * Depth / Ctot
c                  PMSA(IPP) = DFLUX / 86400. * DEPTH / POC(I)
c            ENDIF
c         ELSE
c            PMSA(IPP) = 0.0
c         ENDIF
c         IPP = IPP + INV
c
c7000   CONTINUE
c
c 300 CONTINUE

      IF (INIT) INIT = .FALSE.

      RETURN
C
      END
