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

      subroutine adspo4 ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       P-ad/desorption to particulate inorganic matter. 3 options for sorption formulation.

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |                 MCM                    |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : GEM  (T2087)
C     Author  : Rik Sonneveldt, Pascal Boderie
C     Date    : 970512             Version : 0.10
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     930322  Pascal Boderie  Create first version
C     970512  Rik SOnneveldt  Extension with option for GEM, revision of
C                             existing code.
C     970929  Rik Sonneveldt  ikmrk1 loop aangepast voor bodem
C     990310  Anouk Blauw     formulering Afree aangepast: porositeit
C                             toegevoegd
C     020220  Johannes Smits  New version (IVERSN=1) with calculation of
C                             adsorption capacity for inorganic matter
C                             fractions IM1-3,
C                             four errors with respect to porosity and
C                             pH removed,
C                             partition coefficient option 1 replaced
C                             with adsorption constant,
C                             change of names
C                             Old version maintained (IVERSN=0)
C     041108  Johannes Smits  Formulation Cphae for SWAdsP=2 corrected
C     110308  Johannes Smits  EQAAP maximized on CADST for SWAdsP=2
C
C***********************************************************************
C
C     Description of the module :
C     P-adsorption onto particulate inorganic matter. 3 options for
C     sorption formulation.
C
C        ----- old version -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C AAP     R*4 1 I AAP concentration                                [gP/m3]
C SWAdsP  R*4 1 I swithc for adsorption option to use                  [-]
C DELT    R*4 1 I DELWAQ timestep                                    [scu]
C EQAAP   R*4 1 L calculated equlibrium AAP concentration          [gP/m3]
C FL (1)  R*4 1 O adsorption/desorption flux                     [gP/m3/d]
C TIM     R*4 1 I TIM   concentration                             [gDM/m3]
C KD      R*4 1 I partition coefficent PO4-AAP             [-] or [m3/gDM]
C MAXADS  R*4 1 I maximum adsorption (capacity)                   [gP/gDM]
C PMSA(9) R*4 1 O calculated equlibrium AAP concentration          [gP/m3]
C PO4     R*4 1 I PO4 concentration                                [gP/m3]
C RCADS   R*4 1 I first order rate constatnt adsorption              [1/d]
C
C        ----- new version -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C AAP     R*4 1 I AAP concentration                                [gP/m3]
C PO4     R*4 1 I PO4 concentration                                [gP/m3]
C EQAAP   R*4 1 - calculated equlibrium AAP concentration          [gP/m3]
C KDADS   R*4 1 I distribution coeff. [-], or adsorpt. constant    [m3/gP]
C KADS20  R*4 1 I molar adsorption constant at 20 oC    [mole(a-1).l(a-1)]
C KADS    R*4 1 - molar adsorption constant             [mole(a-1).l(a-1)]
C KSORP   R*4 1 I first order rate constant for adsorption           [1/d]
C FCAP    R*4 1 I adsorption capacity for Fe                      [gP/gFe]
C FRA     R*4 1 - correction factor for oxidised iron                  [-]
C FADS    R*4 1 O adsorption/desorption flux                     [gP/m3/d]
C CADST   R*4 1 - molar total concentration adsorption sites    [moleFe/l]
C CADS    R*4 1 - molar concentration free adsorption sites     [moleFe/l]
C IM1     R*4 1 I IM1 concentration                               [gDM/m3]
C IM2     R*4 1 I IM2 concentration                               [gDM/m3]
C IM3     R*4 1 I IM3 concentration                               [gDM/m3]
C TFE     R*4 1 - total Fe concentration                          [gFe/m3]
C FRFE1   R*4 1 I fraction of Fe in IM1                          [gFe/gDW]
C FRFE2   R*4 1 I fraction of Fe in IM2                          [gFe/gDW]
C FRFE3   R*4 1 I fraction of Fe in IM3                          [gFe/gDW]
C FRFEOX  R*4 1 I fraction of oxidised Fe                              [-]
C FIM1    R*4 1 O fraction of adsorbed P in IM1                        [-]
C FIM2    R*4 1 O fraction of adsorbed P in IM2                        [-]
C FIM3    R*4 1 O fraction of adsorbed P in IM3                        [-]
C PH      R*4 1 I pH                                                   [-]
C OH      R*4 1 - hydroxyl concentration                          [mole/l]
C AOH     R*4 1 I reaction constant for hydroxyl                       [-]
C TEMP    R*4 1 I temperature                                         [oC]
C TC      R*4 1 I temperature coeffcient adsorption constant           [-]
C OXY     R*4 1 I dissolved oxygen concentration                  [gO2/m3]
C CROXY   R*4 1 I critical dissolved oxygen concentration         [gO2/m3]
C POROS   R*4 1 I prorosity                                            [-]
C DELT    R*4 1 I DELWAQ timestep                                    [scu]
C SWADSP  R*4 1 I switch for selection of the adsorption option        [-]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT REAL (A-H,J-Z)
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      INTEGER  IVERSN
      REAL     AAP   , PO4   , EQAAP , KDADS , KADS20 , KADS   , KSORP ,
     +         FCAP  , FRA   , FADS  , CADST , CADS   , IM1    , IM2   ,
     +         IM3   , TFE   , FRFE1 , FRFE2 , FRFE3  , FRFEOX , FIM1  ,
     +         FIM2  , FIM3  , PH    , OH    , AOH    , TEMP   , TC    ,
     +         OXY   , CROXY , POROS , DELT  , SWADSP , QIM1   , QIM2  ,
     +         QIM3  , EQAAPM
      INTEGER  NR_MES, ILUMON
      SAVE     NR_MES
      DATA     NR_MES / 0 /
C

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
      IP16 = IPOINT(16)
      IP17 = IPOINT(17)
      IP18 = IPOINT(18)
      IP19 = IPOINT(19)
      IP20 = IPOINT(20)
      IP21 = IPOINT(21)
      IP22 = IPOINT(22)
      IP23 = IPOINT(23)
      IP24 = IPOINT(24)
      IP25 = IPOINT(25)
      IP26 = IPOINT(26)
      IP27 = IPOINT(27)
      IP28 = IPOINT(28)
      IP29 = IPOINT(29)
      IP30 = IPOINT(30)
      IP31 = IPOINT(31)
      IP32 = IPOINT(32)
      IP33 = IPOINT(33)
      IP34 = IPOINT(34)
C
      IFLUX = 0
C
      IVERSN = NINT( PMSA( IP23) )
C
C     Use the old version when IVERSN=0
C
      DO 9000 ISEG = 1 , NOSEG

!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1.OR.IKMRK1.EQ.3) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      IF (IVERSN .EQ. 0) THEN
C
        dPAds = 0.0
        EQAAP = 0.0
        AtotP = 0.0
        Kads  = 0.0
C
        SWAdsP = PMSA(IP1 )
        PO4    = MAX( PMSA(IP2 ), 0.0)
        AAP    = MAX( PMSA(IP3 ), 0.0)
        IM1    = MAX( PMSA(IP4 ) , 0.0)
        IM2    = MAX( PMSA(IP5 ) , 0.0)
        IM3    = MAX( PMSA(IP6 ) , 0.0)
        RCADS  = PMSA(IP12)
        KD     = PMSA(IP7 )
        MAXADS = PMSA(IP8 )
        DELT   = PMSA(IP9 )
C
        TIM    = IM1 + IM2 + IM3
        QIM1 = 0.0
        QIM2 = 0.0
        QIM3 = 0.0
        IF ( TIM .LT. 1E-10) THEN
           FIM1 = 0.0
           FIM2 = 0.0
           FIM3 = 0.0
        ELSE
           FIM1 = IM1 / TIM
           FIM2 = IM2 / TIM
           FIM3 = IM3 / TIM
           IF ( IM1 .GT. 1E-10 ) QIM1 = AAP*FIM1/IM1
           IF ( IM2 .GT. 1E-10 ) QIM2 = AAP*FIM2/IM2
           IF ( IM3 .GT. 1E-10 ) QIM3 = AAP*FIM3/IM3
        ENDIF
C
C------ Error messages
C
        IF (KD.LT.0.0) CALL ERRSYS ('KD in ADSPO4 lower then zero', 1 )
        IF (((ABS(KD) .LT. 1E-20) .OR. (ABS(PO4).LT.1E-20))
     &      .AND. (ABS(MAXADS) .LT. 1E-20) )
     &     CALL ERRSYS
     &              ('(KD or PO4) and MAXADS equal zero in ADSPO4', 1 )
C
C------ (1) Instantanaeous equilibrium partitioning
C           SWAdsP = 0
C
        IF (NINT(SWAdsP) .EQ. 0) THEN
            dPAds =(((AAP + PO4) / (1.+ KD)) - AAP) /  DELT
            EQAAP = -1.0
        ENDIF
C
C------ (2) Kinetic Langmuir sorption
C           SWAdsP = 1
C
        IF (NINT(SWAdsP) .EQ. 1) THEN
          IF ( (MAXADS .LT. 1.E-10) .OR. (KD .LT. 1.E-10)) THEN
             EQAAP = 0.0
             dPAds = 0.0
          ELSE
             EQAAP = TIM * (KD * PO4 * MAXADS) / (KD * PO4 + MAXADS)
             dPAds = RCADS * (EQAAP - AAP)
          ENDIF
        ENDIF
C
C------ (3) GEM formulation for sorption
C           SWAdsP = 2
C
        IF (NINT(SWAdsP) .EQ. 2) THEN
           KAds20   = PMSA(IP10)
           TCKAds   = PMSA(IP11)
           RCadsP   = PMSA(IP25)
           aOHPO4   = PMSA(IP13)
           fr_Fe    = PMSA(IP24)
           frFeox   = PMSA(IP17)
           OXY      = PMSA(IP18)
           CrOXY    = PMSA(IP19)
           pH       = PMSA(IP20)
           Temp     = PMSA(IP21)
           poros    = PMSA(IP22)
C
C--------- (3a) Eqs 6.38, 6.37 of GEM report
           OH = 10.0**(pH-14.0)
           IF (OXY .GE. CrOXY*POROS) THEN
              fOxSor = 1.0
           ELSE
              fOxSor = frFeox
           ENDIF
C
C--------- (3b) Eqn 6.36 of GEM report : total sorption capacity (mol/l)
           AtotP = fOxSor * fr_Fe * TIM/(56000.0 * poros)
C
C--------- (3c) Eqn 6.35 of GEM report : free sorption capacity (mol/l)
           Afree = AtotP - (AAP/(31000.0 *poros))
C
C--------- (3d) Eqn 6.34 of GEM report : temp.correction of Kads
           Kads = Kads20 * TCKads**(Temp-20.0)
C
C--------- (3e) Eqn 6.33 of GEM report : equilibrium conc of Pads (gP/m3)
           EQAAP = (AAP + PO4) *
     &             (1.0 - 1.0/(Kads*Afree*(OH**aOHPO4) + 1.0))
C
C--------- (3f) Eqn 6.31 of GEM report : the adsorption flux (gP/m3/d)
           dPads = RCAdsP * (EQAAP - AAP)
C
        ENDIF
C
C---- Output of module
C
      FL(1+IFLUX) = dPAds
      PMSA(IP26)  = EQAAP
      PMSA(IP27)  = AtotP
      PMSA(IP28)  = Kads
      PMSA(IP29)  = FIM1
      PMSA(IP30)  = FIM2
      PMSA(IP31)  = FIM3
      PMSA(IP32)  = QIM1
      PMSA(IP33)  = QIM2
      PMSA(IP34)  = QIM3
C
C---- End active cells block
C
      ELSE
C
C     Use the new version when IVERSN=1
C
        SWADSP = PMSA(IP1 )
        PO4    = MAX( PMSA(IP2 ) , 0.0)
        AAP    = MAX( PMSA(IP3 ) , 0.0)
        IM1    = MAX( PMSA(IP4 ) , 0.0)
        IM2    = MAX( PMSA(IP5 ) , 0.0)
        IM3    = MAX( PMSA(IP6 ) , 0.0)
        KDADS  = PMSA(IP7 )
        FCAP   = PMSA(IP8 )
        DELT   = PMSA(IP9 )
        KSORP  = PMSA(IP12)
        FRFE1  = PMSA(IP14)
        FRFE2  = PMSA(IP15)
        FRFE3  = PMSA(IP16)
C
C     Calculation of the total concentration of iron and
C     the fractions of adsorbed phosphate in the inorganic matter
C     fractions IM1-3
C
        TFE  = FRFE1 * IM1 + FRFE2 * IM2 + FRFE3 * IM3
        QIM1 = 0.0
        QIM2 = 0.0
        QIM3 = 0.0
        IF ( TFE .LT. 1E-10) THEN
           FIM1 = 0.0
           FIM2 = 0.0
           FIM3 = 0.0
        ELSE
           FIM1 = FRFE1 * IM1 / TFE
           FIM2 = FRFE2 * IM2 / TFE
           FIM3 = FRFE3 * IM3 / TFE
           IF ( IM1 .GT. 1E-10 ) QIM1 = AAP*FIM1/IM1
           IF ( IM2 .GT. 1E-10 ) QIM2 = AAP*FIM2/IM2
           IF ( IM3 .GT. 1E-10 ) QIM3 = AAP*FIM3/IM3
        ENDIF
C
C     Error messages
C
        IF (KDADS .LT. 0.0) CALL ERRSYS ('KDADS in ADSPO4 negative', 1 )
        IF (((ABS(KDADS) .LT. 1E-20) .OR. (ABS(PO4) .LT. 1E-20))
     +     .AND. (ABS(FCAP) .LT. 1E-20) )
     +     CALL ERRSYS
     +     ('(KDADS or PO4) and FCAP equal zero in ADSPO4', 1 )
C
C     Start the calculation of the sorption flux
C     Use one of three options
C
        CADS  = 0.0
        CADST = 0.0
        EQAAP = 0.0
        FADS  = 0.0
        KADS  = 0.0
C
C     SWADSP = 0 : Instantaneous equilibrium partitioning
C
        IF (NINT(SWADSP) .EQ. 0) THEN
            FADS  =(((AAP + PO4) / (1.0 + KDADS)) - AAP) / DELT
            EQAAP = -1.0
        ENDIF
C
C     SWADSP = 1 : Langmuir sorption
C
        IF (NINT(SWADSP) .EQ. 1) THEN
          IF ( (FCAP .LT. 1E-10) .OR. (KDADS .LT. 1E-10)) THEN
             EQAAP = 0.0
             FADS  = 0.0
          ELSE
             CADST = FCAP * TFE
             EQAAP = (KDADS * CADST * PO4) / (KDADS * PO4 + 1.0)
             FADS  = KSORP * (EQAAP - AAP)
          ENDIF
        ENDIF
C
C     SWADSP = 2 : pH dependent Langmuir sorption
C
        IF (NINT(SWADSP) .EQ. 2) THEN
           KADS20   = PMSA(IP10)
           TC       = PMSA(IP11)
           AOH      = PMSA(IP13)
           FRFEOX   = PMSA(IP17)
           OXY      = PMSA(IP18)
           CROXY    = PMSA(IP19)
           PH       = PMSA(IP20)
           TEMP     = PMSA(IP21)
           POROS    = PMSA(IP22)

           IF (POROS .LT. 1E-10) CALL ERRSYS
     +        ('POROS in ADSPO4 equals zero', 1 )
           IF ( TFE .LT. 1.E-10 ) THEN
              IF ( NR_MES .LT. 25 ) THEN
                 CALL GETMLU(ILUMON)
                 NR_MES = NR_MES + 1
                 WRITE ( ILUMON , * ) 'WARNING :zero TFE in ADSPO4',
     +                                ' segment=',ISEG,' TFE=',TFE
              ENDIF
              IF ( NR_MES .EQ. 25 ) THEN
                 CALL GETMLU(ILUMON)
                 NR_MES = NR_MES + 1
                 WRITE(ILUMON,*) ' 25 WARNINGS on zero TFE'
                 WRITE(ILUMON,*) ' Further messages on algae surpressed'
              ENDIF
           ENDIF
C
C     Calculate pH dependency (hydroxyl) and factor for redox potential
C
           OH = 10.0**(PH-14.0)
           IF (OXY .GE. (CROXY*POROS) ) THEN
              FRA = 1.0
           ELSE
              FRA = FRFEOX
           ENDIF
C
C     Calculate total sorption capacity (mol/l)
C
           CADST = FRA * TFE / (56000.0 * POROS)
C
C     Calculate free sorption capacity (mol/l)
C
           CADS  = CADST - (AAP / (31000.0 * POROS) )
C
C     Calculate temperature corrected KADS
C
           KADS  = KADS20 * TC**(TEMP-20.0)
C
C     Calculate equilibrium concentration of adsorbed P (gP/m3)
C
           IF ( ABS(CADS) .LT. 1.E-20 ) THEN
              EQAAP = 0.0
           ELSE
              EQAAP = (AAP + PO4)/(1.0 + OH**AOH/(KADS*CADS))
           ENDIF
C
C     Maximize EQAAP on equivalent CADST
C
           IF ( CADS .LT. 0.0 ) THEN
              EQAAPM = 0.9 * CADST * (31000.0 * POROS)
              EQAAP = EQAAPM
           ENDIF
C
C     Calculate the adsorption flux (gP/m3/d)
C
           FADS  = KSORP * (EQAAP - AAP)

        ENDIF
C
C     Output of module
C
      FL(1+IFLUX) = FADS
      PMSA(IP26)  = EQAAP
      PMSA(IP27)  = CADST
      PMSA(IP28)  = KADS
      PMSA(IP29)  = FIM1
      PMSA(IP30)  = FIM2
      PMSA(IP31)  = FIM3
      PMSA(IP32)  = QIM1
      PMSA(IP33)  = QIM2
      PMSA(IP34)  = QIM3
C
C     End active cells block
C
      ENDIF
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
      IP7   = IP7   + INCREM (  7 )
      IP8   = IP8   + INCREM (  8 )
      IP9   = IP9   + INCREM (  9 )
      IP10  = IP10  + INCREM ( 10 )
      IP11  = IP11  + INCREM ( 11 )
      IP12  = IP12  + INCREM ( 12 )
      IP13  = IP13  + INCREM ( 13 )
      IP14  = IP14  + INCREM ( 14 )
      IP15  = IP15  + INCREM ( 15 )
      IP16  = IP16  + INCREM ( 16 )
      IP17  = IP17  + INCREM ( 17 )
      IP18  = IP18  + INCREM ( 18 )
      IP19  = IP19  + INCREM ( 19 )
      IP20  = IP20  + INCREM ( 20 )
      IP21  = IP21  + INCREM ( 21 )
      IP22  = IP22  + INCREM ( 22 )
      IP23  = IP23  + INCREM ( 23 )
      IP24  = IP24  + INCREM ( 24 )
      IP25  = IP25  + INCREM ( 25 )
      IP26  = IP26  + INCREM ( 26 )
      IP27  = IP27  + INCREM ( 27 )
      IP28  = IP28  + INCREM ( 28 )
      IP29  = IP29  + INCREM ( 29 )
      IP30  = IP30  + INCREM ( 30 )
      IP31  = IP31  + INCREM ( 31 )
      IP32  = IP32  + INCREM ( 32 )
      IP33  = IP33  + INCREM ( 33 )
      IP34  = IP34  + INCREM ( 34 )
C
 9000 CONTINUE
C
      RETURN
C
      END
