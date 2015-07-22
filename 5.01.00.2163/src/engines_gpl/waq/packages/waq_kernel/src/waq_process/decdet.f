      subroutine decdet ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Mineralisation & conversion of detritus POC1,POC2,POC3,POC4,DOC
C
C***********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : Generiek Estuarium Model T2087
C     Author  : Rik Sonneveldt
C     Date    : 05mei97            Version : 0.00
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     970505  Rik Sonneveldt  Create first version, based on WATMIN 0.01
C                             created by Pascal Boderie/Jos van Gils
C     970929  Rik Sonneveldt  Loop ikenmerk1 aangepast
C     010713  Johannes Smits  correction with respect to nutrient
C                             accelleration and conversion fluxes
C     2009/11 Jan van Beek    merging routines for POC2 and POC3
C                             extension with conversion of POC2 to DOC
C                             and POC4
C                             extension with sulphur
C     120904  Johannes Smits  extension with option for input rates for
C                             N and P in stead of nutrient stripping
C                             merging of decfsn, decsln and decren
C                             plus prevention of dividing by zero when
C                             AUN=ALN or AUP=ALP
C                             plus correction critical concentrations
C                             for B_NO3 and B_SULF
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        Mineralization and conversion of detritus Carbon, Nitrogen,
C        Phosphorus and Sulphur.
C        Two options are available, with or without nutrient stripping.
C        Input mineralization rates can be different for C, N and P.
C        Input rates for C are also used for S.
C        Mineralization for nutrients N, P and S can be faster than for
C        C, also due to nutrient stripping. Hence C:N , C:P and C:S in
C        other detritus can be higher than in fast detritus.
C        POC, PON, POP and POS refer not only to fast, medium, slow,
C        refractory particulate detritus (POC1, POC2, POC3, POC4),
C        but also to dissolved detritus (DOC).
C        In case of POC4 and DOC some of the iput items are dummies and
C        have fixed values that lead to:
C        a) basic rate is always upper limit and equal for C,N,P,S
C        b) nutrient availability does not affact the rate
C        c) nutrient stripping is not applied
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C POC     R*4 1 I     concentration of detritus C                 [gC/m3]
C PON     R*4 1 I     concentration of detritus N                 [gN/m3]
C POP     R*4 1 I     concentration of detritus P                 [gP/m3]
C POS     R*4 1 I     concentration of detritus S                 [gS/m3]
C RC20LOC R*4 1 I     degradation rate detC at 20 oC, lower value [1/day]
C RC20UPC R*4 1 I     degradation rate detC at 20 oC, upper value [1/day]
C RC20LON R*4 1 I     degradation rate detN at 20 oC, lower value [1/day]
C RC20UPN R*4 1 I     degradation rate detN at 20 oC, upper value [1/day]
C RC20LOP R*4 1 I     degradation rate detP at 20 oC, lower value [1/day]
C RC20UPP R*4 1 I     degradation rate detP at 20 oC, upper value [1/day]
C RC20C   R*4 1 -     degradation rate detC at 20 oC              [1/day]
C RC20N   R*4 1 -     degradation rate detN at 20 oC              [1/day]
C RC20P   R*4 1 -     degradation rate detP at 20 oC              [1/day]
C RC20S   R*4 1 -     degradation rate detS at 20 oC              [1/day]
C TEMP    R*4 1 I     temperature                                    [oC]
C TC      R*4 1 I     temperature coefficient                         [-]
C TEMPC   R*4 1 -     temperature function                            [-]
C ANR     R*4 1 I     nitrogen content of refractory detritus     [gN/gC]
C APR     R*4 1 I     phosphorus content of refractory detritus   [gP/gC]
C ASR     R*4 1 I     sulfur content of refractory detritus       [gS/gC]
C ALN     R*4 1 I     nitrogen content of detritus, lower value   [gN/gC]
C ALP     R*4 1 I     phosphorus content of detritus, lower value [gN/gC]
C AUN     R*4 1 I     nitrogen content of detritus, upper value   [gN/gC]
C AUP     R*4 1 I     phosphorus content of detritus, upper value [gN/gC]
C FNUT    R*4 1 -     limiting factor for nutrient availability       [-]
C N_FACT  R*4 1 -     accelleration factor for nitrogen               [-]
C P_FACT  R*4 1 -     accelleration factor for phosphorus             [-]
C S_FACT  R*4 1 -     accelleration factor for sulphur                [-]
C OXY     R*4 1 I     concentration of dissolved oxygen          [gO2/m3]
C NO3     R*4 1 I     concentration of nitrate                    [gN/m3]
C B_NO3   R*4 1 I     attenuation constant for denitrification        [-]
C B_SULF  R*4 1 I     attenuation constant for sulfate reduction      [-]
C ELFACT  R*4 1 -     attenuation factor for electron acceptor        [-]
C B_DTP   R*4 1 I     conversion ratio for particulate detritus       [-]
C B_DTD   R*4 1 I     conversion ratio for dissolved detritus         [-]
C SWOMDEC R*4 1 I     option: 0.0 with nutrient stripping, 1.0 without
C ID      R*4 1 I     ident.=1 (POC1, POC2, POC3), indent.=2 (POC4, DOC)
C DECOC   R*4 1 O     degradation flux for detritus C         [gC/m3/day]
C DECON   R*4 1 O     degradation flux for detritus N         [gN/m3/day]
C DECOP   R*4 1 O     degradation flux for detritus P         [gP/m3/day]
C DECOS   R*4 1 O     degradation flux for detritus S         [gS/m3/day]
C CNVPC   R*4 1 O     conversion flux for part. detritus C    [gC/m3/day]
C CNVPN   R*4 1 O     conversion flux for part. detritus N    [gN/m3/day]
C CNVPP   R*4 1 O     conversion flux for part. detritus P    [gP/m3/day]
C CNVPS   R*4 1 O     conversion flux for part. detritus S    [gS/m3/day]
C CNVDC   R*4 1 O     conversion flux for diss. detritus C    [gC/m3/day]
C CNVDN   R*4 1 O     conversion flux for diss. detritus N    [gN/m3/day]
C CNVDP   R*4 1 O     conversion flux for diss. detritus P    [gP/m3/day]
C CNVDS   R*4 1 O     conversion flux for diss. detritus S    [gS/m3/day]
C
C     Logical Units : -
C
C     Modules called : -
C
C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT REAL (A-H,J-Z)
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      REAL     POC     , PON     , POP     , POS     , RC20LOC ,
     +         RC20UPC , RC20LON , RC20UPN , RC20LOP , RC20UPP ,
     +         RC20C   , RC20N   , RC20P   , RC20S   , TEMP    ,
     +         TC      , TEMPC   , ANR     , APR     , ASR     ,
     +         ALN     ,   ALP   , AUN     , AUP     , FNUT    ,
     +         N_FACT  , P_FACT  , S_FACT  , OXY     , NO3     ,
     +         B_NO3   , B_SULF  , ELFACT  , B_DTP   , B_DTD   ,
     +         DECOC   , DECON   , DECOP   , DECOS   , CNVPC   ,
     +         CNVPN   , CNVPP   , CNVPS   , CNVDC   , CNVDN   ,
     +         CNVDP   , CNVDS   , SWOMDEC , ID
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
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
      IN8  = INCREM( 8)
      IN9  = INCREM( 9)
      IN10 = INCREM(10)
      IN11 = INCREM(11)
      IN12 = INCREM(12)
      IN13 = INCREM(13)
      IN14 = INCREM(14)
      IN15 = INCREM(15)
      IN16 = INCREM(16)
      IN17 = INCREM(17)
      IN18 = INCREM(18)
      IN19 = INCREM(19)
      IN20 = INCREM(20)
      IN21 = INCREM(21)
      IN22 = INCREM(22)
      IN23 = INCREM(23)
      IN24 = INCREM(24)
      IN25 = INCREM(25)
      IN26 = INCREM(26)
      IN27 = INCREM(27)
      IN28 = INCREM(28)
      IN29 = INCREM(29)
      IN30 = INCREM(30)
      IN31 = INCREM(31)
      IN32 = INCREM(32)
      IN33 = INCREM(33)
      IN34 = INCREM(34)
C
      IFLUX = 0
C
      DO 9000 ISEG = 1 , NOSEG
C
C       In all "active" segments
C
!!      CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!      IF (IKMRK1.EQ.1.OR.IKMRK1.EQ.3) THEN
        IF (BTEST(IKNMRK(ISEG),0)) THEN
C
C          INPUT of subroutine
C
           POC     = MAX(PMSA(IP1),0.0)
           PON     = MAX(PMSA(IP2),0.0)
           POP     = MAX(PMSA(IP3),0.0)
           POS     = MAX(PMSA(IP4),0.0)
           ID      = PMSA(IP5)
C
C          all rates are equal for ID=2 (POC4 and DOC)
C
           RC20UPC = PMSA(IP6)
           IF (NINT(ID) .EQ. 2) THEN
           RC20LOC = PMSA(IP6)
           RC20UPN = PMSA(IP6)
           RC20LON = PMSA(IP6)
           RC20UPP = PMSA(IP6)
           RC20LOP = PMSA(IP6)
           ELSE
           RC20LOC = PMSA(IP7)
           RC20UPN = PMSA(IP8)
           RC20LON = PMSA(IP9)
           RC20UPP = PMSA(IP10)
           RC20LOP = PMSA(IP11)
           ENDIF
C
           TC      = PMSA(IP12)
           TEMP    = PMSA(IP13)
           ANR     = PMSA(IP14)
           APR     = PMSA(IP15)
           ASR     = PMSA(IP16)
           ALN     = PMSA(IP17)
           ALP     = PMSA(IP18)
           AUN     = PMSA(IP19)
           AUP     = PMSA(IP20)
           OXY     = PMSA(IP21)
           NO3     = PMSA(IP22)
           B_NO3   = PMSA(IP23)
           B_SULF  = PMSA(IP24)
           B_DTP   = PMSA(IP25)
           B_DTD   = PMSA(IP26)
           SWOMDEC = PMSA(IP27)
C
C          Errors if certain vars =< 0
C
           IF (ANR .LT. 1E-30) CALL ERRSYS ('DECDET: a_dNpr =< 0', 1 )
           IF (APR .LT. 1E-30) CALL ERRSYS ('DECDET: a_dPpr =< 0', 1 )
           IF (ASR .LT. 1E-30) CALL ERRSYS ('DECDET: a_dSpr =< 0', 1 )
           IF (ALN .LT. 1E-30)
     &     CALL ERRSYS ('DECDET: al_dN/F/M/S/ =< 0', 1 )
           IF (ALP .LT. 1E-30)
     &     CALL ERRSYS ('DECDET: al_dP/F/M/S/ =< 0', 1 )
           IF (AUN .LT. 1E-30)
     &     CALL ERRSYS ('DECDET: au_dN/F/M/S/ =< 0', 1 )
           IF (AUP .LT. 1E-30)
     &     CALL ERRSYS ('DECDET: au_dP/F/M/S/ =< 0', 1 )
C
C          Errors if upper limits =< lower limits
C
           IF (AUN .LT. ALN)
     &     CALL ERRSYS ('DECDET: au_dN/F/M/S/ < al_dN/F/M/S/ ',1)
           IF (AUP .LT. ALP)
     &     CALL ERRSYS ('DECDET: au_dP/F/M/S/ < al_dP/F/M/S/ ',1)
C
           IF (RC20UPC .LT. RC20LOC)
     &     CALL ERRSYS ('DECDET: ku_d/F/M/S/dec20 < kl_d/F/M/S/dec20 ',1)
           IF (RC20UPN .LT. RC20LON)
     &     CALL ERRSYS ('DECDET: ku_d/F/M/S/dcN20 < kl_d/F/M/S/dcN20 ',1)
           IF (RC20UPP .LT. RC20LOP)
     &     CALL ERRSYS ('DECDET: ku_d/F/M/S/dcP20 < kl_d/F/M/S/dcP20 ',1)
C
C          If  detritus = 0 : set fluxes to zero and skip algorithm
C
           IF (POC .LT. 1E-10 .OR. PON .LT. 1E-10 .OR. POP .LT. 1E-10)
     &        THEN
C
              DECOC = 0.0
              DECON = 0.0
              DECOP = 0.0
              DECOS = 0.0
              CNVPC = 0.0
              CNVPN = 0.0
              CNVPP = 0.0
              CNVPS = 0.0
              CNVDC = 0.0
              CNVDN = 0.0
              CNVDP = 0.0
              CNVDS = 0.0
C
           ELSE
C
C             Calculate degrad. rate at 20oC for current stochiometry
C
              IF ((PON/POC) .GT. AUN .AND. (POP/POC) .GT. AUP) THEN
C
C                -- both stoch's above upper limit
C
                 RC20C = RC20UPC
                 RC20N = RC20UPN
                 RC20P = RC20UPP
C
              ELSE IF ((PON/POC) .LT. ALN .OR. (POP/POC) .LT. ALP)
     &                THEN
C
C                -- one or both stoch's below lower limit
C
                 RC20C = RC20LOC
                 RC20N = RC20LON
                 RC20P = RC20LOP
C
              ELSE
C
C                -- both stoch's between upper and lower limit
C                   or one stoch above ul and one between ul and ll
C
                 IF (AUN .EQ. ALN .OR. AUP .EQ. ALP) THEN
                    FNUT = 0.5
                 ELSE
                    FNUT = MIN( ((PON/POC)-ALN) / (AUN-ALN) ,
     &                          ((POP/POC)-ALP) / (AUP-ALP) )
                 ENDIF
C
                 RC20C = RC20LOC + FNUT * (RC20UPC-RC20LOC)
                 RC20N = RC20LON + FNUT * (RC20UPN-RC20LON)
                 RC20P = RC20LOP + FNUT * (RC20UPP-RC20LOP)
C
              ENDIF
C
              RC20S = RC20C
C
C             Calculate correction factors
C             for temperature
C
              TEMPC = TC**(TEMP-20)
C
C             for dominant electron acceptor
C
              IF (OXY .GT. 0.1) THEN
                 ELFACT = 1.0
              ELSE IF (NO3 .GT. 0.1) THEN
                 ELFACT = B_NO3
              ELSE
                 ELFACT = B_SULF
              ENDIF
C
C             for nutrient stripping
C
              IF (NINT(SWOMDEC) .EQ. 0.0) THEN
                 N_FACT = 1.0 + ((PON/POC) - ANR) / ANR
                 P_FACT = 1.0 + ((POP/POC) - APR) / APR
                 S_FACT = 1.0 + ((POS/POC) - ASR) / ASR
                 N_FACT = MAX(N_FACT,0.5)
                 P_FACT = MAX(P_FACT,0.5)
                 S_FACT = MAX(S_FACT,0.5)
              ELSE
                 N_FACT = 1.0
                 P_FACT = 1.0
                 S_FACT = 1.0
              ENDIF
C
C             Calculate the fluxes for mineralization and conversion
C
              DECOC = RC20C * TEMPC * ELFACT * POC
              CNVPC = B_DTP * DECOC
              CNVDC = B_DTD * DECOC
C
              DECON = RC20N * TEMPC * ELFACT * N_FACT * PON
              CNVPN = (1/N_FACT) * B_DTP * DECON
              CNVDN = (1/N_FACT) * B_DTD * DECON
C
              DECOP = RC20P * TEMPC * ELFACT * P_FACT * POP
              CNVPP = (1/P_FACT) * B_DTP * DECOP
              CNVDP = (1/P_FACT) * B_DTD * DECOP
C
              DECOS = RC20S * TEMPC * ELFACT * S_FACT * POS
              CNVPS = (1/S_FACT) * B_DTP * DECOS
              CNVDS = (1/S_FACT) * B_DTD * DECOS
C
           ENDIF
C
C          OUTPUT of subroutine
C
           PMSA(IP28) = RC20C * TEMPC * ELFACT
           PMSA(IP29) = RC20N * TEMPC * ELFACT
           PMSA(IP30) = RC20P * TEMPC * ELFACT
           PMSA(IP31) = N_FACT
           PMSA(IP32) = P_FACT
           PMSA(IP33) = S_FACT
           PMSA(IP34) = DECOC
C
           FL( 1 + IFLUX) = CNVPC
           FL( 2 + IFLUX) = CNVPN
           FL( 3 + IFLUX) = CNVPP
           FL( 4 + IFLUX) = CNVPS
           FL( 5 + IFLUX) = CNVDC
           FL( 6 + IFLUX) = CNVDN
           FL( 7 + IFLUX) = CNVDP
           FL( 8 + IFLUX) = CNVDS
           FL( 9 + IFLUX) = DECOC
           FL(10 + IFLUX) = DECON
           FL(11 + IFLUX) = DECOP
           FL(12 + IFLUX) = DECOS
C
        ENDIF
C
C       Pointers ophogen
C
        IFLUX = IFLUX + NOFLUX
        IP1   = IP1   + IN1
        IP2   = IP2   + IN2
        IP3   = IP3   + IN3
        IP4   = IP4   + IN4
        IP5   = IP5   + IN5
        IP6   = IP6   + IN6
        IP7   = IP7   + IN7
        IP8   = IP8   + IN8
        IP9   = IP9   + IN9
        IP10  = IP10  + IN10
        IP11  = IP11  + IN11
        IP12  = IP12  + IN12
        IP13  = IP13  + IN13
        IP14  = IP14  + IN14
        IP15  = IP15  + IN15
        IP16  = IP16  + IN16
        IP17  = IP17  + IN17
        IP18  = IP18  + IN18
        IP19  = IP19  + IN19
        IP20  = IP20  + IN20
        IP21  = IP21  + IN21
        IP22  = IP22  + IN22
        IP23  = IP23  + IN23
        IP24  = IP24  + IN24
        IP25  = IP25  + IN25
        IP26  = IP26  + IN26
        IP27  = IP27  + IN27
        IP28  = IP28  + IN28
        IP29  = IP29  + IN29
        IP30  = IP30  + IN30
        IP31  = IP31  + IN31
        IP32  = IP32  + IN32
        IP33  = IP33  + IN33
        IP34  = IP34  + IN34
C
 9000 CONTINUE
C
      RETURN
      END
