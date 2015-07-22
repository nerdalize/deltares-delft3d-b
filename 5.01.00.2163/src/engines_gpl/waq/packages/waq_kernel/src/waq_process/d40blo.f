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

      subroutine d40blo ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       BLOOM II algae module

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  : Jos van Gils
C     Date    : 940725             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     981113  Marnix vd Vat   Added Depth as alternative to Bloomdepth
C                             salinity dependend mortality added
C     980702  Jos van Gils    Bug fixed: respflux mult. with depth
C     971222  Jos van Gils    Switch for oxygen prod., sep. flux for OXY
C                             Computation of respiration
C     971217  Marnix vd Vat   MrtExAlg added, output pointers adapted
C     940725  Jos van Gils    First Version
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      USE      DATA_3DL
      USE      DATA_VTRANS

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local
C
C     Name    Type  Length   I/O  Description

C     ALGDM   R     1             Dry matter in algae (gDM/m3)
C     AMMONI  R     1             Ammonium concentration (gN/m3)
C     BIOMAS  R     NTYP          Species biomass (gC/m3)
C     BLDEP   R     1             Bloomdepth (DEPTH averaged over BLSTEP)
C     BLSTEP  R*4   1             Time step Bloom (days)
C     CHLORO  R     1             Total chlorophyl in algae (mg/m3)
C     CL      R     1             Chlorinity (gCl/m3)
C     DEPTHW  R     1             Depth (m)
C     DAYLEN  R     1             Day length (h)
C     DELTAT  R     1             Time step DELWAQ (d)
C     DEAT4   R*4   1             ??$Necessity to transfer?$
C     EXTALG  R     1             Extinction by algae (1/m)
C     EXTTOT  R     1             Total extinction (1/m)
C     FAUT    R     NTYP          Fraction autolysis per species (-)
C     FDET    R     NTYP          Fraction detritus per species (-)
C     FL(IFPROD)    NTYP_A        Primary production per type (g/m3/d)
C     FL(IFMORT)    NTYP_A        Mortality per type (g/m3/d)
C     FL(IFAUTO)    4             Autolysis fluxes per nutrient (g/m3/d)
C                                 (C, N, P, Si)
C     FL(IFDETR)    4             Detritus production per nutrient
C                                 (C, N, P, Si)  (g/m3/d)
C     FL(IFOOXP)    4             OOx production
C                                 (C, N, P, Si)  (g/m3/d)
C     FL(IFUPTA)    5             Uptake of nutrients
C                                 (CO2, NH4, NO3, PO4, SiOx)  (g/m3/d)
C     FRAMMO  R     1             Fraction of NH4 in N-Uptake (-)
C     FBOD5   R     1             BOD5/BODinf in algae (-)
C     HISTOR  L     1             Indicates call for history element at
C                                 an history output timestep
C     ID      I     1             Week number (-)
C     ISWVTR  I     1             Switch if 3DL is to be used
C     LIMFAC  R     6             Limiting factors (-)
C     LPRINO  I     1             Saves original value of LPRINT
C     LCOUPL  I     1             Flag for BLOOM II
C     LDUMPO  I     1             Saves original value of IDUMP
C     NTYP_A  I     1             Actual number of types
C     NTYP_M  I     1             Limit number of types
C     NGRO_A  I     1             Actual number of groups
C     NGRO_M  I     1             Limit number of groups
C     NSET    I     1             Counter for subroutine SETABC of BLOOM II
C     NITRAT  R     1             Nitrate (gN/m3)
C     NUPTAK  R     1             N-Uptake (gN/m3/d)
C     PHOSPH  R     1             Phosphate (gP/m3)
C     RATGRO  R     NGRO          Effective growth rate per group (1/d)
C     RATMOR  R     NGRO          Effective mortality per group (1/d)
C     RUNNAM  C*12  1             Filename consisting of runid without
C     RADIAT  R     1             Irradiation (W/m2)
C     SILICA  R     1             Silicate (gSi/m3)
C     TIMMUL  R     1             Time step multiplyer Bloom call (-)
C     TEMPER  R     1             Temperature (degrees C)
C     TOTNUT  R     4             C, N, P, Si in algae (gX/m3)

      INTEGER  NTYP_M
      PARAMETER ( NTYP_M = 30 )
      PARAMETER ( NIPFIX = 28 , NIPVAR= 26 )
      REAL     BIOMAS(NTYP_M), FAUT  (NTYP_M), FDET  (NTYP_M),
     1         ALGTYP(0:20,NTYP_M), MRTM1(NTYP_M), MRTM2(NTYP_M),
     2         MRTB1(NTYP_M), MRTB2(NTYP_M), CGROUP(NTYP_M)
      INTEGER  IFIX(NTYP_M)
C     PARAMETER ( NGRO_M = 10 )
      REAL     RATGRO(NTYP_M), RATMOR(NTYP_M)
      CHARACTER*12    RUNNAM
      LOGICAL  HISTOR, THIS
      INTEGER  NTYP_A, NGRO_A,
     J         NSET  , LCOUPL, LPRINO, LDUMPO, ID
      REAL     TIMMUL, TEMPER, RADIAT, DEPTHW, DAYLEN,
     J         AMMONI, NITRAT, PHOSPH, SILICA, DELTAT, BLSTEP,
     J         EXTTOT, DEAT4 , NUPTAK, FRAMMO, FBOD5 , EXTALG,
     J         CHLORO, TOTNUT(4)     ,                 ALGDM ,
     J         THRNH4, THRNO3, THRPO4, THRSI , RCRESP, TCRESP,
     M         BLDEP , CL    , TIC   , CO2   , KCO2  , CO2LIM
      REAL  :: LIMFAC(6+2*NTYP_M)
      INTEGER  IP1 , IP2 , IP3 , IP4 , IP5 , IP6 , IP7 , IP8 , IP9 ,
     J         IP10, IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18,
     J         IP19, IP20, IP21, IP22, IP23, IP24, IP25, IP26, IP27,
     J         IP28
     J         IO1 , IO2 , IO3 , IO4 , IO5 , IO6 , IO7  ,IO8 , IO9 ,
     J         IO10, IO11, IO12, IO13, IO14, IO15, IO16 ,IO17, IO18,
     J         IO19
      INTEGER  INIT , IFLUX, ISEG, IALG, IOFF, IP, IGRO
      INTEGER  IFAUTO, IFDETR, IFOOXP, IFUPTA, IFPROD, IFMORT
      INTEGER  ISWVTR
      INTEGER  SWBLOOMOUT
      INTEGER  SWTICCO2
      CHARACTER CDUMMY
      REAL*8 ORG_AVAILN
C
C     JVB much more variables needs to be saved, for the time being all
C
C     SAVE     INIT,RDCNT,ID
      SAVE
C
      DATA     INIT   / 1 /
      DATA     NSET   / 0 /
      DATA     LCOUPL / 1 /
C
      IF ( INIT .EQ. 1 ) THEN
c        OPEN(78,FILE='DBSDW4.DBG')
         INIT = 0
         TIMMUL = PMSA(IPOINT(1))
         DELTAT = PMSA(IPOINT(19))
         BLSTEP = TIMMUL * DELTAT
         RDCNT  = - BLSTEP
         ID = 0

C     Set logical numbers and open autonomous I/O files Bloom
C     $ Get RUNNAM from some place (DELWAQ intermediate files?)

         RUNNAM = 'bloominp.XXX'
         CALL BLFILE (RUNNAM)

C        Copy algae type properties for input
         DO 40 I=1,NTYP_M
C          BLOOMALG
           IO1 = NIPFIX + I
           IO1 = IPOINT(IO1)
C
C          Hier ook voor ulva van (g) naar (g/m3) lijkt me niet wordt
C          hier alleen naar negatieve waarde gekeken
C
           ALGTYP(0,I) = PMSA(IO1)
C          SPECALG
           IO1 = NIPFIX + 1*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('SpecAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(1,I) = NINT(PMSA(IO1))
C          FAUTALG
           IO1 = NIPFIX + 2*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('FrAutAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(15,I) = PMSA(IO1)
C          EXTVLALG
           IO1 = NIPFIX + 4*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('ExtVlAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(2,I) = PMSA(IO1)
C          DMCFALG
           IO1 = NIPFIX + 5*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('DMCFAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(3,I) = PMSA(IO1)
C          NCRALG
           IO1 = NIPFIX + 6*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('NCRAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(4,I) = PMSA(IO1)
C          PCRALG
           IO1 = NIPFIX + 7*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('PCRAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(5,I) = PMSA(IO1)
C          SCRALG
           IO1 = NIPFIX + 8*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('SCRAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(6,I) = PMSA(IO1)
C          XNCRALG
           IO1 = NIPFIX + 9*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('XNCRAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(16,I) = PMSA(IO1)
C          XPCRALG
           IO1 = NIPFIX +10*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('XPCRAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(17,I) = PMSA(IO1)
C          FNCRALG
           IO1 = NIPFIX +11*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('FNCRAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(18,I) = PMSA(IO1)
C          CHLACALG
           IO1 = NIPFIX +12*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('ChlaCAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(7,I) = PMSA(IO1)
C          PPMAXALG
           IO1 = NIPFIX + 13*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('PPMaxAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(8,I) = PMSA(IO1)
C          TCPMXALG
           IO1 = NIPFIX + 14*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('TcPMxAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(9,I) = PMSA(IO1)
C          TFPMXALG
           IO1 = NIPFIX + 15*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('TFPMxAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(10,I) = PMSA(IO1)
C          MORT0ALG
           IO1 = NIPFIX + 16*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('Mort0Alg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(11,I) = PMSA(IO1)
C          TCMRTALG
           IO1 = NIPFIX + 17*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('TcMrtAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(12,I) = PMSA(IO1)
C          MRESPALG
           IO1 = NIPFIX + 18*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('MRespAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(13,I) = PMSA(IO1)
C          TCRSPALG
           IO1 = NIPFIX + 19*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('TcRspAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(14,I) = PMSA(IO1)
C          SDMIXALG
           IO1 = NIPFIX + 20*NTYP_M + I
cjvb       set SDMIX for all types, time/space dependent
cjvb       IF (INCREM(IO1).NE.0) CALL BLSTOP('SDMixAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(19,I) = PMSA(IO1)
C          MRTEXALG
           IO1 = NIPFIX + 21*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('MrtExAlg',I)
           IO1 = IPOINT(IO1)
           ALGTYP(20,I) = PMSA(IO1)
C          FIXALG
           IO1 = NIPFIX + 25*NTYP_M + I
           IF (INCREM(IO1).NE.0) CALL BLSTOP('FixAlg',I)
           IO1 = IPOINT(IO1)
           IFIX(I) = NINT(PMSA(IO1))
   40    CONTINUE

C     Read BLOOM-input and set some parameters

         CALL BLINPU (NTYP_M,NTYP_A,NGRO_A,ALGTYP)
         IF (NTYP_A.GT.NTYP_M) GOTO 901
C        IF (NGRO_A.GT.NGRO_M) GOTO 902

C     set common CBLBAL communication with balance routines

         CALL IBLBAL ( NTYP_M, NTYP_A, ALGTYP, IPOINT(NIPFIX+1))

C     Initialize BLOOM (Unit conversions and filling of A-matrix)

         CALL BLINIT (LPRINO,LDUMPO)

C     initialise 3DLight data

         IF ( NOQ3 .GT. 0 ) THEN
            call dhnoseg(nosegw)
            call dhnolay(nolay)
            NOSEGL = NOSEGW/NOLAY
            IF ( NOSEGL*NOLAY .NE. NOSEGW ) THEN
               CALL GETMLU(LUNREP)
               WRITE(LUNREP,*) ' WARNING unstructured 3D application'
               WRITE(LUNREP,*) ' BLOOM 3D light approach not possible'
               ACTIVE_3DL = .FALSE.
               NOLAY = 1
            ELSE
               ACTIVE_3DL = .TRUE.
               IF ( .NOT. ACTIVE_3DL ) THEN ! to trick something in the debugger, (don't) use the variables here
                  NOSEG_3DL  = 0                ! number of segments, copy of NOSEG
                  NOSEGL_3DL = 0                ! number of segments per layer
                  NOLAY_3DL  = 0                ! number of layers
                  NGRO_3DL   = 0                ! number of BLOOM algae groups, copy of NGRO_A
                  ISEG_3DL   = 0                ! actual segment for which bloom is called
                  ILAY_3DL   = 0                ! actual layer for which bloom is called
                  ACTIVE_3DL = .FALSE.          ! switch indicating if 3DL functionality is active
                  EFFIC_3DL  = 0.0
               ENDIF
            ENDIF
         ELSE
            NOSEGW     = NOSEG
            NOSEGL     = NOSEG
            ACTIVE_3DL = .FALSE.
            NOLAY = 1
         ENDIF
         CALL INIT_3DL( NOSEG, NOSEGW, NOSEGL, NOLAY, NGRO_A, NTYP_A )
         ALLOCATE(IFIX_3DL(NTYP_A))
         IFIX_3DL=IFIX

C     Return after initialization

         RETURN

      ENDIF

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

      IO1  = IPOINT(NIPFIX+NIPVAR*NTYP_M+ 1)
      IO2  = IPOINT(NIPFIX+NIPVAR*NTYP_M+ 2)
      IO3  = IPOINT(NIPFIX+NIPVAR*NTYP_M+ 3)
      IO4  = IPOINT(NIPFIX+NIPVAR*NTYP_M+ 4)
      IO5  = IPOINT(NIPFIX+NIPVAR*NTYP_M+ 5)
      IO6  = IPOINT(NIPFIX+NIPVAR*NTYP_M+ 6)
      IO7  = IPOINT(NIPFIX+NIPVAR*NTYP_M+ 7)
      IO8  = IPOINT(NIPFIX+NIPVAR*NTYP_M+ 8)
      IO9  = IPOINT(NIPFIX+NIPVAR*NTYP_M+ 9)
      IO10 = IPOINT(NIPFIX+NIPVAR*NTYP_M+10)
      IO11 = IPOINT(NIPFIX+NIPVAR*NTYP_M+11)
      IO12 = IPOINT(NIPFIX+NIPVAR*NTYP_M+12)
      IO13 = IPOINT(NIPFIX+NIPVAR*NTYP_M+13)
      IO14 = IPOINT(NIPFIX+NIPVAR*NTYP_M+14)
      IO15 = IPOINT(NIPFIX+NIPVAR*NTYP_M+15)
      IO16 = IPOINT(NIPFIX+NIPVAR*NTYP_M+16)
      IO17 = IPOINT(NIPFIX+NIPVAR*NTYP_M+17)
      IO18 = IPOINT(NIPFIX+NIPVAR*NTYP_M+18)
      IO19 = IPOINT(NIPFIX+NIPVAR*NTYP_M+19)
C
      ISWVTR = NINT(PMSA(IPOINT(24)))
      IF ( ACTIVE_3DL .AND. ISWVTR .EQ. 0 ) THEN
         CALL GETMLU(LUNREP)
         WRITE(LUNREP,*) ' WARNING vertical distribution not active'
         WRITE(LUNREP,*) ' BLOOM 3D light approach not possible'
         ACTIVE_3DL = .FALSE.
      ENDIF

      TIMMUL = PMSA(IP1 )
      DELTAT = PMSA(IP19)
      BLSTEP = TIMMUL * DELTAT
      RDCNT  = RDCNT + BLSTEP
      IF ((AINT(RDCNT / 7.) + 1).NE.ID) THEN
        THIS   = .TRUE.
        ID = AINT(RDCNT / 7.) + 1
        IF (ID.GT.52) THEN
          ID = ID - 52
          RDCNT = RDCNT - 52. * 7.
        ENDIF
      ELSE
        THIS   = .FALSE.
      ENDIF

C     First segment loop set efficiencies

      DO ISEG = 1 , NOSEG
         CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!       IF (IKMRK1.EQ.1) THEN
         IF (BTEST(IKNMRK(ISEG),0)) THEN
            CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
            ISEG_3DL = ISEG
            ILAY_3DL = (ISEG-1)/NOSEGL_3DL+1
            EXTTOT = PMSA(IP2 )
            EXTALG = PMSA(IP3 )
            TEMPER = PMSA(IP4 )
            RADIAT = PMSA(IP5 ) * 60.48
            IF ( IKMRK1 .EQ. 3 ) THEN
               RADIAT = 0.0                    ! WAQ-G bodem geen groei
               CALL BL_NO_AUTOLYSE(ORG_AVAILN) ! WAQ-G bodem geen autolyse
            ENDIF
            DEPTH  = PMSA(IP6 )
            BLDEP  = PMSA(IP7 )
            DAYLEN = PMSA(IP8 ) * 24.
            DEPTHW = DEPTH
            IF (BLDEP.GT.0.) DEPTHW = BLDEP
            CL     = PMSA(IP22)

            ! CO2 limitation

            TIC      = MAX(0.0,PMSA(IP25))
            CO2      = MAX(0.0,PMSA(IP26))
            SWTICCO2 = NINT(PMSA(IP27))
            KCO2     = PMSA(IP28)

            ! use tic or co2 depending on the switch

            IF ( SWTICCO2 .EQ. 1 ) THEN
               TIC  = CO2*12./44.
            ENDIF

            ! set limitation

            IF ( KCO2 .GT. 1.0E-20 ) THEN
               CO2LIM = MIN(1.0,TIC/KCO2)
            ELSE
               CO2LIM = 1.0
            ENDIF

!nt2
            DO IALG = 1,NTYP_A
!nt2           scale PP with co2 limitation
               PPMCO2 = ALGTYP(8,IALG)*CO2LIM
               CALL BLSPPM(IALG,PPMCO2)
!nt2

cjvb           set SDMIX for all types, time/space dependent
C              SDMIXALG
               IOFF = NIPFIX + 20*NTYP_M + IALG
               IP = IPOINT(IOFF) + (ISEG-1)*INCREM(IOFF)
               SDMIXN = PMSA(IP)
               CALL BLSSDM(IALG,SDMIXN)
cjvb
               IF (IFIX(IALG).LT.0) THEN
C
C                 No PP for fixed ulva in non bottom segment
C
                  IF ((IKMRK2.EQ.1).OR.(IKMRK2.EQ.2)) THEN
                     CALL BLSPPM(IALG,0.0)
                  ENDIF
               ENDIF

               IOFF = NIPFIX + NTYP_M*22
               IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
               MRTM2 (IALG) = PMSA(IP)
               IOFF = NIPFIX + NTYP_M*23
               IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
               MRTB1 (IALG) = PMSA(IP)
               IOFF = NIPFIX + NTYP_M*24
               IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
               MRTB2 (IALG) = PMSA(IP)
            ENDDO
            CALL BLCLST (MRTM1,MRTM2,MRTB1,MRTB2,NTYP_A,CL)

            CALL SET_EFFI( TEMPER, RADIAT, EXTTOT, DEPTHW, DAYLEN,
     +                     ID    )
c           EXTNOP = EXTTOT - EXTALG
c           CALL SET_EFFINOP( TEMPER, RADNOP, EXTNOP, DEPTHW, DAYLEN,
c    +                        ID    )

            IF ( IKMRK1 .EQ. 3 ) THEN
               CALL BL_RESTORE_AUTOLYSE(ORG_AVAILN) ! WAQ-G restore autolyse
            ENDIF
            CALL BLCLRS (MRTM1,NTYP_A)
!nt2        reset PPMAX anyhow
            DO IALG = 1,NTYP_A
               CALL BLSPPM(IALG,ALGTYP(8,IALG))
            ENDDO
            IF ((IKMRK2.EQ.1).OR.(IKMRK2.EQ.2)) THEN
               DO IALG = 1,NTYP_A
                  IF (IFIX(IALG).LT.0) THEN
                     CALL BLSPPM(IALG,ALGTYP(8,IALG))
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
         IP2  = IP2  + INCREM( 2)
         IP3  = IP3  + INCREM( 3)
         IP4  = IP4  + INCREM( 4)
         IP5  = IP5  + INCREM( 5)
         IP6  = IP6  + INCREM( 6)
         IP7  = IP7  + INCREM( 7)
         IP8  = IP8  + INCREM( 8)
         IP22 = IP22 + INCREM(22)
         IP25 = IP25 + INCREM(25)
         IP26 = IP26 + INCREM(26)
         IP27 = IP27 + INCREM(27)
         IP28 = IP28 + INCREM(28)
      ENDDO
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP22 = IPOINT(22)
      IP25 = IPOINT(25)
      IP26 = IPOINT(26)
      IP27 = IPOINT(27)
      IP28 = IPOINT(28)

C     Second segment loop, actual bloom call

      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
      CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
      IF (IKMRK1.EQ.1 .OR. IKMRK1.EQ.3) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
C
      ISEG_3DL = ISEG
      ILAY_3DL = (ISEG-1)/NOSEGL_3DL+1
C
      TIMMUL = PMSA(IP1 )
      EXTTOT = PMSA(IP2 )
      EXTALG = PMSA(IP3 )
      TEMPER = PMSA(IP4 )

C     Conversion from standard Delwaq 4.0 [W/m2] to [J/cm2/week] for Bloom

      RADIAT = PMSA(IP5 ) * 60.48
      IF ( IKMRK1 .EQ. 3 ) THEN
         RADIAT = 0.0                    ! WAQ-G bodem geen groei
         CALL BL_NO_AUTOLYSE(ORG_AVAILN) ! WAQ-G bodem no autolyse
      ENDIF
      DEPTH  = PMSA(IP6 )
      BLDEP  = PMSA(IP7 )
C     Replace DEPTHW with BLDEP if BLDEP > 0.0
      DEPTHW = DEPTH
      IF (BLDEP.GT.0.) DEPTHW = BLDEP

C     Conversion from standard Delwaq 4.0 [d] to [h] for Bloom

      DAYLEN = PMSA(IP8 ) * 24.
      IF (DAYLEN.GT.24.) GOTO 903
      AMMONI = PMSA(IP9 )
      NITRAT = PMSA(IP10)
      PHOSPH = PMSA(IP11)
      SILICA = PMSA(IP12)
      THRNH4 = PMSA(IP13)
      THRNO3 = PMSA(IP14)
      THRPO4 = PMSA(IP15)
      THRSI  = PMSA(IP16)
      DETN   = PMSA(IP17)
      DETP   = PMSA(IP18)
      DELTAT = PMSA(IP19)
c     write (*,*) iseg, ammoni, nitrat, phosph, silica
      SWBLOOMOUT = NINT(PMSA(IP20))
      IF ((SWBLOOMOUT.NE.0).AND.(THIS)) THEN
        HISTOR = .TRUE.
      ELSE
        HISTOR = .FALSE.
      ENDIF
      CL     = PMSA(IP22)
      VOLUME = PMSA(IP23)

      ! CO2 limitation

      TIC      = MAX(0.0,PMSA(IP25))
      CO2      = MAX(0.0,PMSA(IP26))
      SWTICCO2 = NINT(PMSA(IP27))
      KCO2     = PMSA(IP28)

      ! use tic or co2 depending on the switch

      IF ( SWTICCO2 .EQ. 1 ) THEN
         TIC  = CO2*12./44.
      ENDIF

      ! set limitation

      IF ( KCO2 .GT. 1.0E-20 ) THEN
         CO2LIM = MIN(1.0,TIC/KCO2)
      ELSE
         CO2LIM = 1.0
      ENDIF

C     SUBTRACT THRESHOLDS FROM DISSOLVED CONCENTRATION, NOT BELOW ZERO,
C     BUT BELOW ZERO IF ORIGINAL CONCENTRATION BELOW ZERO
      AMMONI = MAX(MIN(AMMONI,0.0),AMMONI - THRNH4)
      NITRAT = MAX(MIN(NITRAT,0.0),NITRAT - THRNO3)
      PHOSPH = MAX(MIN(PHOSPH,0.0),PHOSPH - THRPO4)
      SILICA = MAX(MIN(SILICA,0.0),SILICA - THRSI )

      DO 20 IALG = 1,NTYP_A

!nt2      scale PP with co2 limitation
          PPMCO2 = ALGTYP(8,IALG)*CO2LIM
          CALL BLSPPM(IALG,PPMCO2)
!nt2
cjvb     set SDMIX for all types, time/space dependent
C        SDMIXALG
         IOFF = NIPFIX + 20*NTYP_M + IALG
         IP = IPOINT(IOFF) + (ISEG-1)*INCREM(IOFF)
         SDMIXN = PMSA(IP)
         CALL BLSSDM(IALG,SDMIXN)
cjvb
c         scale ulva from (g) to (g/m3)
c
          IOFF = NIPFIX
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          IF (IFIX(IALG).LT.0) THEN
             BIOMAS(IALG) = PMSA(IP)/DEPTH
C
C            No PP for fixed ulva in non bottom segment
C
             IF ((IKMRK2.EQ.1).OR.(IKMRK2.EQ.2)) THEN
                CALL BLSPPM(IALG,0.0)
             ENDIF
          ELSE
             BIOMAS(IALG) = PMSA(IP)
          ENDIF
cjvb
          IOFF = NIPFIX + NTYP_M*2
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          FAUT  (IALG) = PMSA(IP)
          IOFF = NIPFIX + NTYP_M*3
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          FDET  (IALG) = PMSA(IP)
          IOFF = NIPFIX + NTYP_M*22
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          MRTM2 (IALG) = PMSA(IP)
          IOFF = NIPFIX + NTYP_M*23
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          MRTB1 (IALG) = PMSA(IP)
          IOFF = NIPFIX + NTYP_M*24
          IP = IPOINT(IOFF+IALG) + (ISEG-1)*INCREM(IOFF+IALG)
          MRTB2 (IALG) = PMSA(IP)

          IF ( IKMRK1 .EQ. 3 ) THEN ! WAQG bodem no autolyses
             FDET(IALG) = FDET(IALG)+FAUT(IALG)
             FAUT(IALG) = 0.0
          ENDIF

   20 CONTINUE

      IFAUTO = IFLUX + 1
      IFDETR = IFLUX + 5
      IFOOXP = IFLUX + 9
      IFUPTA = IFLUX + 13
      IFPROD = IFLUX + 23
      IFMORT = IFLUX + 23 + NTYP_M
C
C     Set output control variables
C     $ How can we couple this to the DELWAQ history flag?
C     HISTOR should be true for history elements at history times
C     Present .true. gives independent output of Bloom
C     .false. prohibits independent output of Bloom


CJVB  tijdelijk altijd om de weekcyclus te negeren
cjvb  HISTOR = .TRUE.
      CALL BLOUTC (HISTOR,LPRINO,LDUMPO)

C     Salinity dependend mortality
C     Adapt mortality rates

      CALL BLCLST (MRTM1,MRTM2,MRTB1,MRTB2,NTYP_A,CL)

C     Compute mortality

      CALL BLMORT ( BIOMAS        , TEMPER        , FAUT          ,
     J              FDET          , FL(IFAUTO)    , FL(IFDETR)    ,
     J              FL(IFOOXP)    , FL(IFMORT)    , DEAT4         ,
     J              BLSTEP                                        )

C     Compute primary production and nutrient uptake

c     WRITE(78,'(I3,74E12.3)') ISEG,(PMSA(IPOINT(I)),I=1,74)

      CALL BLPRIM ( BIOMAS        , AMMONI        , NITRAT        ,
     J              PHOSPH        , SILICA        , DETN          ,
     M              DETP          , FL(IFMORT)    ,
     J              FL(IFDETR)    , BLSTEP        , EXTTOT        ,
     J              EXTALG        , TEMPER        , RADIAT        ,
     J              DEPTHW        , DAYLEN        , ID            ,
     J              LCOUPL        , NSET          , DEAT4         ,
     J              TOTNUT        , CHLORO        , FL(IFPROD)    ,
     J              FL(IFUPTA)    , LIMFAC        , NUPTAK        ,
     J              FRAMMO        , FBOD5         , RATGRO        ,
     J              RATMOR        , ALGDM         , ISEG          ,
     J              CGROUP        )

C     Copy C-uptake flux to seperate flux for Oxygen

      IF ( NINT(PMSA(IP21)).EQ.0 )
     JFL(IFUPTA+9) = FL(IFUPTA)

C     Salinity dependend mortality
C     Reset mortality rates

      CALL BLCLRS (MRTM1,NTYP_A)

!nt2  reset PPMAX anyhow
      DO IALG = 1,NTYP_A
         CALL BLSPPM(IALG,ALGTYP(8,IALG))
      ENDDO

C     Reset PPMAX for ulva-fixed if necessary

      IF ((IKMRK2.EQ.1).OR.(IKMRK2.EQ.2)) THEN
         DO IALG = 1,NTYP_A
            IF (IFIX(IALG).LT.0) THEN
               CALL BLSPPM(IALG,ALGTYP(8,IALG))
            ENDIF
         ENDDO
      ENDIF
      IF ( IKMRK1 .EQ. 3 ) THEN
         CALL BL_RESTORE_AUTOLYSE(ORG_AVAILN) ! WAQ-G restore autolyse
      ENDIF

      PMSA(IO1 ) = NUPTAK
      PMSA(IO2 ) = FRAMMO
      PMSA(IO3 ) = TOTNUT(1)
      PMSA(IO4 ) = TOTNUT(2)
      PMSA(IO5 ) = TOTNUT(3)
      PMSA(IO6 ) = TOTNUT(4)
      PMSA(IO7 ) = ALGDM
      PMSA(IO8 ) = FBOD5
      PMSA(IO9 ) = CHLORO
      PMSA(IO10) = CHLORO
      PMSA(IO11) = LIMFAC(1)
      PMSA(IO12) = LIMFAC(2)
      PMSA(IO13) = LIMFAC(3)
      PMSA(IO14) = LIMFAC(4)
      PMSA(IO15) = LIMFAC(5)
      PMSA(IO16) = LIMFAC(6)
      PMSA(IO17) = FL(IFUPTA)*DEPTHW
      PMSA(IO19) = FL(IFUPTA+7)*DEPTHW

      PMSA(IO18) = 0.0
      DO 30 IGRO = 1,NTYP_A
          IOFF = NIPFIX + NIPVAR*NTYP_M + 19
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          PMSA(IP) = RATGRO(IGRO)
          IOFF = NIPFIX + NIPVAR*NTYP_M + 19 + NTYP_M
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          PMSA(IP) = RATMOR(IGRO)
          IOFF = NIPFIX + NIPVAR*NTYP_M + 19 + 2*NTYP_M
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          PMSA(IP) = CGROUP(IGRO)

          IOFF = NIPFIX + 18*NTYP_M
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          RCRESP = PMSA(IP)
          IOFF = NIPFIX + 19*NTYP_M
          IP = IPOINT(IOFF+IGRO) + (ISEG-1)*INCREM(IOFF+IGRO)
          TCRESP = PMSA(IP)
          PMSA(IO18) = PMSA(IO18) + RCRESP*TCRESP**TEMPER*BIOMAS(IGRO)
   30 CONTINUE
      PMSA(IO18) = PMSA(IO18)*DEPTHW

      ENDIF
C
      IFLUX = IFLUX + NOFLUX
C
      IP1  = IP1  + INCREM( 1)
      IP2  = IP2  + INCREM( 2)
      IP3  = IP3  + INCREM( 3)
      IP4  = IP4  + INCREM( 4)
      IP5  = IP5  + INCREM( 5)
      IP6  = IP6  + INCREM( 6)
      IP7  = IP7  + INCREM( 7)
      IP8  = IP8  + INCREM( 8)
      IP9  = IP9  + INCREM( 9)
      IP10 = IP10 + INCREM(10)
      IP11 = IP11 + INCREM(11)
      IP12 = IP12 + INCREM(12)
      IP13 = IP13 + INCREM(13)
      IP14 = IP14 + INCREM(14)
      IP15 = IP15 + INCREM(15)
      IP16 = IP16 + INCREM(16)
      IP17 = IP17 + INCREM(17)
      IP18 = IP18 + INCREM(18)
      IP19 = IP19 + INCREM(19)
      IP20 = IP20 + INCREM(20)
      IP21 = IP21 + INCREM(21)
      IP22 = IP22 + INCREM(22)
      IP23 = IP23 + INCREM(23)
      IP24 = IP24 + INCREM(24)
      IP25 = IP25 + INCREM(25)
      IP26 = IP26 + INCREM(26)
      IP27 = IP27 + INCREM(27)
      IP28 = IP28 + INCREM(28)

      IO1  = IO1   + INCREM(NIPFIX+NIPVAR*NTYP_M+ 1)
      IO2  = IO2   + INCREM(NIPFIX+NIPVAR*NTYP_M+ 2)
      IO3  = IO3   + INCREM(NIPFIX+NIPVAR*NTYP_M+ 3)
      IO4  = IO4   + INCREM(NIPFIX+NIPVAR*NTYP_M+ 4)
      IO5  = IO5   + INCREM(NIPFIX+NIPVAR*NTYP_M+ 5)
      IO6  = IO6   + INCREM(NIPFIX+NIPVAR*NTYP_M+ 6)
      IO7  = IO7   + INCREM(NIPFIX+NIPVAR*NTYP_M+ 7)
      IO8  = IO8   + INCREM(NIPFIX+NIPVAR*NTYP_M+ 8)
      IO9  = IO9   + INCREM(NIPFIX+NIPVAR*NTYP_M+ 9)
      IO10 = IO10  + INCREM(NIPFIX+NIPVAR*NTYP_M+10)
      IO11 = IO11  + INCREM(NIPFIX+NIPVAR*NTYP_M+11)
      IO12 = IO12  + INCREM(NIPFIX+NIPVAR*NTYP_M+12)
      IO13 = IO13  + INCREM(NIPFIX+NIPVAR*NTYP_M+13)
      IO14 = IO14  + INCREM(NIPFIX+NIPVAR*NTYP_M+14)
      IO15 = IO15  + INCREM(NIPFIX+NIPVAR*NTYP_M+15)
      IO16 = IO16  + INCREM(NIPFIX+NIPVAR*NTYP_M+16)
      IO17 = IO17  + INCREM(NIPFIX+NIPVAR*NTYP_M+17)
      IO18 = IO18  + INCREM(NIPFIX+NIPVAR*NTYP_M+18)
      IO19 = IO19  + INCREM(NIPFIX+NIPVAR*NTYP_M+19)
C
 9000 CONTINUE
C
      RETURN
C
  901 STOP 'ERROR D40BLO: DIMENSION NTYP_M TOO SMALL'
C 902 STOP 'ERROR D40BLO: DIMENSION NGRO_M TOO SMALL'
  903 STOP 'ERROR D40BLO: DAYLEN > 1.0 DAY'
      END

      SUBROUTINE BLSTOP(MES,I)

      CHARACTER*12 MES

      WRITE(*,*) 'ERROR IN BLOOM: '
      WRITE(*,*) 'CHARACTERISTIC ',MES,' FOR ALGAE TYPE ',I,
     1           ' MUST BE A CONSTANT!'

      CALL SRSTOP(1)

      RETURN

      END
      SUBROUTINE BL_NO_AUTOLYSE(ORG_AVAILN)
      REAL*8 ORG_AVAILN
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      ORG_AVAILN = AVAILN
      AVAILN     = 0.0
      END
      SUBROUTINE BL_RESTORE_AUTOLYSE(ORG_AVAILN)
      REAL*8 ORG_AVAILN
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      AVAILN     = ORG_AVAILN
      END
