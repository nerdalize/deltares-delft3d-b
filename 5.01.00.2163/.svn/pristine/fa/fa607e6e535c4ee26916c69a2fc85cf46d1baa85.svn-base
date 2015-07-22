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

      subroutine nitrif ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Nitrification of ammonium + decay of CBOD

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  : Pascal Boderie
C     Date    : 921210             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     950104  Jos van Gils    Skewness of oxygen function added
C     921210  Pascal Boderie  Create first version, based on T721.13
C                             created by Jos van Gils
C     020220  Johannes Smits  New generic version with MM-kinetics
C                             for NH4 and OXY (IVERSN = 1)
C                             Maintained old version with O2FUNC
C                             (IVERSN = 0)
C     030807  Annette Beems   expansion with new version for TEWOR
C                             models with MONOD-kinetics for OXY
C                             (IVERSN = 2) Q2943
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        NITRIFICATION FORMULA COMPOSED OF A ZERO-ORDER TERM,
C        AND MICHAELIS-MENTEN TERMS FOR AMMONIUM AND OXYGEN
C
C        ----- old version -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C CFL     R*4 1 I constant in O2FUNC                                   [-]
C CONC    R*4 1 I ammonium concentration                            [g/m3]
C COX     R*4 1 I critical oxygen concentratio for nitrification    [g/m3]
C CRTEMP  R*4 1 I critical temperature for nitrification              [oC]
C FL (1)  R*4 1 O nitrification flux                             [gN/m3/d]
C RC      R*4 1 I first order nitrification rate                     [1/d]
C O2FUNC  R*4 1 I function for OXY effect on the nitrification rate    [-]
C OOX     R*4 1 I critical concentr. dissolved oxygen               [g/m3]
C OXY     R*4 1 I concentration of dissolved oxygen                 [g/m3]
C POROS   R*4 1 L porosity                                             [-]
C SKEWN   R*4 1 I constant in O2FUNC                                   [-]
C TC      R*4 1 I temperature coefficient for nitrification            [-]
C TEMP    R*4 1 I ambient temperature                                 [oC]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
C ZERO    R*4 1 I zeroth order nitrification rate                [gN/m3/d]
C
C        ----- new version -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C AMFUNC  R*4 1 I function for NH4 effect on the nitrification rate    [-]
C CRTEMP  R*4 1 I critical temperature for nitrification              [oC]
C CROXY   R*4 1 I critical oxygen concentratio for nitrification    [g/m3]
C FL (1)  R*4 1 O nitrification flux                             [gN/m3/d]
C K0NIT   R*4 1 I zeroth order nitrification rate                [gN/m3/d]
C K0TEMP  R*4 1 I zeroth order nitrification rate below CRTEMP   [gN/m3/d]
C K0NOX   R*4 1 I zeroth order nitrification rate below CROXY    [gN/m3/d]
C KNIT    R*4 1 I MM nitrification rate                          [gN/m3/d]
C KSAM    R*4 1 I half saturation constant for ammonium            [gN/m3]
C KSOX    R*4 1 I half saturation constant for oxygen               [g/m3]
C NH4     R*4 1 I ammonium concentration                            [g/m3]
C OXFUNC  R*4 1 I function for OXY effect on the nitrification rate    [-]
C OXY     R*4 1 I concentration of dissolved oxygen                 [g/m3]
C POROS   R*4 1 L porosity                                             [-]
C TC      R*4 1 I temperature coefficient for nitrification            [-]
C TEMP    R*4 1 I ambient temperature                                 [oC]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT NONE
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10,
     +         IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IP19
      INTEGER  IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10,
     +         IN11, IN12, IN13, IN14, IN15, IN16, IN17, IN18, IN19
      INTEGER  IFLUX, ISEG, IKMRK1
      REAL     TC     , O2FUNC , COX    , OOX    , CFL    , SKEWN  ,
     +         ZERO   , RC     , CONC
      INTEGER  IVERSN
      REAL     K0NIT  , K0OX   , K0TEMP , KNIT   , KSAM   , KSOX   ,
     +         CROXY  , NH4    , AMFUNC , OXFUNC
      REAL     POROS  , CRTEMP , OXY    , TEMP   , TEMPC  , TEMP20
      REAL     DELT
      REAL     FLNIT
      REAL     NOX_RATIO
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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF ( IKMRK1 .GT. 0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN

      IVERSN = NINT ( PMSA( IP13) )
C
C     Use new version when IVERSN = 1.0
C
      IF ( IVERSN .EQ. 1 ) THEN

            K0TEMP = PMSA(IP1 )
            NH4    = MAX ( 0.0, PMSA(IP2 ) )
            KNIT   = PMSA(IP3 )
            TC     = PMSA(IP4 )
            OXY    = MAX ( 0.0, PMSA(IP5 ) )
            KSAM   = PMSA(IP6 )
            KSOX   = PMSA(IP7 )
            TEMP   = PMSA(IP8 )
            CRTEMP = PMSA(IP9 )
            K0OX   = PMSA(IP10)
            CROXY  = PMSA(IP11)
            POROS  = PMSA(IP12)
            DELT   = PMSA(IP18)
C
C           Set the rates according to CRTEMP and CROXY
C
            IF ( TEMP .LT. CRTEMP .OR. OXY .LE. 0.0 ) KNIT = 0.0
C
                  K0NIT = 0.0
C
            IF ( TEMP .LT. CRTEMP .AND. OXY .GT. 0.0 ) THEN
                  K0NIT = K0TEMP
            ELSEIF ( TEMP .GE. CRTEMP .AND. OXY .LE. 0.0 ) THEN
                  K0NIT = K0OX
            ENDIF
C
            IF ( OXY .LE. (CROXY * POROS) ) K0NIT = 0.0
C
C           Calculate the nitrification flux
C
            TEMP20 = TEMP - 20.0
            TEMPC  = TC ** TEMP20
            AMFUNC = NH4 / ( KSAM * POROS + NH4 )
            OXFUNC = OXY / ( KSOX * POROS + OXY )
            FLNIT  = K0NIT + KNIT * TEMPC * AMFUNC * OXFUNC

C           maximise on the availebility of DO and NH4 with safety margin 0.5/0.9

            NOX_RATIO = 4.57
            FLNIT     = MIN(FLNIT,0.5*OXY/NOX_RATIO/DELT)
            FLNIT     = MIN(FLNIT,0.9*NH4/DELT)
            FL( 1 + IFLUX ) = FLNIT
C
C           Zuurstoffunctie als uitvoer
C
            PMSA(IP19) = OXFUNC
C
C
C     Use TEWOR version when IVERSN = 2.0
C
      ELSEIF ( IVERSN .EQ. 2 ) THEN

            NH4    = MAX ( 0.0, PMSA(IP2 ) )
            KNIT   = PMSA(IP14)
            OXY    = MAX ( 0.0, PMSA(IP5 ) )
            KSOX   = PMSA(IP7 )
            POROS  = PMSA(IP12)
C
C           Calculate the nitrification flux
C
            OXFUNC = OXY / ( KSOX*POROS + OXY )
            FL( 1 + IFLUX ) = KNIT * NH4 * OXFUNC
C
C           Zuurstoffunctie als uitvoer
C
            PMSA(IP19) = OXFUNC
C
C     Use old version when IVERSN = 0.0
C
      ELSE
C
            ZERO   = PMSA(IP1)
            CONC   = MAX ( 0.0, PMSA(IP2 ) )
            RC     = PMSA(IP14)
            TC     = PMSA(IP4 )
            OXY    = MAX ( 0.0, PMSA(IP5 ) )
            OOX    = PMSA(IP15)
            COX    = PMSA(IP11)
            TEMP   = PMSA(IP8 )
            CRTEMP = PMSA(IP9 )
            CFL    = PMSA(IP16)
            SKEWN  = PMSA(IP17)
            POROS  = PMSA(IP12)
C
C           Calculate oxygen function
C
            IF ( (OOX - COX) .LT. 1E-20 )  CALL ERRSYS
     &            ('OOX - COX in NITRIF zero', 1 )
            IF ( OXY .GT. (OOX*POROS) ) THEN
                  O2FUNC = 1.0
            ELSEIF (OXY .LT. (COX*POROS) ) THEN
                  O2FUNC = CFL
            ELSE
                  O2FUNC = (1.0 - CFL) * (OXY-COX*POROS)/
     &                     ((OOX-COX)*POROS) + CFL
                  O2FUNC = (1.0 - CFL) * ((OXY-COX*POROS)/
     &                     ((OOX-COX)*POROS))**10**SKEWN + CFL
            ENDIF
C
C           Calculate flux
C
            IF (TEMP .LE. CRTEMP) THEN
                  FL( 1+ IFLUX ) = ZERO
            ELSE
                  TEMP20 = TEMP - 20.0
                  TEMPC  = TC ** TEMP20
                  FL( 1 + IFLUX ) = ZERO + RC * CONC* TEMPC * O2FUNC
            ENDIF
C
C           Zuurstoffunctie als uitvoer
C
            PMSA(IP19) = O2FUNC
C
      ENDIF
C
      ENDIF
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
C
 9000 CONTINUE
C
      RETURN
C
      END
