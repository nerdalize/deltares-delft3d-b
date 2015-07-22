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

      subroutine denwat ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Denitrification in water column

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
C     921210  Pascal Boderie  Create first version, based on T721.13
C                             created by Jos van Gils
C     930527  Rik Sonneveldt  Formulation O2FUNC corrected.
C     940105  Jos van Gils    Extension O2FUNC for Sawes
C     971111  Jan van Beek    Bug verbeterd (PR 006)
C     020503  Johannes Smits  New generic version with MM-kinetics
C                             for NO3 and OXY (IVERSN = 1)
C                             Maintained old version with O2FUNC
C                             (IVERSN = 0)
C
C***********************************************************************
C
C     Description of the module :
C
C        ----- old version -----
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            ----
C CROXY   R*4 1 I critical oxy. concentration for denititrification [g/m3]
C CRTEMP  R*4 1 L critical temperature for denitrification            [oC]
C CURVA   R*4 1 L constant in oxygen function                          [-]
C DENR    R*4 1 I zero-order denitrification rate                [gN/m3/d]
C DENRC   R*4 1 I first-order denitrification rate                   [1/d]
C FL (1)  R*4 1 O denitrification flux                           [gN/m3/d]
C NO3     R*4 1 I nitrate concentration                            [gN/m3]
C O2FUNC  R*4 1 L oxygen inhibition function                           [-]
C COXDEN  R*4 1 I critical oxy. concentration for denititrification [g/m3]
C OOXDEN  R*4 1 I critical oxy. concentration for denititrification [g/m3]
C OXY     R*4 1 I concentration of dissolved oxygen                [gN/m3]
C POROS   R*4 1 L porosity                                             [-]
C TC      R*4 1 I temperature coefficient for denitrif.                [-]
C TEMP    R*4 1 I ambient temperature                                 [oC]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
C
C        ----- new version -----
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            ----
C CROXY   R*4 1 I critical oxy. concentration for denititrification [g/m3]
C CRTEMP  R*4 1 L critical temperature for denitrification            [oC]
C FL (1)  R*4 1 O denitrification flux                           [gN/m3/d]
C KDEN    R*4 1 I MM denitrification rate                        [gN/m3/d]
C K0DEN   R*4 1 I zero-order denitrification rate                [gN/m3/d]
C K0TEMP  R*4 1 I zero-order denitrification rate below CRTEMP   [gN/m3/d]
C K0OX    R*4 1 I zero-order denitrification rate below CROXY    [gN/m3/d]
C KSOX    R*4 1 L half saturation concentration for oxygen       [gN/m3/d]
C KSNI    R*4 1 L half saturation concentration for nitrate      [gN/m3/d]
C NIFUNC  R*4 1 L MM nitrate function                                  [-]
C NO3     R*4 1 I nitrate concentration                            [gN/m3]
C OXFUNC  R*4 1 L MM oxygen inhibition function                        [-]
C OXY     R*4 1 I concentration of dissolved oxygen                [gN/m3]
C POROS   R*4 1 L porosity                                             [-]
C TC      R*4 1 I temperature coefficient for denitrif.                [-]
C TEMP    R*4 1 I ambient temperature                                 [oC]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10,
     +         IP11, IP12, IP13, IP14, IP15, IP16, IP17
      INTEGER  IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10,
     +         IN11, IN12, IN13, IN14, IN15, IN16, IN17
      INTEGER  IFLUX, ISEG, IKMRK1
      REAL     TC     , DENR   , DENRC  , OOXDEN , COXDEN , CURVA  ,
     +         O2FUNC , DELTOX , CURVAQ
      INTEGER  IVERSN
      REAL     KDEN   , K0DEN  , K0TEMP , K0OX   , KSNI   , KSOX   ,
     +         CROXY  , NIFUNC , OXFUNC
      REAL     POROS  , CRTEMP , OXY    , NO3    , TEMP   , TEMPC  ,
     +         TEMP20
C
      LOGICAL  TMPOPT , OXYOPT
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
C
C     Factors that determine temperature effect space dependent?
C
      IF ( IN4 .EQ. 0 .AND. IN8 .EQ. 0 ) THEN
C
C        NO! Compute temperature effect and switch TMPOPT off
C
         TEMP   = PMSA(IP8)
         TC     = PMSA(IP4)
         TEMP20 = TEMP - 20.0
         TEMPC  = TC ** TEMP20
         TMPOPT = .FALSE.
      ELSE
C
C        YES! Switch TMPOPT on
C
         TMPOPT = .TRUE.
      ENDIF
C
C     Factors that determine oxygen effect space dependent?
C     Only relevant for old version IVERSN=0
C
      IVERSN = NINT ( PMSA( IP13) )
      IF ( IVERSN .EQ. 0 ) THEN

         IF ( IN5 .EQ. 0 .AND. IN15 .EQ. 0 .AND. IN11 .EQ. 0 .AND.
     +        IN16 .EQ. 0 .AND. IN12 .EQ. 0 ) THEN
C
C        NO! Compute oxygen effect and switch OXYOPT off
C
            OXY    = PMSA(IP5 )
            POROS  = PMSA(IP12)
            OOXDEN = PMSA(IP15)
            COXDEN = PMSA(IP11)
            DELTOX = (COXDEN - OOXDEN) * POROS
            IF ( DELTOX .LT. 1E-20 )  CALL ERRSYS
     &         ('(COXDEN - OOXDEN) in DENWAT <= zero', 1 )
            IF (OXY .GT. (COXDEN*POROS)) THEN
               O2FUNC = 0.0
            ELSEIF (OXY .LT. (OOXDEN*POROS)) THEN
               O2FUNC = 1.0
            ELSE
               CURVA  = MAX(PMSA(IP16),1.0)
               CURVAQ = - LOG(1.) + EXP(CURVA)
               O2FUNC = ( COXDEN*POROS - OXY ) /
     &                  ( DELTOX + CURVAQ*(OXY - OOXDEN*POROS) )
            ENDIF
            OXYOPT = .FALSE.
         ELSE
C
C        YES! Switch OXYOPT on
C
            OXYOPT = .TRUE.
         ENDIF
C
      ENDIF
C
C     Loop over segments
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
C     Use new version when IVERSN=1
C
      IF ( IVERSN .EQ. 1 ) THEN
C
            K0TEMP = PMSA(IP1 )
            NO3    = MAX ( 0.0, PMSA(IP2 ) )
            KDEN   = PMSA(IP3 )
            TC     = PMSA(IP4)
            OXY    = PMSA(IP5 )
            KSNI   = PMSA(IP6 )
            KSOX   = PMSA(IP7 )
            TEMP   = PMSA(IP8 )
            CRTEMP = PMSA(IP9 )
            K0OX   = PMSA(IP10)
            CROXY  = PMSA(IP11)
            POROS  = PMSA(IP12)
C
C           Set the rates according to CRTEMP and CROXY
C
            IF (TEMP .LT. CRTEMP .OR. OXY .GE. (CROXY*POROS)) KDEN = 0.0
C
            K0DEN = 0.0
C
            IF (TEMP .LT. CRTEMP .AND. OXY .LT. (CROXY * POROS)) THEN
                  K0DEN = K0TEMP
            ELSEIF (TEMP .GE. CRTEMP .AND. OXY .LT.(CROXY * POROS)) THEN
                  K0DEN = K0OX
            ENDIF
C
C           Compute space dependent temperature effect if TMPOPT on
C
            IF ( TMPOPT ) THEN
                  TEMP20 = TEMP - 20.0
                  TEMPC  = TC ** TEMP20
            ENDIF
C
C           Calculation of denitrification flux
C
            NIFUNC = NO3 / ( KSNI * POROS + NO3 )
            OXFUNC = 1.0 - OXY / ( KSOX * POROS + OXY )
            IF ( OXY .LT. 0.0 ) OXFUNC = 1.0
C
            FL( 1 + IFLUX ) = K0DEN + KDEN * TEMPC * NIFUNC * OXFUNC
C
C           Zuurstoffunctie als uitvoer
C
            PMSA(IP17) = OXFUNC
C
      ELSE
C
C     Use old version whem IVERSN=0
C
            NO3    = MAX ( 0.0, PMSA(IP2 ) )
            DENR   = PMSA(IP1)
            TEMP   = PMSA(IP8)
            CRTEMP = PMSA(IP9 )
            DENRC  = PMSA(IP14)
C
C           Compute space dependent temperature effect if TMPOPT on
C
            IF ( TMPOPT ) THEN
                  TC     = PMSA(IP4)
                  TEMP20 = TEMP - 20.0
                  TEMPC  = TC ** TEMP20
            ENDIF
            IF (TEMP .LE. CRTEMP ) DENRC = 0.0
C
C           Compute space dependent oxygen effect if OXYOPT on
C
            IF ( OXYOPT ) THEN
                  OXY    = PMSA(IP5 )
                  POROS  = PMSA(IP12)
                  OOXDEN = PMSA(IP15)
                  COXDEN = PMSA(IP11)
                  DELTOX = (COXDEN - OOXDEN) * POROS
                  IF ( DELTOX .LT. 1E-20 )  CALL ERRSYS
     &               ('(COXDEN - OOXDEN) in DENWAT <= zero', 1 )
                  IF (OXY .GT. COXDEN*POROS) THEN
                        O2FUNC = 0.0
                  ELSEIF (OXY .LT. OOXDEN*POROS) THEN
                        O2FUNC = 1.0
                  ELSE
                        CURVA  = MAX(PMSA(IP16),1.0)
                        CURVAQ = - LOG(1.) + EXP(CURVA)
                        O2FUNC = ( COXDEN*POROS - OXY ) /
     &                           ( DELTOX + CURVAQ*(OXY-OOXDEN*POROS))
                  ENDIF
C
            ENDIF
C
C           Denitrification is assumed to take place in the water
C           below a certain oxygen concentration in the water
C           Calculation of denitrification flux ( M.L-3.t-1)
C           Old:
C           O2FUNC = MAX ( 0.0 , ( COXDEN - OXY ) / DELTOX )
C           O2FUNC = (COXDEN - OXY) / (DELTOX + CURVAQ*(OXY - OOXDEN))
C           O2FUNC = MAX ( 0.0 , O2FUNC )
C           O2FUNC = MIN ( 1.0 , O2FUNC )
C           TEMFAK = DENRC * DENTC ** TEMP20
C           FL( 1 + IFLUX ) = DENR + TEMFAK * NO3 * O2FUNC
C
            FL( 1 + IFLUX ) = DENR + DENRC * TEMPC * NO3 * O2FUNC
C
C           Zuurstoffunctie als uitvoer
C
            PMSA(IP17) = O2FUNC
C
      ENDIF
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1  = IP1  + IN1
      IP2  = IP2  + IN2
      IP3  = IP3  + IN3
      IP4  = IP4  + IN4
      IP5  = IP5  + IN5
      IP6  = IP6  + IN6
      IP7  = IP7  + IN7
      IP8  = IP8  + IN8
      IP9  = IP9  + IN9
      IP10 = IP10 + IN10
      IP11 = IP11 + IN11
      IP12 = IP12 + IN12
      IP13 = IP13 + IN13
      IP14 = IP14 + IN14
      IP15 = IP15 + IN15
      IP16 = IP16 + IN16
      IP17 = IP17 + IN17
C
 9000 CONTINUE
C
      RETURN
      END
