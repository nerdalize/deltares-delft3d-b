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

      subroutine satco2 ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Saturation concentration carbon dioxide

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : WESTERSCHELDE
C     Author  : Jos van Gils
C     Date    : 950103             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     981222  Jan van Beek    Alternative formulation added
C     981215  Jos van Gils    Compute saturation concentration
C                             for all layers
C     950103  Jos van Gils    Create first version
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        COMPUTATION OF CARBON DIOXIDE SATURATION CONCENTRATION
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C CL2     R*4 1 I concentration of chloride                        [kg/m3]
C CO2SAT  R*4 1 0 saturation concentration                          [g/m3]
C PAPCO2  R*4 1 0 partial CO2 pressure                             [g/m3]
C SAL     R*4 1 I Salinity                                           [ppt]
C SWITCH  I*4 1 I Switch for formulation options                       [-]
C TEMP    R*4 1 I ambient temperature                                 [xC]

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local declarations
C
      INTEGER  SWITCH, LUNREP, IKMRK1, ISEG  , IP1   ,
     +         IP2   , IP3   , IP4   , IP5   , IP6
      REAL     CL2   , TEMP  , TEMPA , SAL   , PAPCO2,
     +         RION  , RKCO2 , TEMP2 , PART1 , PART2 ,
     +         CO2SAT, A1    , A2    , A3    , B1    ,
     +         B2    , B3
      PARAMETER ( A1 =  -58.0931   ,
     +            A2 =   90.5069   ,
     +            A3 =   22.2940   ,
     +            B1 =    0.027766 ,
     +            B2 =   -0.025888 ,
     +            B3 =    0.0050578)
C

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
C
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
!jvb  IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      CL2    = PMSA(IP1 )/1000.
      TEMP   = PMSA(IP2 )
      SWITCH = NINT(PMSA(IP3 ))
      SAL    = PMSA(IP4 )
      PAPCO2 = PMSA(IP5)
C     PAPCO2 = 3.162E-4
C

      IF ( SWITCH .EQ. 1 ) THEN

C === REAERATION CO2 ==================================================
C
C     SATURATION CONCENTRATION CO2 = PARTIAL PRESSURE CO2 IN ATMOSPHERE
C     (ASSUMED MOL/L)              * REACTION CONSTANT
C
C     PARTIAL PRESSURE = 10**-3.5 ATM (PAG. 180)
C
C     REACTION CONSTANT KCO2 = FUNCTION (ABS.TEMPERATURE,CHLORINITY)
C     ABS. TEMP  = 273.15 + TEMPD (MODEL TEMP. IN DEGREES CELSIUS)
C     CHLORINITY = 0.001*CL (CL IS MODEL CONC. CL- IN MG/L)
C
C     REF.: AQUATIC CHEMISTRY,  STUMM & MORGAN, WILEY & SONS, 1981
C
C =====================================================================

         TEMPA  = TEMP + 273.15
         RION   = 0.147E-02 + 0.3592E-01*CL2 + 0.68E-04*CL2**2
         RKCO2  = 10.0**(-( - 0.238573E+04/TEMPA + 0.140184E+02 -
     J         0.152642E-01*TEMPA + RION*(0.28569 - 0.6167E-05*TEMPA)))

      ELSEIF ( SWITCH .EQ. 2 ) THEN
C
C        Weiss volgen Monteiro (CISR)
C
         TEMP2  = (TEMP+273.)/100.
         PART1  = A1 + A2/TEMP2 + A3*LOG(TEMP2)
         PART2  = SAL*(B1+B2*TEMP2+B3*TEMP2*TEMP2)
         RKCO2  = EXP(PART1+PART2)
C
      ELSE
          CALL GETMLU(LUNREP)
          WRITE(LUNREP,*) 'ERROR in SATCO2'
          WRITE(LUNREP,*) 'Illegal option for CO2 saturation formula'
          WRITE(LUNREP,*) 'Option in input:',SWITCH
          WRITE(*,*) ' ERROR in SATCO2'
          WRITE(*,*) ' Illegal option for CO2 saturation formula'
          WRITE(*,*) ' Option in input:',SWITCH
          CALL SRSTOP(1)
      ENDIF

C     Output of calculated saturation

      CO2SAT = PAPCO2 * RKCO2 * 1000. * 44.
      PMSA (IP6) = CO2SAT
C
!jvb  ENDIF
C
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
C
 9000 CONTINUE
C
      RETURN
C
      END
