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

      subroutine trcoef ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Gas and liquid exchange organic micro pollutants (Lyman and O'Conner)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.80
C     Author  : Jan van Beek
C     Date    : 930326             Version : 1.0
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     930326  Jan Van Beek    Create first version
C     930326  Pascal Boderie  Create 2 options for calc of kl an kg
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        Calculation of transport coefficients for air-water exhchange
C        as a function of:
C        sw=0: wind, veloc and molmass (Chemical estimates, Lyman)
C        sw=1: veloc, diffusion coefficients (IMPAQT)= O'Conner formulas
C
C Name    T   L I/O   Description                                   Uni
C ----    --- -  -    -------------------                            --
C SWITCH  R*4 1 I  Switch for calculation method                     [-]
C WIND    R*4 1 I  Windspeed                                       [m/s]
C VELOC   R*4 1 I  Flow velocity                                   [m/s]
C M       R*4 1 I  Molecuulmassa omive                           [g/mol]
C LDIF    R*4 1 O  molekular diffusion coefficient waterphase     [m2/d]
C GDIF    R*4 1 O  molekular diffusion coefficient gasphase       [m2/d]
C KL      R*4 1 O  mass transport coefficient waterphase           [m/d]
C KG      R*4 1 O  mass transport coefficient gasphase             [m/d]
C VL      R*4 1 LC viscosity waterphase                    [Pa/s=kg/m/s]
C VG      R*4 1 LC viscosity gashase                       [Pa/s=kg/m/s]
C RHOL    R*4 1 LC density waterphase                            [kg/m3]
C RHOG    R*4 1 LC density gasphase                              [kg/m3]
C TEMP    R*4 1 I  Temperatuur
C DEPTH   R*4 1 I  Diepte
C-----------------------------------------------------------------------

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      LOGICAL  WOROPT , WNDOPT , TMPOPT
C
C     Local declarations, constants in source
C
      PARAMETER ( C1     =    18.     ,
     +            C2     =     5.64   ,
     +            C3     =     0.969  ,
     +            C4     =     0.673  ,
     +            C5     =    32.     ,
     +            C6     =     0.526  ,
     +            C7     =     0.7    ,
     +            CRIT1  =     1.9    ,
     +            CRIT2  =     5.0    ,
     +            E      =     2.718  ,
     +            KELVIN =   273.15   ,
     +            VCMIN  =     0.001  ,
     +            C11    =     1.293  ,
     +            C12    =     0.00367,
     +            C13    =  1000.0    ,
     +            C14    =     0.088  ,
     +            C15    =     1.32   ,
     +            C16    =     0.009  ,
     +            C17    =     1.E-5  ,
     +            C18    =     0.001  ,
     +            C21    =     0.01   ,
     +            C22    =     6.1    ,
     +            C23    =     0.63   ,
     +            C24    =     0.001  ,
     +            C25    =     0.0463 ,
     +            C26    =     0.67   ,
     +            C27    =     1.E-6  ,
     +            C28    =     0.0144 ,
     +            C29    =     0.00341,
     +            C30    =     2.2    ,
     +            CRIT3  =     0.3     )
C
      IN2  = INCREM( 2)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
      IN8  = INCREM( 8)
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
C
      EXP1 = EXP ( C6*(CRIT2-CRIT1) )
      IF ( IN2 .EQ. 0 ) THEN
         WIND   = PMSA(IP2 )
      IF( WIND  .LT. 0.0   ) CALL ERRSYS ('WIND       in TRCOEF < 0', 1)
         IF ( WIND .GE. CRIT1 ) THEN
            IF ( WIND .LT. CRIT2 ) THEN
               EXP2   = EXP ( C6*(WIND-CRIT1) )
            ELSE
               FAC1   = ( 1. + (WIND - CRIT2)**C7 ) * EXP1
            ENDIF
         ENDIF
C     Calculate wind at watersurface from wind at 10m (m/s)
         FWIND  = C21 * WIND * SQRT( C22 + C23 * WIND)
         IF ( FWIND .LT. CRIT3  ) THEN
            FWIN2 = C28 * FWIND**C30
         ELSE
            FWIN2 = C29 * FWIND
         ENDIF
         WNDOPT = .FALSE.
      ELSE
         WNDOPT = .TRUE.
      ENDIF
C
      IF ( IN4 .EQ. 0 ) THEN
         M      = PMSA(IP4 )
      IF( M     .LT. 1.E-30) CALL ERRSYS ('MOLMASS    in TRCOEF = 0', 1)
         WORTL1 = SQRT(C1/M)
         WORTL5 = SQRT(C5/M)*C2
         WOROPT = .FALSE.
      ELSE
         WOROPT = .TRUE.
      ENDIF
C
      IF ( IN5 .EQ. 0 .AND. IN6 .EQ. 0 .AND. IN8 .EQ. 0 ) THEN
         TEMP   = PMSA(IP8 )
C--calculation of bulk densitys of water and air:
         RHOG = C11 / (1. + C12 * TEMP)
         RHOL = C13 - C14 * TEMP
C--calculation of viscosities of water and air:
         VG = (C15 + C16 * TEMP) * C17
         VL = C18
         LDIF   = PMSA(IP5 )
         GDIF   = PMSA(IP6 )
      IF( GDIF  .LT. 1.E-30) CALL ERRSYS ('GAS-DIFF   in TRCOEF = 0', 1)
      IF( LDIF  .LT. 1.E-30) CALL ERRSYS ('WATER-DIFF in TRCOEF = 0', 1)
C     Calculate Schmidt numbers for water and gas
         SCG    = VG / ( RHOG * GDIF / 86400.)
         SCL    = VL / ( RHOL * LDIF / 86400.)
         TMPOPT = .FALSE.
      ELSE
         TMPOPT = .TRUE.
      ENDIF
C
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.1)) THEN
C
C     Map PMSA on local variables
C
      ISWTCH = PMSA(IP1 ) + 0.5
C
      IF ( ISWTCH .EQ. 0 ) THEN
C
         VELOC  = PMSA(IP3 )
      IF( VELOC .LT. 0.0   ) CALL ERRSYS ('VELOC      in TRCOEF < 0', 1)
         DEPTH  = PMSA(IP7 )
C
         IF ( WNDOPT ) THEN
            WIND  = PMSA(IP2 )
      IF( WIND  .LT. 0.0   ) CALL ERRSYS ('WIND       in TRCOEF < 0', 1)
            IF ( WIND .GE. CRIT1 ) THEN
               IF ( WIND .LT. CRIT2 ) THEN
                  EXP2   = EXP ( C6*(WIND-CRIT1) )
               ELSE
                  FAC1   = ( 1. + (WIND - CRIT2)**C7 ) * EXP1
               ENDIF
            ENDIF
         ENDIF
      IF( WIND  .LT. 0.0   ) CALL ERRSYS ('WIND       in TRCOEF < 0', 1)
C
         IF ( WOROPT ) THEN
            M      = PMSA(IP4 )
      IF( M     .LT. 1.E-30) CALL ERRSYS ('MOLMASS    in TRCOEF = 0', 1)
            WORTL1 = SQRT(C1/M)
            WORTL5 = SQRT(C5/M)*C2
         ENDIF
C
C     Ensure that VELOC is >= VCMIN
C
         VELOC = MAX ( VELOC , VCMIN )
C
C     Gasphase exchange coefficient
C
         KG    = KELVIN * (WIND + VELOC) * WORTL1
C
C     Water exchange coefficient
C
         IF ( WIND .LT. CRIT1  ) THEN
            KL  = VELOC**C3/DEPTH**C4*WORTL5
         ELSEIF ( WIND .LT. CRIT2 ) THEN
            KL  = VELOC**C3/DEPTH**C4*WORTL5*EXP2
         ELSE
            KL  = VELOC**C3/DEPTH**C4*WORTL5*FAC1
         ENDIF
C
      ENDIF


      IF ( ISWTCH .EQ. 1 ) THEN
C --- Impact formulations (O'connor personal communication?)
C
         IF ( WNDOPT ) THEN
            WIND  = PMSA(IP2 )
      IF( WIND  .LT. 0.0   ) CALL ERRSYS ('WIND       in TRCOEF < 0', 1)
C     Calculate wind at watersurface from wind at 10m (m/s)
            FWIND = C21 * WIND * SQRT( C22 + C23 * WIND)
            IF ( FWIND .LT. CRIT3  ) THEN
               FWIN2 = C28 * FWIND**C30
            ELSE
               FWIN2 = C29 * FWIND
            ENDIF
         ENDIF
C
         IF ( TMPOPT ) THEN
            TEMP   = PMSA(IP8 )
C--calculation of bulk densitys of water and air:
            RHOG = C11 / (1. + C12 * TEMP)
            RHOL = C13 - C14 * TEMP
C--calculation of viscosities of water and air:
            VG = (C15 + C16 * TEMP) * C17
            VL = C18
C --- Impact formulations (O'connor personal communication?)
            LDIF   = PMSA(IP5 )
            GDIF   = PMSA(IP6 )
      IF( GDIF  .LT. 1.E-30) CALL ERRSYS ('GAS-DIFF   in TRCOEF = 0', 1)
      IF( LDIF  .LT. 1.E-30) CALL ERRSYS ('WATER-DIFF in TRCOEF = 0', 1)
C     Calculate Schmidt numbers for water and gas
            SCG    = VG / ( RHOG * GDIF / 86400.)
            SCL    = VL / ( RHOL * LDIF / 86400.)
         ENDIF
         KG = ( C24 + C25 * FWIND / SCG**C26 )* 86400.
         KL = ( C27 + FWIN2 / SQRT(SCL) ) * 86400.
      ENDIF
C
C     Output
C
      PMSA(IP9 ) = KL
      PMSA(IP10) = KG
C
      ENDIF
      ENDIF
C
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
c
 9000 CONTINUE
c
      RETURN
C
      END
