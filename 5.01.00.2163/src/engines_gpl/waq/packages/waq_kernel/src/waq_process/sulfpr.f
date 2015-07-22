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

      subroutine sulfpr ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Precipitation and dissolution of sulphide as first order process

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : SLIK, ONTW. BODEM-WATER UITWISSELINGSMODULES, Q2935.30
C     Author  : Johannes Smits
C     Date    : 021128             Version : 0.02
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     020524  Johannes Smits  New generic process for sulphide
C                             precipitation and dissolution
C     021210  Johannes Smits  Correction to nullify either
C                             precipitation or dissolution
C                             and to prevent negative concentrations
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        Sulphide precipitation and dissolution kinetics as first order
C        process with respect to the difference of actual and
C        equilibrium free dissolved sulphide concentrations.
C        Process is valid for overlying water as well as sediment.
C
C        ----- description of parameters -----
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C CSP     R*4 1 I concentration of precipitated sulphide            [g/m3]
C CSD     R*4 1 I actual concentration of free dissolved sulphide [mole/l]
C CSDE    R*4 1 I equilibrium conc. of free dissolved sulphide    [mole/l]
C DELT    R*4 1 I timestep                                             [d]
C FL (1)  R*4 1 O sulphide precipitation flux                    [gS/m3/d]
C FL (2)  R*4 1 O sulphide dissolution flux                      [gS/m3/d]
C FLUXPR  R*4 1 - sulphide precipitation flux                    [gS/m3/d]
C FLUXDS  R*4 1 - sulphide dissolution flux                      [gS/m3/d]
C KDIS    R*4 1 I first order sulphide dissolution rate              [1/d]
C KPRC    R*4 1 I first order sulphide precipitation rate            [1/d]
C KTDIS   R*4 1 I temperature coefficient for dissolution              [-]
C KTPRC   R*4 1 I temperature coefficient for precipitation            [-]
C POROS   R*4 1 I porosity                                             [-]
C TEMP    R*4 1 I ambient temperature                                 [oC]
C TEMPCD  R*4 1 L ambient temp. correction function for dissolution    [-]
C TEMPCP  R*4 1 L ambient temp. correction function for precipitation  [-]
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
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10
      INTEGER  IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10
      INTEGER  IFLUX  , ISEG   , IKMRK1
C
      REAL     CSP    , CSD
      REAL     KDIS   , KPRC   , CSDE   , KTDIS  , KTPRC
      REAL     POROS  , TEMP   , TEMPCD , TEMPCP , TEMP20
      REAL     DELT   , FLUXPR , FLUXDS
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
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF ( IKMRK1 .GT. 0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
            CSP    = MAX (PMSA(IP1), 0.0)
            CSD    = MAX (PMSA(IP2), 0.0)
            CSDE   = MAX (PMSA(IP3), 0.0)
            KDIS   = PMSA(IP4 )
            KTDIS  = PMSA(IP5 )
            KPRC   = PMSA(IP6 )
            KTPRC  = PMSA(IP7 )
            TEMP   = PMSA(IP8 )
            POROS  = PMSA(IP9 )
            DELT   = PMSA(IP10)
C
C           Calculate the precipitation and dissolution fluxes
C           The constant 32000 concerns conversion mole/l to gS/m3
C
            TEMP20 = TEMP - 20.0
            TEMPCP = KTPRC ** TEMP20
            TEMPCD = KTDIS ** TEMP20
C
            FLUXPR = 32000.0 * KPRC * TEMPCP * (CSD - CSDE) * POROS
            FLUXDS = 32000.0 * KDIS * TEMPCD * (CSDE - CSD) * POROS
C
C           Correct fluxes depending on under- or supersaturation
C
            IF ( FLUXPR .LT. 0.0) FLUXPR = 0.0
            IF ( FLUXDS .LT. 0.0) FLUXDS = 0.0
            IF ( FLUXDS*DELT .GE. CSP) FLUXDS = 0.5 * CSP / DELT
C
            FL( 1+IFLUX ) = FLUXPR
            FL( 2+IFLUX ) = FLUXDS
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
C
 9000 CONTINUE
C
      RETURN
C
      END
