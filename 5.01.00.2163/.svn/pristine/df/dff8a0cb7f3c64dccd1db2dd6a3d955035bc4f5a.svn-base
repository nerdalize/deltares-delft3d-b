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

      subroutine vivian ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Dissolution/precipitation of P in vivianite

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |                 MCM                    |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : GEM  (T2087)
C     Author  : Rik Sonneveldt
C     Date    : 970512             Version : 0.0
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     970512  Rik SOnneveldt  first version, based on ADSPO4.FOR
C     970929  Rik SOnneveldt  IKMRK1 LOOP AAngepast
C     010711  Johannes Smits  corrected for porosity, change of names
C     021218  Johannes Smits  corrected to prevent negative fluxes
C                             and negative concentrations
C
C***********************************************************************
C
C     Description of the module :
C     precipitation and dissolution of vivianite
C
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----

C CPHD    R*4 1 I     concentration dissolved phosphate            [gP/m3]
C CPHDE   R*4 1 I     saturation concentration dissolved phosphate [gP/m3]
C CPHPR   R*4 1 I     concentration vivianite                      [gP/m3]
C CROXY   R*4 1 I     critical concentration dissolved oxygen     [gO2/m3]
C DELT    R*4 1 I     timestep                                         [d]
C FPRC    R*4 1 O     precipitation flux                         [gP/m3/d]
C FRP     R*4 1 -     switch concerning redox pot. for precipitation   [-]
C FRS     R*4 1 -     switch concerning redox pot. for dissolution     [-]
C FSOL    R*4 1 O     dissolution flux                           [gP/m3/d]
C KPRC    R*4 1 I     precipitation rate                             [1/d]
C KSOL    R*4 1 I     dissolution rate                          [m3/gO2/d]
C OXY     R*4 1 I     concentration dissolved oxygen              [gO2/m3]
C POROS   R*4 1 I     porosity                                         [-]
C TCPRC   R*4 1 I     temperature coefficient of precipitation         [-]
C TCSOL   R*4 1 I     temperature coefficient of dissolution           [-]
C TEMP    R*4 1 I     temperature                                     [oC]
C TMPPRC  R*4 1 -     temperature function for precipitation           [-]
C TMPSOL  R*4 1 -     temperature function for dissolution             [-]

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT REAL (A-H,J-Z)

      INTEGER  NOSEG , NOFLUX, NOQ1  , NOQ2  , NOQ3  , NOQ4
      INTEGER  IPOINT(*)       , INCREM(*),
     +         IEXPNT(4,*)     , IKNMRK(*)
      REAL     PMSA(*)         , FL(*)

      REAL     KSOL , KPRC   , FSOL   , FPRC  , FRP   , FRS   ,
     +         TEMP , TMPSOL , TMPPRC , TCSOL , TCPRC ,
     +         CPHD , CPHPR  , CPHDE  , POROS , OXY   , CROXY ,
     +         DELT

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
C
      IFLUX = 0

      DO 9000 ISEG = 1 , NOSEG

!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)

!!    IF (IKMRK1.EQ.1.OR.IKMRK1.EQ.3) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
         CPHD    = PMSA(IP1)
         CPHPR   = PMSA(IP2)
         CPHDE   = PMSA(IP3)
         KPRC    = PMSA(IP4)
         TCPRC   = PMSA(IP5)
         KSOL    = PMSA(IP6)
         TCSOL   = PMSA(IP7)
         OXY     = PMSA(IP8)
         CROXY   = PMSA(IP9)
         TEMP    = PMSA(IP10)
         POROS   = PMSA(IP11)
         DELT    = PMSA(IP12)
C
C     Calculation of the precipitation or dissolution flux
C     dependent on dissolved oxygen
C
         IF ( OXY .GE. CROXY ) THEN
            FRP = 0.0
            FRS = 1.0
         ELSE
            FRP = 1.0
            FRS = 0.0
         ENDIF

         FPRC = 0.0
         FSOL = 0.0
         TMPPRC = TCPRC**(TEMP - 20.0)
         TMPSOL = TCSOL**(TEMP - 20.0)
C
         FPRC = FRP * KPRC * TMPPRC * ( CPHD / POROS - CPHDE ) * POROS
         FSOL = FRS * KSOL * TMPSOL * CPHPR * OXY / POROS
C
         IF ( FPRC .LT. 0.0) FPRC = 0.0
         IF ( FSOL .LT. 0.0) FSOL = 0.0
         IF ( FSOL*DELT .GE. CPHPR) FSOL = 0.5 * CPHPR / DELT
C
C     Output of module
C
         FL(1+IFLUX) = FPRC
         FL(2+IFLUX) = FSOL
C
C     End active cells block
C
      ENDIF

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
C
 9000 CONTINUE
C
      RETURN
C
      END
