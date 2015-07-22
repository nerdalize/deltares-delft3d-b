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

      subroutine nralgs ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Nutrient release of algae in S1 and S2

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
C     ......  ..............  ..............................
C     930730  Pascal Boderie  Mortality of algae in sediment
C     073107  Jos van Gils    Expanded with optional autolysis to Switch
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                   Unit
C ----    --- -  -    -------------------                            ---
C FALGx   R*4 1 I mortality flux of algea-type x in sediment  [gC/m3/d]
C FL(1)   R*4 1 O autolysis of NN4                            [gN/m3/d]
C FL(2)   R*4 1 O production of N-det                         [gN/m3/d]
C FL(3)   R*4 1 O autolysis of P                              [gP/m3/d]
C FL(4)   R*4 1 O production of P-det                         [gP/m3/d]
C FL(5)   R*4 1 O autolysis of Si                            [gSi/m3/d]
C FL(6)   R*4 1 O production of Si-det                      [gSiC/m3/d]
C FRMRT1  R*4 1 I fraction of mortality dissolved as nutrients      [-]
C FRMRT2  R*4 1 I fraction of mortality dissolved as nutrients      [-]
C NCRAT1  R*4 1 I Nitrogen-Carbon ratio in green-algea          [gN/gC]
C NCRAT2  R*4 1 I Nitrogen-Carbon ratio in diatoms              [gN/gC]
C PCRAT1  R*4 1 I Phosphorus-Carbon ratio in green-algea        [gP/gC]
C PCRAT2  R*4 1 I Phosphorus-Carbon ratio in diatoms            [gP/gC]
C SICRAT  R*4 1 I Silicate-Carbon ratio in diatoms             [gSi/gC]

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
      integer  ip1 , ip2 , ip3 , ip4 , ip5 , ip6 , ip7 , ip8 ,
     j         ip9 , ip10, ip11, ip12, ip13,
     j         iflux, iseg, ikmrk1, ikmrk2
      real     FALG1,NCRAT1,PCRAT1,AUT1,DET1,FALG2,NCRAT2,PCRAT2,
     j         SICRAT,AUT2,DET2,DEPTH,SWITCH,AA,DC1,DC2

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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C
      FALG1     = PMSA(IP1)
      NCRAT1    = PMSA(IP2)
      PCRAT1    = PMSA(IP3)
      AUT1      = PMSA(IP4)
      DET1      = PMSA(IP5)
      FALG2     = PMSA(IP6)
      NCRAT2    = PMSA(IP7)
      PCRAT2    = PMSA(IP8)
      SICRAT    = PMSA(IP9)
      AUT2      = PMSA(IP10)
      DET2      = PMSA(IP11)
      DEPTH     = PMSA(IP12)
      switch    = PMSA(IP13)

C***********************************************************************
C**** Processes connected to the ALGEA model
C***********************************************************************

C     Calculate fractions for carbon (different from nutrient fractions)
C     no part of carbon to autolyse!
       DC1 = 0.0
       DC2 = 0.0
       IF (AUT1 .LT. 1.0) DC1 = DET1 / (1-AUT1)
       IF (AUT2 .LT. 1.0) DC2 = DET2 / (1-AUT2)

C@    Production of DETC
      FL ( 1 + IFLUX ) = ( FALG1 * DC1+
     &            FALG2 * DC2 ) / DEPTH

C@    Production of OOC
      FL ( 2 + IFLUX ) = ( FALG1 * ( 1.0 - DC1 ) +
     &            FALG2 * ( 1.0 - DC2 ) ) / DEPTH

C@    Autolysis of NN4
      AA = ( FALG1 * NCRAT1 * AUT1 +
     &            FALG2 * NCRAT2 * AUT2 ) / DEPTH
      if ( abs(switch) .lt. 0.5 ) then
          FL ( 3 + IFLUX ) = AA
          FL (12 + IFLUX ) = 0.0
      else
          FL (12 + IFLUX ) = AA
          FL ( 3 + IFLUX ) = 0.0
      endif

C@    Production of N-det
      FL ( 4 + IFLUX ) = ( FALG1 * NCRAT1 * DET1 +
     &            FALG2 * NCRAT2 * DET2 ) / DEPTH

C@    Production of OON
      FL ( 5 + IFLUX ) = ( FALG1 * NCRAT1 * ( 1.0 - AUT1 - DET1 ) +
     &            FALG2 * NCRAT2 * ( 1.0 - AUT2 - DET2 ) ) / DEPTH

C@    Autolysis of P
      AA = ( FALG1 * PCRAT1 * AUT1 +
     &            FALG2 * PCRAT2 * AUT2 ) / DEPTH
      if ( abs(switch) .lt. 0.5 ) then
          FL ( 6 + IFLUX ) = AA
          FL (13 + IFLUX ) = 0.0
      else
          FL (13 + IFLUX ) = AA
          FL ( 6 + IFLUX ) = 0.0
      endif

C@    Production of P-det
      FL ( 7 + IFLUX ) = ( FALG1 * PCRAT1 * DET1 +
     &            FALG2 * PCRAT2 * DET2 ) / DEPTH

C@    Production of OOP
      FL ( 8 + IFLUX ) = ( FALG1 * PCRAT1 * ( 1.0 - AUT1 - DET1 ) +
     &            FALG2 * PCRAT2 * ( 1.0 - AUT2 - DET2 ) ) / DEPTH

C@    Autolysis of Si
      FL ( 9 + IFLUX ) = ( FALG2 * SICRAT  * AUT2 ) / DEPTH

C@    Production of Si-det
      FL (10 + IFLUX ) = ( FALG2 * SICRAT *  DET2 ) / DEPTH

C@    Production of OOSi
      FL (11 + IFLUX ) = ( FALG2 * SICRAT * (1.0 - AUT2 -  DET2) ) /
     &                   DEPTH
C
      ENDIF
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
C
 9000 CONTINUE
C
      RETURN
      END
