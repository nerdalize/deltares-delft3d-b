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

      subroutine sednut ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Sedimentation of nutrients in the organic carbon matrix (GEM)

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
C     921210  Pascal Boderie  Create first version, based on T890 SLIB
C     930210  Pascal Boderie  Version with adaptions for T692 (Delsta study)
C     951010  P. Boderie      Add calculation sedimentation velocity
C     970828  JvanBeek        Beveiliging ingebouwd tegen extreme c/n, c/p
C                              ratio's
C     980428  Jos van Gils    Invoer fSedDetC ipv dSedDetC, Depth extra
C     980605  Jos van Gils    Bereken ook fluxen in g/m2 (GEM)
C     980904  Jan van Beek    Bug fixed: never use PMSA as work space!!
C   20000419  Jan van Beek    Check on dummy exchanges (0->0)
C     121008  Johannes Smits  descriptions carbon nutrient ratios corrected
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C SFL     R*4 1 I  sedimention flux organic                      [gX/m2/d]
C CN      R*4 1 I  CN ratio substance                             [gC/gN]
C CP      R*4 1 I  CP ratio substance                             [gC/gP]
C CS      R*4 1 I  CS ratio substance                             [gC/gS]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

C     Segment pointers en incrementen
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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C
      SFL    = PMSA(IP1 )
      CN     = PMSA(IP2 )
      CP     = PMSA(IP3 )
      CS     = PMSA(IP4 )
      DEPTH  = PMSA(IP5 )

C*******************************************************************************
C**** Processes connected to the SEDIMENTATION of nutrients in C-Matrix
C***********************************************************************

C     INITIALISATIE
      FSEDDN = 0.0
      FSEDDP = 0.0
      FSEDDS = 0.0

C     SEDIMENTATION

      IF ( CN .GT. 0.1 ) FSEDDN = SFL / CN
      IF ( CP .GT. 0.1 ) FSEDDP = SFL / CP
      IF ( CS .GT. 0.1 ) FSEDDS = SFL / CS
C
      PMSA ( IP7 ) = FSEDDN
      PMSA ( IP8 ) = FSEDDP
      PMSA ( IP9 ) = FSEDDS
C
      IF ( DEPTH .GT. 0.0 ) THEN
          FL( 1 + IFLUX ) =  FSEDDN /DEPTH
          FL( 2 + IFLUX ) =  FSEDDP /DEPTH
          FL( 3 + IFLUX ) =  FSEDDS /DEPTH
      ELSE
          FL( 1 + IFLUX ) =  0.0
          FL( 2 + IFLUX ) =  0.0
          FL( 3 + IFLUX ) =  0.0
      ENDIF
C
      ENDIF
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + IN1
      IP2   = IP2   + IN2
      IP3   = IP3   + IN3
      IP4   = IP4   + IN4
      IP5   = IP5   + IN5
      IP7   = IP7   + IN7
      IP8   = IP8   + IN8
      IP9   = IP9   + IN9
C
 9000 CONTINUE

c.....Exchangeloop over de horizontale richting
      DO 8000 IQ=1,NOQ1+NOQ2

c........VxSedNut op nul (exchange uitvoer)
         PMSA(IP10) = 0.0
         PMSA(IP11) = 0.0
         PMSA(IP12) = 0.0

         IP10 = IP10 + IN10
         IP11 = IP11 + IN11
         IP12 = IP12 + IN12

 8000 CONTINUE
c.....initialisatie segment gerelateerde items
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
C
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)

c.....Startwaarden VxSedCMatrix
      IP6 = IP6 + ( NOQ1+NOQ2 ) * IN6

c.....Exchangeloop over de verticale richting
      DO 7000 IQ=NOQ1+NOQ2+1,NOQ1+NOQ2+NOQ3

         IVAN  = IEXPNT(1,IQ)

         IF ( IVAN .GT. 0 ) THEN
            VCMAT = PMSA( IP6 )
            CN    = PMSA( IP2 + (IVAN-1) * IN2 )
            CP    = PMSA( IP3 + (IVAN-1) * IN3 )
            CS    = PMSA( IP4 + (IVAN-1) * IN4 )

c...........Berekenen VxNut
            VN   = 0.0
            VP   = 0.0
            VS   = 0.0
            IF ( CN .GT. 0.0 ) VN   = VCMAT/CN
            IF ( CP .GT. 0.0 ) VP   = VCMAT/CP
            IF ( CS .GT. 0.0 ) VS   = VCMAT/CS

c...........VxNuts toekennen aan de PMSA
            PMSA(IP10) = VN
            PMSA(IP11) = VP
            PMSA(IP12) = VS
         ENDIF

c........Exchangepointers ophogen
         IP6 = IP6 + IN6
         IP10 = IP10 + IN10
         IP11 = IP11 + IN11
         IP12 = IP12 + IN12

 7000 CONTINUE
c
      RETURN
C
      END
