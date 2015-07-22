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

      subroutine pripro ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Nett primary production and mortality DYNAMO algae

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
C     921210  Pascal Boderie  Create first version, based on T721.13
C                             created by Jos van Gils
C     921229  Pascal Boderie  Add third algae type, nutrient ratio's
C                             per species
C     .......
C     950922  Pascal Boderie  Toevoegen Respiration rate in uitvoer
C     980428  Jos van Gils    Toevoegen fPPxxx, fMrtxxx
C     980722  Jos van Gils    Toevoegen saliniteits-afhankelijke sterfte
C
C     990421  Jan van Beek    minimale algen hoeveelheid toegevoegd
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                   Unit
C ----    --- -  -    -------------------                            ---
C DL      R*4 1 I daylength for growth saturation green-algae          [
C EFF     R*4 1 L average light efficiency green-algae                 [
C FNUT    R*4 1 L nutrient limitation function green-algae             [
C PPMAX1  R*4 1 I pot. max. pr. prod. rc. green-algae (st.temp)      [1/
C PMSA    R*4 1 L Gross act. pr. prod. rc. green-algae               [1/
C TFUNG1  R*4 1 L temp. function for growth processes green            [

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local declaration
C
      REAL     ALGMIN
      INTEGER  NR_MES
      SAVE     NR_MES
      DATA     NR_MES / 0 /
C
      CALL GETMLU(ILUMON)
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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      ALG       = PMSA(IP1 )
      IF ( ALG .LT. 0.0 ) THEN
         IF ( NR_MES .LT. 25 ) THEN
            NR_MES = NR_MES + 1
            WRITE ( ILUMON , * ) 'WARNING :negative algae correction',
     +                           ' segment=',ISEG,' conc=',ALG
         ENDIF
         IF ( NR_MES .EQ. 25 ) THEN
            NR_MES = NR_MES + 1
            WRITE(ILUMON,*) ' 25 WARNINGS on negative algae'
            WRITE(ILUMON,*) ' Further messages on algae surpressed'
         ENDIF
         ALG = 0.0
      ENDIF
      DL        = PMSA(IP2 )
      FNUT      = PMSA(IP3 )
      EFF       = PMSA(IP4 )
      TFUNG     = PMSA(IP5 )
      TFUNM     = PMSA(IP6 )
      PPMAX     = PMSA(IP7 )
      MRESP     = PMSA(IP8 )
      GRESP     = PMSA(IP9 )
      MORT0     = PMSA(IP10)
      MORTS     = PMSA(IP11)
      SAL1      = PMSA(IP12)
      SAL2      = PMSA(IP13)
      SAL       = PMSA(IP14)
      ALGMIN    = PMSA(IP15)
      ACTMOR    = MORT0

C     Mortality coefficient depends on salinity
C     Value for low salinity is MORT0
C     Value for high salinity is MORTS
C     Linear transition from MORT0 to MORTS
C        between SAL1 and SAL2

      IF ( SAL1 .GT. 0.0 .AND. SAL2 .GT. SAL1 ) THEN
          IF ( SAL .LE. SAL1 ) THEN
              ACTMOR = MORT0
          ELSEIF ( SAL .GE. SAL2 ) THEN
              ACTMOR = MORTS
          ELSE
              ACTMOR = MORT0 + (SAL-SAL1)/(SAL2-SAL1)*(MORTS-MORT0)
          ENDIF
      ENDIF

C     Gross primary production
      PPROD =  DL * EFF  * FNUT  * TFUNG  * PPMAX

C     The respiration does not include excretion!!
C     The proces formulation used here does not release nutrients due
C     to respiration, but reduces the uptake of nutrients.
C     Respiration = maintainance part + growth part
      RESP    = MRESP  * TFUNM  + GRESP  * (PPROD - MRESP * TFUNM )

C     Nett primary production
      FL ( 1 + IFLUX ) = ( PPROD - RESP ) * ALG

C     Mortality, including processes as autolysis and zooplankton 'graas
      FL ( 2 + IFLUX ) = ACTMOR * TFUNM * MAX(ALG-ALGMIN,0.0)

      PMSA (IP16) = PPROD - RESP
      PMSA (IP17) = ACTMOR * TFUNM
      PMSA (IP18) = RESP
      PMSA (IP19) = (PPROD - RESP ) * ALG
      PMSA (IP20) = ACTMOR * TFUNM * MAX(ALG-ALGMIN,0.0)

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
      IP14  = IP14  + INCREM ( 14 )
      IP15  = IP15  + INCREM ( 15 )
      IP16  = IP16  + INCREM ( 16 )
      IP17  = IP17  + INCREM ( 17 )
      IP18  = IP18  + INCREM ( 18 )
      IP19  = IP19  + INCREM ( 19 )
      IP20  = IP20  + INCREM ( 20 )
c
 9000 CONTINUE
c
      RETURN
      END
