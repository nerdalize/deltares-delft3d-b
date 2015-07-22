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

      subroutine sdppro ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Traditional algal growth module (DYNAMO)

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
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                   Unit
C ----    --- -  -    -------------------                            ---
C DL      R*4 1 I daylength for growth saturation green-algea          [
C EFF     R*4 1 L average light efficiency green-algea                 [
C FNUT    R*4 1 L nutrient limitation function green-algea             [
C PPMAX1  R*4 1 I pot. max. pr. prod. rc. green-algea (st.temp)      [1/
C PMSA    R*4 1 L Gross act. pr. prod. rc. green-algea               [1/
C TFUNG1  R*4 1 L temp. function for growth processes green            [

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      PARAMETER (DINI=0.01)

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
      IP29 = IPOINT(29)
      IP30 = IPOINT(30)
      IP31 = IPOINT(31)
      IP32 = IPOINT(32)
      IP33 = IPOINT(33)
      IP34 = IPOINT(34)
      IP35 = IPOINT(35)
      IP36 = IPOINT(36)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C

      SURF      = PMSA(IP26)
      DEPTH     = PMSA(IP27)
      DIAT      = PMSA(IP1 )
      DL        = PMSA(IP2 )
      EFF       = PMSA(IP3 )
      TFUNG     = PMSA(IP4 )
      TFUNM     = PMSA(IP5 )
      PPMAX     = PMSA(IP6 )
      MRESP     = PMSA(IP7 )
      GRESP     = PMSA(IP8 )
      MORT0     = PMSA(IP9 )
      CNH4      = PMSA(IP10)
      CNO3      = PMSA(IP11)
      CPO4      = PMSA(IP12)
      CSI       = PMSA(IP13)
      SWMINN    = PMSA(IP14)*DEPTH
      MINN      = PMSA(IP15)*DEPTH
      SWMINP    = PMSA(IP16)*DEPTH
      MINP      = PMSA(IP17)*DEPTH
      SWMINS    = PMSA(IP18)*DEPTH
      MINS      = PMSA(IP19)*DEPTH
      KMDIN     = PMSA(IP20)
      KMPO4     = PMSA(IP21)
      KMSIL     = PMSA(IP22)
      NCRAT     = PMSA(IP23)
      PCRAT     = PMSA(IP24)
      SICRAT    = PMSA(IP25)
      FRNBAC    = PMSA(IP28)
      DELTAT    = PMSA(IP29)
      NH4KR     = PMSA(IP30)

C     DIATOMS IN SEDIMENT - IF PPMAX -1 ONLY MORTALITY
      IF (PPMAX.LT.0.0) THEN
        PRODD = 0.0
        RESP = 0.0
        FL ( 1 + IFLUX ) =  0.0
        FL ( 3 + IFLUX ) =  0.0
        FL ( 4 + IFLUX ) =  0.0
        FL ( 5 + IFLUX ) =  0.0
        FL ( 6 + IFLUX ) =  0.0
        FL ( 7 + IFLUX ) =  0.0
        FL ( 8 + IFLUX ) =  0.0
        FL ( 9 + IFLUX ) =  0.0
        FL (10 + IFLUX ) =  0.0
        FL (11 + IFLUX ) =  0.0
        FL (12 + IFLUX ) =  0.0
        EFF = 0.0
        DL = 0.0
        EFNMFB = 0.0
      ELSE

C       Gross primary production (no nutrient limitation yet, 1/day)
        PPRODD =  DL * EFF  * TFUNG  * PPMAX

C       Respiration = maintainance part + growth part (1/day)
        RESP    = MRESP  * TFUNM  + GRESP  * (PPRODD - MRESP * TFUNM )

C       Nett production (g/m2/day)
C       Minimale biomassa bij initialisatie
        DIAT2 = MAX(DIAT, DINI)
        PRODD   = MAX ( (PPRODD - RESP) * DIAT2, 0.0 )

C       Requested uptake of nutrients (g/m2/day)

        UPTAKC = PRODD
        UPTAKN = PRODD*NCRAT
        UPTAKP = PRODD*PCRAT
        UPTAKS = PRODD*SICRAT

C       Compute requested uptake of nutrients from the water column
C       Note: N is consumed by bacteria with a fraction FRNBAC

        UPTA2N = UPTAKN - (MINN+SWMINN)*(1.-FRNBAC)
        UPTA2P = UPTAKP - (MINP+SWMINP)
        UPTA2S = UPTAKS - (MINS+SWMINS)

C       UPTA2x is filled by CHKLIM with maximum available uptake
C       of nutrients from the water column (considering Monod limitation)

        DIN = CNH4 + CNO3
        CALL CHKLIM (UPTA2N, DIN   , DEPTH , DELTAT, KMDIN )
        CALL CHKLIM (UPTA2P, CPO4  , DEPTH , DELTAT, KMPO4 )
        CALL CHKLIM (UPTA2S, CSI   , DEPTH , DELTAT, KMSIL )

C       Find total available uptake of nutrients from mineralization flux
C       and from the water column

        UPTAKN = UPTA2N + (MINN+SWMINN)*(1.-FRNBAC)
        UPTAKP = UPTA2P + (MINP+SWMINP)
        UPTAKS = UPTA2S + (MINS+SWMINS)

C       Find actual production and compute 'nutrient efficiency'

        IF (PRODD.GT.1E-30) THEN
          EFNMFB = AMIN1 (UPTAKC, UPTAKN/NCRAT, UPTAKP/PCRAT,
     J                    UPTAKS/SICRAT) / PRODD
          EFNMFB = AMAX1(EFNMFB,0.0)
        ELSE
          EFNMFB = 1.0
        ENDIF
        PRODD = AMIN1(UPTAKC, UPTAKN/NCRAT, UPTAKP/PCRAT, UPTAKS/SICRAT)
        PRODD = AMAX1 ( PRODD , 0.0 )

C16/2   PMSA(IP11) = PRODD

C       Nett primary production and uptake of nutrients

        FL ( 1 + IFLUX ) =  PRODD / DEPTH

C       Division of nitrogen uptake over NH4 and NO3 and mineralization
        IF ((PRODD*NCRAT).LE.(MINN+SWMINN)) THEN
          IF (SWMINN.GT.0.0) THEN
            FL (10 + IFLUX ) =  PRODD * NCRAT/ DEPTH
            FL ( 7 + IFLUX ) =  0.0
          ELSE
            FL ( 7 + IFLUX ) =  PRODD * NCRAT/ DEPTH
            FL (10 + IFLUX ) =  0.0
          ENDIF
          FL ( 3 + IFLUX ) =  0.0
          FL ( 4 + IFLUX ) =  0.0
        ELSE
          XNTOT = (PRODD * NCRAT - (MINN+SWMINN)*FRNBAC) * DELTAT/DEPTH
          IF (CNH4.GT.NH4KR) THEN
             IF (XNTOT.LE.(CNH4 - NH4KR)) THEN
                 NH4D = 1.
                 NO3D = 0.
             ELSE
                 XNREST = XNTOT - CNH4 + NH4KR
                 FNH4   = NH4KR / (CNO3 + NH4KR)
                 NH4D = ((CNH4 - NH4KR) + FNH4 * XNREST) / XNTOT
                 NO3D = 1. - NH4D
             ENDIF
          ELSE
              NH4D = CNH4 / (CNO3 + CNH4)
              NO3D = 1. - NH4D
          ENDIF
          IF (SWMINN.GT.0.0) THEN
            FL (10 + IFLUX ) =  SWMINN*FRNBAC / DEPTH
            FL ( 7 + IFLUX ) =  0.0
          ELSE
            FL ( 7 + IFLUX ) =  MINN*FRNBAC / DEPTH
            FL (10 + IFLUX ) =  0.0
          ENDIF
          FL ( 3 + IFLUX ) =  XNTOT / DELTAT * NH4D
          FL ( 4 + IFLUX ) =  XNTOT / DELTAT * NO3D
        ENDIF

C       Division of phosphorus dissolved and from mineralization
        IF ((PRODD*PCRAT).LE.(MINP+SWMINP)) THEN
          FL ( 5 + IFLUX ) =  0.0
          IF (SWMINP.GT.0.0) THEN
            FL ( 8 + IFLUX ) =  0.0
            FL ( 11+ IFLUX ) =  PRODD * PCRAT/ DEPTH
          ELSE
            FL ( 8 + IFLUX ) =  PRODD * PCRAT/ DEPTH
            FL ( 11+ IFLUX ) =  0.0
          ENDIF
        ELSE
          IF (SWMINP.GT.0.0) THEN
            FL ( 8 + IFLUX ) =  0.0
            FL ( 11+ IFLUX ) =  SWMINP/ DEPTH
          ELSE
            FL ( 8 + IFLUX ) =  MINP/ DEPTH
            FL ( 11+ IFLUX ) =  0.0
          ENDIF
          FL ( 5 + IFLUX ) =  (PRODD* PCRAT-MINP-SWMINP)/DEPTH
        ENDIF

C       Division of silicium dissolved and from mineralization
        IF ((PRODD*SICRAT).LE.(MINS+SWMINS)) THEN
          FL ( 6 + IFLUX ) =  0.0
          IF (SWMINS.GT.0.0) THEN
            FL ( 9 + IFLUX ) =  0.0
            FL ( 12+ IFLUX ) =  PRODD * SICRAT/ DEPTH
          ELSE
            FL ( 9 + IFLUX ) =  PRODD * SICRAT/ DEPTH
            FL ( 12+ IFLUX ) =  0.0
          ENDIF
        ELSE
          IF (SWMINS.GT.0.0) THEN
            FL ( 9 + IFLUX ) =  0.0
            FL ( 12+ IFLUX ) =  SWMINS/ DEPTH
          ELSE
            FL ( 9 + IFLUX ) =  MINS/ DEPTH
            FL ( 12+ IFLUX ) =  0.0
          ENDIF
          FL ( 6 + IFLUX ) =  (PRODD* SICRAT-MINS-SWMINS)/DEPTH
        ENDIF

      ENDIF

C16/2   PMSA(IP12) = RESP


C     Mortality, including processes as autolysis and zooplankton 'graas
      FL ( 2 + IFLUX ) = MORT0  *  TFUNM * DIAT / DEPTH

      PMSA (IP31) = PRODD
      PMSA (IP32) = MORT0 * TFUNM
      PMSA (IP33) = RESP
      PMSA (IP34) = MORT0  *  TFUNM * DIAT
      PMSA (IP35) = EFNMFB
      PMSA (IP36) = EFF*DL

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
      IP14  = IP14  + INCREM ( 14 )
      IP15  = IP15  + INCREM ( 15 )
      IP16  = IP16  + INCREM ( 16 )
      IP17  = IP17  + INCREM ( 17 )
      IP18  = IP18  + INCREM ( 18 )
      IP19  = IP19  + INCREM ( 19 )
      IP20  = IP20  + INCREM ( 20 )
      IP21  = IP21  + INCREM ( 21 )
      IP22  = IP22  + INCREM ( 22 )
      IP23  = IP23  + INCREM ( 23 )
      IP24  = IP24  + INCREM ( 24 )
      IP25  = IP25  + INCREM ( 25 )
      IP26  = IP26  + INCREM ( 26 )
      IP27  = IP27  + INCREM ( 27 )
      IP28  = IP28  + INCREM ( 28 )
      IP29  = IP29  + INCREM ( 29 )
      IP30  = IP30  + INCREM ( 30 )
      IP31  = IP31  + INCREM ( 31 )
      IP32  = IP32  + INCREM ( 32 )
      IP33  = IP33  + INCREM ( 33 )
      IP34  = IP34  + INCREM ( 34 )
      IP35  = IP35  + INCREM ( 35 )
      IP36  = IP36  + INCREM ( 36 )
c
 9000 CONTINUE
c
      RETURN
      END

C
C***********************************************************************
C
      SUBROUTINE CHKLIM ( REQFLX, CONC  , DEPTH , DELTAT, KM    )
      IMPLICIT REAL (A-Z)
      REAL    AVAFLX, CONC  , DEPTH , DELTAT, KM    ,
     1        REQFLX
C
C     Nutrient limited flux (g/m2/day)
C
      AVAFLX = (CONC/(KM+CONC)) * CONC / DELTAT * DEPTH
      AVAFLX = AMAX1 ( AVAFLX, 0.0 )
      IF (AVAFLX .LT. REQFLX) REQFLX = AVAFLX
      RETURN
      END
