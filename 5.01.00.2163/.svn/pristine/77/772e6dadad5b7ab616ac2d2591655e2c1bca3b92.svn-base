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

      subroutine rfpart (pmsa  , fl    , ipoint, increm, noseg ,
     &                   noflux, iexpnt, iknmrk, noq1  , noq2  ,
     &                   noq3  , noq4  )
!>\file
!>       Reprofunctions for HM partition coefficients

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |                 MCM                    |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : Albufeira (M3564.06)
C     Author  : Johannes Smits
C     Date    : 040202             Version : 0.0
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     040202  Johannes Smits  first version
C
C***********************************************************************
C
C Name    T   L I/O   Description                                   Units
C ----    --- -  -    -------------------                            ----
C ALK     R*4 1 I     alkalinity                                 [mole/m3]
C CCL     R*4 1 I     chloride concentration                      [gCl/m3]
C CECIM1  R*4 1 I     cation exchange capacity of IM1            [eq/kgDW]
C CECIM2  R*4 1 I     cation exchange capacity of IM2            [eq/kgDW]
C CECIM3  R*4 1 I     cation exchange capacity of IM3            [eq/kgDW]
C DOC     R*4 1 I     dissolve organic carbon concentration        [gC/m3]
C KPIM1   R*4 1 O     partition coefficient for IM1              [m3/kgDW]
C KPIM2   R*4 1 O     partition coefficient for IM2              [m3/kgDW]
C KPIM3   R*4 1 O     partition coefficient for IM3              [m3/kgDW]
C KP0     R*4 1 -     reference partition coefficient            [m3/kgDW]
C PH      R*4 1 I     acidity                                          [-]
C AC      R*4 1 I     coefficient a for reprofunction            [various]
C BC      R*4 1 I     coefficient b for reprofunction            [various]
C CC      R*4 1 I     coefficient c for reprofunction            [various]
C DC      R*4 1 I     coefficient d for reprofunction            [various]
C GC      R*4 1 I     coefficient g for reprofunction            [various]
C LC      R*4 1 I     coefficient l for reprofunction            [various]
C MC      R*4 1 I     coefficient m for reprofunction            [various]
C NC      R*4 1 I     coefficient n for reprofunction            [various]
C OC      R*4 1 I     coefficient o for reprofunction            [various]
C IVERSN  I*4 1 -     option parameter for reprofunction               [-]
C                     (0=no repro, 1=Rine repro, 2=North Sea repro)
C
C     Logical Units : -
C
C     Modules called : -
C
C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT NONE
C
C     declaration of arguments
C
      INTEGER  NOSEG , NOFLUX, NOQ1  , NOQ2  , NOQ3  ,  NOQ4
      INTEGER  IPOINT(*)       , INCREM(*),
     +         IEXPNT(4,*)     , IKNMRK(*)
      REAL     PMSA(*)         , FL(*)
C
C     local declarations
C
      INTEGER  IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10,
     +         IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18 ,
     +         IP19, IP20
      INTEGER  IFLUX, ISEG, IKMRK1
C
      INTEGER  IVERSN
      INTEGER  LUNREP
C
      REAL     PH     , ALK    , CCL    , DOC   ,
     +         CECIM1 , CECIM2 , CECIM3 ,
     +         AC     , BC     , CC     , DC    , LC     , GC     ,
     +         MC     , NC     , OC     ,
     +         LOGALK , LOGCCL , LOGDOC , LOGKP0
C
      REAL     KPIM1  , KPIM2  , KPIM3  , KP0
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
C
      DO ISEG = 1 , NOSEG
C
!1       CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
C
!!       IF (IKMRK1.EQ.1.OR.IKMRK1.EQ.3) THEN
         IF (BTEST(IKNMRK(ISEG),0)) THEN
C
            PH      = PMSA(IP1 )
            ALK     = PMSA(IP2 )
            CCL     = PMSA(IP3 )
            DOC     = PMSA(IP4 )
            CECIM1  = PMSA(IP5 )
            CECIM2  = PMSA(IP6 )
            CECIM3  = PMSA(IP7 )
            AC      = PMSA(IP8 )
            BC      = PMSA(IP9 )
            CC      = PMSA(IP10)
            DC      = PMSA(IP11)
            GC      = PMSA(IP13)
            LC      = PMSA(IP12)
            MC      = PMSA(IP14)
            NC      = PMSA(IP15)
            OC      = PMSA(IP16)
            IVERSN  = NINT ( PMSA( IP17) )
C
            IF ( ALK .LT. 1.0) ALK = 1.0
            IF ( CCL .LT. 1.0) CCL = 1.0
            IF ( DOC .LT. 0.1) DOC = 0.1
C
C           Calculation of partition coefficients depending on switch
C
            IF ( IVERSN .EQ. 1 ) THEN

C              "Rhine" function

               LOGALK = LOG10(ALK)
               LOGCCL = LOG10(CCL)
               LOGDOC = LOG10(DOC)
               LOGKP0 = AC + BC*PH + CC*(PH**2) + DC*LOGALK + GC*LOGCCL +
     +                  LC*LOGDOC + MC*PH*LOGALK + NC*PH*LOGALK*LOGALK +
     +                  OC*PH*PH*LOGALK
               KP0    = 10.0**LOGKP0

               KPIM1 = KP0 * (CECIM1 / 0.0002)
               KPIM2 = KP0 * (CECIM2 / 0.0002)
               KPIM3 = KP0 * (CECIM3 / 0.0002)
C
            ELSEIF ( IVERSN .EQ. 2 ) THEN

C              "North Sea" function

               KP0   = (10.0**AC) * (10.0**(BC*PH)) *
     +                 ((1800.*CCL+CC)**DC)
               KPIM1 = KP0 * CECIM1 * 1000.
               KPIM2 = KP0 * CECIM2 * 1000.
               KPIM3 = KP0 * CECIM3 * 1000.
C
            ELSE

C              switch for function out of range

               CALL GETMLU(LUNREP)
               WRITE(LUNREP,*) 'ERROR in RFPART'
               WRITE(LUNREP,*) 'Illegal option for repro function partition coefficient'
               WRITE(LUNREP,*) 'Option in input:',IVERSN
               WRITE(*,*) ' ERROR in RFPART'
               WRITE(*,*) ' Illegal option for repro function partition coefficient'
               WRITE(*,*) ' Option in input:',IVERSN
               CALL SRSTOP(1)

            ENDIF
C
C           Output of module
C
            PMSA(IP18) = KPIM1
            PMSA(IP19) = KPIM2
            PMSA(IP20) = KPIM3
C
C        End active cells block
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
         IP13  = IP13  + INCREM ( 13 )
         IP14  = IP14  + INCREM ( 14 )
         IP15  = IP15  + INCREM ( 15 )
         IP16  = IP16  + INCREM ( 16 )
         IP17  = IP17  + INCREM ( 17 )
         IP18  = IP18  + INCREM ( 18 )
         IP19  = IP19  + INCREM ( 19 )
         IP20  = IP20  + INCREM ( 20 )
C
      ENDDO
C
      RETURN
C
      END
