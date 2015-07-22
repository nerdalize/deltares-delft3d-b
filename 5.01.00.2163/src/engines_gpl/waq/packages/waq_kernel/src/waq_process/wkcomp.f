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

      subroutine wkcomp ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Computes sum parameters from fractions (GEM)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : GEM T2087
C     Author  : Jos van Gils
C
C     Date    : 05jun98            Version : 0.00
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     980605  Jos van Gils    Created for integration GEM
C     121008  Johannes Smits  Modified for sulphur, PAP removed
C
C***********************************************************************
C
C     Description of the module :
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library

C     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( 80 ) , INCREM(80) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      REAL     NO3,NH4,PO4,Si,IM1,IM2,IM3,Phyt,AlgN,AlgP,AlgSi,AlgDM,
     J         POCnoa,POMnoa,PONnoa,POPnoa, POSnoa, DOC, DON, DOP,
     J         DOS, AAP, VIVP, APATP, DmIM1, DmIM2, DmIM3
      REAL     TIM,POC,TOC,PON,TON,DIN,TotN,Kjel,POP,TOP,PIP,TotP,
     J         FrAAP, FrVAP, TotSi, TOSnoa
      REAL     POC1, PON1, POP1, POS1, DmPOC1, CN1, CP1, CS1,
     J         POC2, PON2, POP2, POS2, DmPOC2, CN2, CP2, CS2,
     J         POC3, PON3, POP3, POS3, DmPOC3, CN3, CP3, CS3,
     J         POC4, PON4, POP4, POS4, DmPOC4, CN4, CP4, CS4
      INTEGER  IFLUX, IKMRK1, ISEG
      INTEGER  IP(80)
C
      IP = IPOINT
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
C        CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
C        IF (IKMRK1.EQ.1.OR.IKMRK1.EQ.2) THEN
C
            NO3     = PMSA(IP(1 ))
            NH4     = PMSA(IP(2 ))
            PO4     = PMSA(IP(3 ))
            Si      = PMSA(IP(4 ))
            IM1     = PMSA(IP(5 ))
            IM2     = PMSA(IP(6 ))
            IM3     = PMSA(IP(7 ))
            Phyt    = PMSA(IP(8 ))
            AlgN    = PMSA(IP(9 ))
            AlgP    = PMSA(IP(10))
            AlgSi   = PMSA(IP(11))
            AlgDM   = PMSA(IP(12))
            POC1    = PMSA(IP(13))
            POC2    = PMSA(IP(14))
            POC3    = PMSA(IP(15))
            POC4    = PMSA(IP(16))
            PON1    = PMSA(IP(17))
            DOC     = PMSA(IP(18))
            DON     = PMSA(IP(19))
            DOP     = PMSA(IP(20))
            DOS     = PMSA(IP(21))
            AAP     = PMSA(IP(22))
            VIVP    = PMSA(IP(23))
            APATP   = PMSA(IP(24))
C
            DmIM1   = PMSA(IP(25))
            DmIM2   = PMSA(IP(26))
            DmIM3   = PMSA(IP(27))
C
            PON2    = PMSA(IP(28))
            PON3    = PMSA(IP(29))
            PON4    = PMSA(IP(30))
            POP1    = PMSA(IP(31))
            POP2    = PMSA(IP(32))
            POP3    = PMSA(IP(33))
            POP4    = PMSA(IP(34))
            POS1    = PMSA(IP(35))
            POS2    = PMSA(IP(36))
            POS3    = PMSA(IP(37))
            POS4    = PMSA(IP(38))
C
            POSnoa  = PMSA(IP(39))
C
            DmPOC1  = PMSA(IP(40))
            DmPOC2  = PMSA(IP(41))
            DmPOC3  = PMSA(IP(42))
            DmPOC4  = PMSA(IP(43))
C
            TIM = DmIM1*IM1 + DmIM2*IM2 + DmIM3*IM3
C
            POCnoa = POC1 + POC2 + POC3 + POC4
            POMnoa = POC1*DmPOC1 + POC2*DmPOC2
     J             + POC3*DmPOC3 + POC4*DmPOC4
            PONnoa = PON1 + PON2 + PON3 + PON4
            POPnoa = POP1 + POP2 + POP3 + POP4
            TOSnoa = POS1 + POS2 + POS3 + POS4 + DOS
C
            POC   = Phyt + POCnoa
            TOC   = POC + DOC
C
            PON   = AlgN + PONnoa
            TON   = PON + DON
            DIN   = NH4 + NO3
            TotN  = TON + DIN
            Kjel  = TON + NH4
C
            POP   = AlgP + POPnoa
            TOP   = POP + DOP
            PIP   = AAP + VIVP + APATP
            TotP  = TOP + PO4 + PIP
C
            IF ( TIM .GT. 0.0 ) THEN
            FrAAP = AAP/TIM
            FrVAP = (VIVP+APATP)/TIM
            ELSE
            FrAAP = 0.0
            FrVAP = 0.0
            ENDIF
C
            TotSi = AlgSi + POSnoa + Si
C
            CN1 = 0.0
            CN2 = 0.0
            CN3 = 0.0
            CN4 = 0.0
            CP1 = 0.0
            CP2 = 0.0
            CP3 = 0.0
            CP4 = 0.0
            CS1 = 0.0
            CS2 = 0.0
            CS3 = 0.0
            CS4 = 0.0
C
            IF (PON1.GT.0.0) CN1 = POC1/PON1
            IF (PON2.GT.0.0) CN2 = POC2/PON2
            IF (PON3.GT.0.0) CN3 = POC3/PON3
            IF (PON4.GT.0.0) CN4 = POC4/PON4
            IF (POP1.GT.0.0) CP1 = POC1/POP1
            IF (POP2.GT.0.0) CP2 = POC2/POP2
            IF (POP3.GT.0.0) CP3 = POC3/POP3
            IF (POP4.GT.0.0) CP4 = POC4/POP4
            IF (POS1.GT.0.0) CS1 = POC1/POS1
            IF (POS2.GT.0.0) CS2 = POC2/POS2
            IF (POS3.GT.0.0) CS3 = POC3/POS3
            IF (POS4.GT.0.0) CS4 = POC4/POS4
C
            PMSA (IP(44)) = TIM + AlgDM + POMnoa
            PMSA (IP(45)) = TIM + POMnoa
            PMSA (IP(46)) = TIM + AlgDM + POMnoa
            PMSA (IP(47)) = TIM
C
            PMSA (IP(48)) = AlgDM + POMnoa
            PMSA (IP(49)) = TOC
            PMSA (IP(50)) = POC
C
            PMSA (IP(51)) = TotN
            PMSA (IP(52)) = Kjel
            PMSA (IP(53)) = DIN
            PMSA (IP(54)) = TON
            PMSA (IP(55)) = PON
C
            PMSA (IP(56)) = TotP
            PMSA (IP(57)) = TOP
            PMSA (IP(58)) = POP
            PMSA (IP(59)) = PIP
            PMSA (IP(60)) = FrAAP
            PMSA (IP(61)) = FrVAP
C
            PMSA (IP(62)) = TotSi
C
            PMSA (IP(63)) = POCnoa
            PMSA (IP(64)) = POMnoa
            PMSA (IP(65)) = PONnoa
            PMSA (IP(66)) = POPnoa
            PMSA (IP(67)) = POSnoa
            PMSA (IP(68)) = TOSnoa
C
            PMSA (IP(69)) = CN1
            PMSA (IP(70)) = CN2
            PMSA (IP(71)) = CN3
            PMSA (IP(72)) = CN4
            PMSA (IP(73)) = CP1
            PMSA (IP(74)) = CP2
            PMSA (IP(75)) = CP3
            PMSA (IP(76)) = CP4
            PMSA (IP(77)) = CS1
            PMSA (IP(78)) = CS2
            PMSA (IP(79)) = CS3
            PMSA (IP(80)) = CS4
C
C        ENDIF
C
         IFLUX = IFLUX + NOFLUX
         IP    = IP    + INCREM
C
 9000 CONTINUE
C
      RETURN
C
      END
