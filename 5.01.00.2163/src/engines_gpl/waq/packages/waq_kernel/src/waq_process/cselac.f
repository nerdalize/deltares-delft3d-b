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

      subroutine cselac ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Consumption of electron acceptors (new, generic!)

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
C     Date    : 020529        Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     020528  Johannes Smits  New generic process for consumption of
C                             electron acceptors connected with
C                             decomposition of organic matter
C     070502  Johannes Smits  Stricter check on availability of electron acceptors
C
C***********************************************************************
C
C     Description of the module :
C
C     General water quality module for DELWAQ:
C     The electron acceptors considered are dissolved oxygen,
C     nitrate, sulphate and carbon monoxide (org. matter).
C     The mineralisation fluxes of orgnaic matter fractions are added.
C     The consumption fluxes follow from the total mineralisation flux
C     and the relative contributions of the electron acceptors.
C     The relative consumptions of electron-acceptors O2, NO3, SO4 and
C     carbon monoxide (organic matter) are computed on the basis of
C     MM-kinetics for limitation and inhibition. The reduction of carbon
C     monoxide from organic matter results in methanogenesis.
C     The process is valid for overlying water as well as sediment.
C
C        ----- description of parameters -----
C Name    T   L I/O   Description                                 Units
C ----    --- -  -    -------------------                          ----
C CFEA    R*4 1 I particulate amorphous oxidizing iron          [gFe/m3]
C CNI     R*4 1 I concentration of nitrate                       [gN/m3]
C CNIC    R*4 1 I critical nitrate conc. for inh. methanogen.    [gN/m3]
C COX     R*4 1 I dissolved oxygen concentration                  [g/m3]
C COXC1   R*4 1 I critical diss. ox. conc. for deniftrif.         [g/m3]
C COXC2   R*4 1 I critical diss. ox. conc. for inh. iron. red.    [g/m3]
C COXC3   R*4 1 I critical diss. ox. conc. for inh. sulph. red.   [g/m3]
C COXC4   R*4 1 I critical diss. ox. conc. for inh. methanogen.   [g/m3]
C CSU     R*4 1 I concentration of sulphate                      [gS/m3]
C CRTEMP  R*4 1 I critical temp. for denitr., sulphate red., meth.  [oC]
C DELT    R*4 1 I computational timestep                             [d]
C FCT2    R*4 1 I reduction factor for denitr. below crit. temp.    [oC]
C FCT3    R*4 1 I red. factor for iron red. below crit. temp.       [oC]
C FCT4    R*4 1 I red. factor for sulphate red. below crit. temp.   [oC]
C FCT5    R*4 1 I reduction factor for methanog. below crit. temp.  [oC]
C FDEN    R*4 1 I reduction factor for denitr. below crit. temp.    [oC]
C FIRED   R*4 1 I red. factor for iron red. below crit. temp.       [oC]
C FSRED   R*4 1 I red. factor for sulphate red. below crit. temp.   [oC]
C FMET    R*4 1 I reduction factor for methanog. below crit. temp.  [oC]
C FL (1)  R*4 1 O mineralisation flux for oxygen consumption   [gC/m3/d]
C FL (2)  R*4 1 O mineralisation flux for denitrification      [gC/m3/d]
C FL (3)  R*4 1 O mineralisation flux for sulphate reduction   [gC/m3/d]
C FL (4)  R*4 1 O mineralisation flux for methanogenesis       [gC/m3/d]
C FOX     R*4 1 L unscaled relative contribution of DO cons.         [-]
C FOX20   R*4 1 L unscaled relative contribution of DO cons. at 20oC [-]
C FROX    R*4 1 L scaled relative contribution of DO cons.           [-]
C FROXC   R*4 1 O corrected fractional oxygen contribution           [-]
C FNI     R*4 1 L unscaled relative contribution of denitrification  [-]
C FNI20   R*4 1 L unscaled relative contribution of denitr. at 20 oC [-]
C FRNI    R*4 1 L scaled relative contribution of denitrification    [-]
C FRNIC   R*4 1 O corrected fractional nitrate contribution          [-]
C FFE20   R*4 1 L unscaled relative contribution of iron re. at 20 oC[-]
C FFE     R*4 1 L unscaled relative contribution of iron reduction   [-]
C FRFE    R*4 1 L scaled relative contribution of iron reduction     [-]
C FRFEC   R*4 1 O corrected fractional iron contribution             [-]
C FSU     R*4 1 L unscaled relative contribution of sulph. reduction [-]
C FSU20   R*4 1 L unscaled relative contr. of sulph. red. at 20 oC   [-]
C FRSU    R*4 1 L scaled relative contribution of sulph. reduction   [-]
C FRSUC   R*4 1 O corrected fractional sulphate contribution         [-]
C FCH4    R*4 1 L unscaled relative contribution of methanogenesis   [-]
C FCH420  R*4 1 L unscaled relative contr. of methanog. at 20 oC     [-]
C FRCH4   R*4 1 L scaled relative contribution of methanogenesis     [-]
C FRCH4C  R*4 1 O corrected fractional methane contribution          [-]
C FSUM    R*4 1 L sum of unscaled relative contributions             [-]
C FRNI    R*4 1 O scaled relative contribution of nitrate            [-]
C KSOX    R*4 1 I half saturation constant for DO limitation      [g/m3]
C KSNI    R*4 1 I half saturation constant for NI limitation     [gN/m3]
C KSSU    R*4 1 I half saturation constant for SU limitation     [gS/m3]
C KSOXI   R*4 1 I half saturation constant for inhibition by DO   [g/m3]
C KSNISUI R*4 1 I half saturation constant for inhibition by NI  [gN/m3]
C KSSUI   R*4 1 I half saturation constant for inhibition by SU  [gS/m3]
C KTDEN   R*4 1 I temperature coefficient for denitrifcation         [-]
C KTMET   R*4 1 I temperature coefficient for methanogenesis         [-]
C KTMIN   R*4 1 I temperature coefficient for aerobic mineralisation [-]
C KTIRED  R*4 1 I temperature coefficient for iron reduction         [-]
C KTSRED  R*4 1 I temperature coefficient for sulphate reduction     [-]
C POROS   R*4 1 I porosity                                           [-]
C RDEN    R*4 1 L proposed mineralisation rate for denitrif.   [gC/m3/d]
C RDMAX   R*4 1 L maximal mineralisation rate for denitrif.    [gC/m3/d]
C RIRED   R*4 1 L proposed mineral. rate for iron red.         [gC/m3/d]
C RSRED   R*4 1 L proposed mineral. rate for sulphate red.     [gC/m3/d]
C RIMAX   R*4 1 L maximal mineral. rate for iron red.          [gC/m3/d]
C RSMAX   R*4 1 L maximal mineral. rate for sulphate red.      [gC/m3/d]
C RMIN1   R*4 1 I mineralisation flux of POC1                  [gC/m3/d]
C RMIN2   R*4 1 I mineralisation flux of POC2                  [gC/m3/d]
C RMIN3   R*4 1 I mineralisation flux of POC3                  [gC/m3/d]
C RMIN4   R*4 1 I mineralisation flux of POC4                  [gC/m3/d]
C RMIN5   R*4 1 I mineralisation flux of POC5                  [gC/m3/d]
C RMIN6   R*4 1 I mineralisation flux of DOC                   [gC/m3/d]
C RTMIN   R*4 1 I total mineralisation flux                    [gC/m3/d]
C TEMP    R*4 1 I ambient temperature                               [oC]
C TEMPC1  R*4 1 L ambient temp. corr. function for aerobic mineral.  [-]
C TEMPC2  R*4 1 L ambient temp. corr. function for denitrification   [-]
C TEMPC3  R*4 1 L ambient temp. corr. function for iron red.         [-]
C TEMPC4  R*4 1 L ambient temp. corr. function for sulphate red.     [-]
C TEMPC5  R*4 1 L ambient temp. corr. function for methanogenesis    [-]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)            [oC]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT NONE
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT(43) , INCREM(43) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      INTEGER  IP(43)
      INTEGER  IN(43)
      INTEGER  IFLUX  , ISEG   , IKMRK1, ILUMON
C
      REAL     COX    , CNI    , CFEA   , CSU    , CCH4
      REAL     RMIN1  , RMIN2  , RMIN3  , RMIN4  , RTMIN  , RDEN   ,
     +         RSRED  , RDMAX  , RSMAX  , DELT   , RMIN5  , RMIN6
      REAL     FOX    , FNI    , FSU    , FCH4   , FOX20  , FNI20  ,
     +         FSU20  , FCH420 , FSUM   , FROX   , FRNI   , FRSU   ,
     +         FRCH4  , FROXC  , FRNIC  , FRSUC  , FRCH4C , FRMCH4
      REAL     KSOX   , KSOXI  , KSNI   , KSNISUI, KSSU   , KSSUI  ,
     +         KTMIN  , KTDEN  , KTSRED , KTMET  , FCT2   , FCT3   ,
     +         FCT4   , FDEN   , FIRED  , FSRED  , FMET   , COXC1  ,
     +         COXC2  , COXC3  , COXC4  , CNIC   , KSFE   , KSNIFEI
      REAL     KTIRED , FRFEC  , FCT5   , RIRED  , RIMAX  , FFE20  ,
     +         FFE    , FRFE
      REAL     POROS  , TEMP   , TEMPC1 , TEMPC2 , TEMPC3 , TEMPC4 ,
     +         TEMPC5 , CRTEMP , TEMP20
      REAL     OXC_RATIO
      REAL     NIC_RATIO
      REAL     FEC_RATIO
      REAL     SUC_RATIO
      REAL     CHC_RATIO
      REAL     ROMAX
      REAL     ROXC
      LOGICAL  FIRST, ONLY_OX
      SAVE     FIRST, ONLY_OX
      DATA     FIRST /.TRUE./
C
      CALL GETMLU(ILUMON)
C
      IN = INCREM
      IP = IPOINT
C
C     -----Warnings-----
C
      IF (FIRST) THEN
          IF (PMSA(IP(11)).LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : half saturation constant',
     +                          ' KsOxCon should be greater than zero'
          ELSEIF (PMSA(IP(12)).LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : half saturation constant',
     +                          ' KsNiDen should be greater than zero'
          ELSEIF (PMSA(IP(13)).LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : half saturation constant',
     +                          ' KsFeRed should be greater than zero'
          ELSEIF (PMSA(IP(14)) .LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : half saturation constant',
     +                          ' KsSuRed should be greater than zero'
          ELSEIF (PMSA(IP(15)) .LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : half saturation constant',
     +                          ' KsOxDen should be greater than zero'
          ELSEIF (PMSA(IP(16)) .LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : half saturation constant',
     +                          ' KsNiIRdInh should be greater than zero'
          ELSEIF (PMSA(IP(17)) .LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : half saturation constant',
     +                          ' KsNiSRdInh should be greater than zero'
          ELSEIF (PMSA(IP(18)) .LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : half saturation constant',
     +                          ' KsSuMet should be greater than zero'
          ELSEIF (PMSA(IP(35)) .LE. 0.0) THEN
              WRITE (ILUMON, *) 'WARNING : Poros, PORS1 or PORS2',
     +                          ' should be greater than zero'
          ENDIF
          IF (PMSA(IP(38)) .GT. 0.5) THEN
            ONLY_OX = .TRUE.
          ELSE
            ONLY_OX = .FALSE.
          ENDIF
          FIRST = .FALSE.
      ENDIF
C
C     Do the segment loop.
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF ( IKMRK1 .GT. 0 ) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
C           Read input. Adjust concentrations to zero, when negative.
C           Adjust half saturation const. to prevent division by zero.
C
            COX    = MAX (0.0 , PMSA(IP(1)) )
            CNI    = MAX (0.0 , PMSA(IP(2)) )
            CFEA   = MAX (0.0 , PMSA(IP(3)) )
            CSU    = MAX (0.0 , PMSA(IP(4)) )
            RMIN1  = PMSA(IP(5 ))
            RMIN2  = PMSA(IP(6 ))
            RMIN3  = PMSA(IP(7 ))
            RMIN4  = PMSA(IP(8 ))
            RMIN5  = PMSA(IP(9 ))
            RMIN6  = PMSA(IP(10))
            KSOX   = MAX (1.0E-06 , PMSA(IP(11)) )
            KSNI   = MAX (1.0E-06 , PMSA(IP(12)) )
            KSFE   = MAX (1.0E-06 , PMSA(IP(13)) )
            KSSU   = MAX (1.0E-06 , PMSA(IP(14)) )
            KSOXI  = MAX (1.0E-06 , PMSA(IP(15)) )
            KSNIFEI= MAX (1.0E-06 , PMSA(IP(16)) )
            KSNISUI= MAX (1.0E-06 , PMSA(IP(17)) )
            KSSUI  = MAX (1.0E-06 , PMSA(IP(18)) )
            KTMIN  = PMSA(IP(19))
            KTDEN  = PMSA(IP(20))
            KTIRED = PMSA(IP(21))
            KTSRED = PMSA(IP(22))
            KTMET  = PMSA(IP(23))
            FDEN   = PMSA(IP(24))
            FIRED  = PMSA(IP(25))
            FSRED  = PMSA(IP(26))
            FMET   = PMSA(IP(27))
            COXC1  = PMSA(IP(28))
            COXC2  = PMSA(IP(29))
            COXC3  = PMSA(IP(30))
            COXC4  = PMSA(IP(31))
            CNIC   = PMSA(IP(32))
            CRTEMP = PMSA(IP(33))
            TEMP   = PMSA(IP(34))
            POROS  = PMSA(IP(35))
            DELT   = PMSA(IP(36))
            FRMCH4 = PMSA(IP(37))
C
C           Calculate the sum of the mineralisation fluxes
C
            RTMIN = RMIN1 + RMIN2 + RMIN3 + RMIN4 + RMIN5 + RMIN6
            
            IF (ONLY_OX) THEN
              FROXC = 1.
              FRNIC = 0.
              FRFEC = 0.
              FRSUC = 0.
              FRCH4C = 0.
              
            ELSE
C
C           Calculation of the temperature dependency coefficients
C
            IF ( TEMP .GE. CRTEMP ) THEN
                  FCT2 = 1.0
                  FCT3 = 1.0
                  FCT4 = 1.0
                  FCT5 = 1.0
            ELSE
                  FCT2 = FDEN
                  FCT3 = FIRED
                  FCT4 = FSRED
                  FCT5 = FMET
            ENDIF
C
            TEMP20 = TEMP - 20.0
            TEMPC1 = KTMIN ** TEMP20
            TEMPC2 = FCT2 * KTDEN ** TEMP20
            TEMPC3 = FCT3 * KTIRED ** TEMP20
            TEMPC4 = FCT4 * KTSRED ** TEMP20
            TEMPC5 = FCT5 * KTMET ** TEMP20
C
C           Calculation of the unscaled (relative) contributions
C
            FOX20  = COX / (KSOX * POROS + COX)
C
            FNI20  = ( CNI / (KSNI * POROS + CNI) ) *
     +               ( 1 - COX / (KSOXI * POROS + COX) )
C
            FFE20  = ( CFEA / (KSFE * POROS + CFEA) ) *
     +               ( 1 - CNI / (KSNIFEI * POROS + CNI) )
C
            FSU20  = ( CSU / (KSSU * POROS + CSU) ) *
     +               ( 1 - CNI / (KSNISUI * POROS + CNI) )
C
            FCH420 = 1 - CSU / (KSSUI * POROS + CSU)
C
C           Adjust for temperature dependency.
C
            FOX    = FOX20  * TEMPC1
            FNI    = FNI20  * TEMPC2
            FFE    = FFE20  * TEMPC3
            FSU    = FSU20  * TEMPC4
            FCH4   = FCH420 * TEMPC5
C
C           Correction of the unscaled contributions for too high
C           dissolved oxygen or nitrate concentrations to allow
C           sulphate reduction or methanogenesis.
C
            IF ( COX .GE. (COXC1*POROS) )  FNI = 0.0
            IF ( COX .GE. (COXC2*POROS) )  FFE = 0.0
            IF ( COX .GE. (COXC3*POROS) )  FSU = 0.0
            IF ( COX .GE. (COXC4*POROS) .OR.
     +           CNI .GE. (CNIC*POROS) )  FCH4 = 0.0
C
C           Calculation of the scaled contributions
C
            FSUM  = FOX + FNI + FFE + FSU + FCH4
            FROX  = FOX / FSUM
            FRNI  = FNI / FSUM
            FRFE  = FFE / FSUM
            FRSU  = FSU / FSUM
            FRCH4 = 1 - (FROX + FRNI + FRFE + FRSU)
C
C           Calculate the maximal and proposed consumption fluxes in Carbon equivalents of
C           the electron acceptors oxygen, nitrate and sulphate
C           use a safety margin 0.5 on oxygen 0.9 on the rest
C
            OXC_RATIO = 2.667
            NIC_RATIO = 0.933
            FEC_RATIO = 18.67
            SUC_RATIO = 1.333
            CHC_RATIO = 0.5
            ROMAX = COX / OXC_RATIO / DELT * 0.5
            RDMAX = CNI / NIC_RATIO / DELT * 0.9
            RIMAX = CFEA / FEC_RATIO / DELT * 0.9
            RSMAX = CSU / SUC_RATIO / DELT * 0.9
C
C           Correct scaled contributions for availability of electron
C           acceptors nitrate, oxygen, iron and sulphate
C
            RDEN  = FRNI  * RTMIN
            IF ( RDEN .GT. RDMAX .AND. RDEN .GT. 0.0 ) THEN
                   FRNIC = FRNI * RDMAX / RDEN
                   FROX  = FROX + FRNI - FRNIC
            ELSE
                   FRNIC = FRNI
            ENDIF
C
            ROXC  = FROX  * RTMIN
            IF ( ROXC .GT. ROMAX .AND. ROXC .GT. 0.0 ) THEN
                   FROXC = FROX * ROMAX / ROXC
                   FRFE  = FRFE + FROX - FROXC
            ELSE
                   FROXC = FROX
            ENDIF
C
            RIRED  = FRFE  * RTMIN
            IF ( RIRED .GT. RIMAX .AND. RIRED .GT. 0.0 ) THEN
                   FRFEC = FRFE * RIMAX / RIRED
                   FRSU  = FRSU + FRFE - FRFEC
            ELSE
                   FRFEC = FRFE
            ENDIF
C
            RSRED  = FRSU  * RTMIN
            IF ( RSRED .GT. RSMAX .AND. RSRED .GT. 0.0 ) THEN
                   FRSUC = FRSU * RSMAX / RSRED
            ELSE
                   FRSUC = FRSU
            ENDIF
C
            FRCH4C = 1 - (FROXC + FRNIC + FRFEC + FRSUC)
            
C           endif ONLY_OX
            ENDIF
C
C           Calculate the electron acceptor consumption fluxes
C
            FL( 1+IFLUX ) = FROXC  * RTMIN
            FL( 2+IFLUX ) = FRNIC  * RTMIN
            FL( 3+IFLUX ) = FRFEC  * RTMIN
            FL( 4+IFLUX ) = FRSUC  * RTMIN
            FL( 5+IFLUX ) = FRCH4C * FRMCH4 * RTMIN
            FL( 6+IFLUX ) = FRCH4C * (1.-FRMCH4) * RTMIN
C
C           The corrected scaled contributions are output
C
            PMSA(IP(39)) = FROXC
            PMSA(IP(40)) = FRNIC
            PMSA(IP(41)) = FRFEC
            PMSA(IP(42)) = FRSUC
            PMSA(IP(43)) = FRCH4C
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP   = IP   + IN
C
 9000 CONTINUE
C
      RETURN
C
      END
