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

      subroutine secchi ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Calculation secchi depth for visible-light (370-680nm)

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
C
C     120925  Johannes Smits  adaption for POC1-4 and DOC
C     980716  Jos van Gils    Secchi for non-UITZICHT mode added
C     951207  Marnix vd Vat   Optional addition of UITZICHT module
C     940725  Jos van Gils    Remove contribution of algae
C     921210  Pascal Boderie  Create first version, based on T721.13
C                             created by Jos van Gils
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                              Units
C ----    --- -  -    -------------------                      ----
C EXT     R*4 1 I total extinction coefficient                   [1/m]
C AIM1    R*4 1 I inorganic suspended matter 1                  [g/m3]
C AIM2    R*4 1 I inorganic suspended matter 2                  [g/m3]
C AIM3    R*4 1 I inorganic suspended matter 3                  [g/m3]
C POC1    R*4 1 I fast decomposing detritus                    [gC/m3]
C POC2    R*4 1 I medium decomposing detritus                  [gC/m3]
C POC3    R*4 1 I slow decomposing detritus                    [gC/m3]
C POC4    R*4 1 I refractory detritus                          [gC/m3]
C AH_380  R*4 1 I extinction of dissolved organic matter         [1/m]
C CHLORP  R*4 1 I chlorophyll-a concentration                  [mg/m3]
C SW_UIT  R*4 1 I extinction by UITZUCHT on (1) or Off (0)         [-]
C DIEP1   R*4 1 I argument UITZICHT
C DIEP2   R*4 1 I argument UITZICHT
C CORCHL  R*4 1 I argument UITZICHT
C C_DET   R*4 1 I argument UITZICHT
C C_GL1   R*4 1 I argument UITZICHT
C C_GL2   R*4 1 I argument UITZICHT
C HELHUM  R*4 1 I argument UITZICHT
C TAU     R*4 1 I argument UITZICHT
C ANGLE   R*4 1 I argument UITZICHT
C DETCDM  R*4 1 I dry matter carbon ratio detritus               [g/g]
C PAC     R*4 1 I Poole-Atkins constant                            [-]
C SECCHI  R*4 1 O secchi depth                                     [m]
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT REAL (A-H,J-Z)
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( 23 ) , INCREM(23) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      INTEGER  IP(23)
      INTEGER  IFLUX , ISEG  , IKMRK1
      REAL     AH_380, EXT   , PAC  , SECCH, AIM1  , AIM2  , AIM3  ,
     J         POC1  , POC2  , POC3  , POC4  , CHLORP, DIEP1 , DIEP2 ,
     J         CORCHL, C_DET , C_GL1 , C_GL2 , HELHUM, TAU   , ANGLE ,
     J         DETCDM, GLOEIR, DETRIC, EXTIO , EXTP_D, D_1   , SW_UITZ
C
      IP  = IPOINT
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
          SW_UITZ = PMSA(IP(11))
          IF (NINT(SW_UITZ) .EQ. 0) THEN
C
C  Calculate secchi depth without UITZICHT
C
              EXT = PMSA(IP(1))
              PAC = PMSA(IP(22))
              IF ( EXT .GT. 0.0 ) THEN
                  SECCH = PAC/EXT
              ELSE
                  SECCH = -999.
              ENDIF
C
          ELSE
C
C  Calculate secchi depth with UITZICHT
C
              AIM1   = PMSA(IP(2))
              AIM2   = PMSA(IP(3))
              AIM3   = PMSA(IP(4))
              POC1   = PMSA(IP(5))
              POC2   = PMSA(IP(6))
              POC3   = PMSA(IP(7))
              POC4   = PMSA(IP(8))
              AH_380 = PMSA(IP(9))
              CHLORP = PMSA(IP(10))
              DIEP1  = PMSA(IP(12))
              DIEP2  = PMSA(IP(13))
              CORCHL = PMSA(IP(14))
              C_DET  = PMSA(IP(15))
              C_GL1  = PMSA(IP(16))
              C_GL2  = PMSA(IP(17))
              HELHUM = PMSA(IP(18))
              TAU    = PMSA(IP(19))
              ANGLE  = PMSA(IP(20))
              DETCDM = PMSA(IP(21))
C
              DETRIC = MAX ( 0.0, DETCDM * (POC1 + POC2 + POC3 + POC4) )
              GLOEIR = AIM1 + AIM2 + AIM3
C
C  Calculate total extinction with UITZICHT
C
              CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1                     C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2                     DETRIC, GLOEIR, AH_380, SECCH, D_1   ,
     3                     EXTIO , EXTP_D,.TRUE.)
C
          ENDIF
C
          PMSA(IP(23)) = SECCH
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP   = IP   + INCREM
C
 9000 CONTINUE
C
      RETURN
C
      END
