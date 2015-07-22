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

      subroutine extinc ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Calculation total and partial extinction coefficients

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
C     120925  Johannes Smits  restructed for output of all partial coefficients
C     120917  Jos van Gils    4 POC fractions included
C     120127  Jos van Gils    Explicit switch on Uitzicht
C     000519  Jos van Gils    Flag for Scchi computation added to UIT_ZI
C     980506  Arno Nolte      Removed contribution of POC to ExtVlPOC
C     980123  Jos van Gils    Add salinity effect non-UITZICHT mode
C     951207  Marnix vd Vat   Optional addition of UITZICHT module
C     940725  Jos van Gils    Remove contribution of algae
C     921210  Pascal Boderie  Create first version, based on T721.13
C                             created by Jos van Gils
C
C***********************************************************************
C
C     This module calculates the total and partial extinction coeffcients,
C     optionally with additional module UITZICHT
c     In input DOC has replaced DisHum for UITZICHT
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------
C
      IMPLICIT NONE
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT(38) , INCREM(38) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local declaration
C
      REAL A1    !  R*4 1 I specific ext. inorganic suspended matter 1  [m2/gDM]
      REAL A2    !  R*4 1 I specific ext. inorganic suspended matter 2  [m2/gDM]
      REAL A3    !  R*4 1 I specific ext. inorganic suspended matter 3  [m2/gDM]
      REAL EXT    ! R*4 1 O total extinction                               [1/m]
      REAL EXTIM  ! R*4 1 O calculated extinction IM                       [1/m]
      REAL EXTPOC ! R*4 1 O extinction POC                                 [1/m]
      REAL EXTDOC ! R*4 1 O extinction DOC                                 [1/m]
      REAL EXT0    !R*4 1 I background extinction                          [1/m]
      REAL EXTALG  !R*4 1 I extinction algae                               [1/m]
      REAL EXTMAC  !R*4 1 I extinction macrophytes                         [1/m]
      REAL EXTSAL  !R*4 1 O extinction DOC for fresh water fraction        [1/m]
      REAL AIM1    !R*4 1 I suspended solids  fraction 1                [gDM/m3]
      REAL AIM2    !R*4 1 I suspended solids  fraction 2                [gDM/m3]
      REAL AIM3    !R*4 1 I suspended solids  fraction 3                [gDM/m3]
      REAL POC1  !  R*4 1 I fast decomposing detritus                    [gC/m3]
      REAL POC2  !  R*4 1 I medium decomposing detritus                  [gC/m3]
      REAL POC3  !  R*4 1 I slow decomposing detritus                    [gC/m3]
      REAL POC4  !  R*4 1 I refractory detritus                          [gC/m3]
      INTEGER SW_UIT  ! Extinction by UITZICHT on (1) or Off (0)             [-]
      REAL DOC     ! R*4 1 I dissolved organic carbon                    [gC/m3]
      REAL ADOC    ! R*4 1 I Specific extinction of DOC                  [m2/gC]
      REAL DIEP1   ! R*4 1 I argument UITZICHT
      REAL DIEP2   ! R*4 1 I argument UITZICHT
      REAL CORCHL  ! R*4 1 I argument UITZICHT
      REAL C_DET   ! R*4 1 I argument UITZICHT
      REAL C_GL1   ! R*4 1 I argument UITZICHT
      REAL C_GL2   ! R*4 1 I argument UITZICHT
      REAL HELHUM  ! R*4 1 I argument UITZICHT
      REAL TAU     ! R*4 1 I argument UITZICHT
      REAL ANGLE   ! R*4 1 I argument UITZICHT
      REAL DETCDM  ! R*4 1 I dry matter carbon ratio detritus              [g/g]
      REAL XTSAL0  ! R*4 1 I extra VL extinction at Salinity = 0           [1/m]
      REAL SALMAX  ! R*4 1 I salinity value for extra extinction = 0      [g/kg]
      REAL SALIN   ! R*4 1 I actual salinity                              [g/kg]
      REAL APOC1   ! R*4 1 I specific extintion POC1                [1/m/(G/M3)]
      REAL APOC2   ! R*4 1 I specific extintion POC2                [1/m/(G/M3)]
      REAL APOC3   ! R*4 1 I specific extintion POC3                [1/m/(G/M3)]
      REAL APOC4   ! R*4 1 I specific extintion POC4                [1/m/(G/M3)]
C
      REAL CHLORP, DETRIC, GLOEIR, AH_380
      REAL SECCHI, D_1   , EXTP_D, EXTDET, EXTGL, EXTHUM
      INTEGER      IFLUX, ISEG
C
      INTEGER  IPNT(38)
      INTEGER  NR_MES
      SAVE     NR_MES
      DATA     NR_MES / 0 /
C
      IPNT = IPOINT
      IFLUX = 0
C
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      A1        = PMSA(IPNT(1))
      A2        = PMSA(IPNT(2))
      A3        = PMSA(IPNT(3))
      APOC1     = PMSA(IPNT(4))
      EXT0      = PMSA(IPNT(5))
      EXTALG    = PMSA(IPNT(6))
      EXTMAC    = PMSA(IPNT(7))
      AIM1      = PMSA(IPNT(8))
      AIM2      = PMSA(IPNT(9))
      AIM3      = PMSA(IPNT(10))
      POC1      = PMSA(IPNT(11))
      POC2      = PMSA(IPNT(12))
      SW_UIT    = NINT(PMSA(IPNT(13)))
      DOC       = PMSA(IPNT(14))
      ADOC      = PMSA(IPNT(15))
      DIEP1     = PMSA(IPNT(16))
      DIEP2     = PMSA(IPNT(17))
      CORCHL    = PMSA(IPNT(18))
      C_DET     = PMSA(IPNT(19))
      C_GL1     = PMSA(IPNT(20))
      C_GL2     = PMSA(IPNT(21))
      HELHUM    = PMSA(IPNT(22))
      TAU       = PMSA(IPNT(23))
      ANGLE     = PMSA(IPNT(24))
      DETCDM    = PMSA(IPNT(25))
      XTSAL0    = PMSA(IPNT(26))
      SALIN     = PMSA(IPNT(27))
      SALMAX    = PMSA(IPNT(28))
      APOC2     = PMSA(IPNT(29))
      APOC3     = PMSA(IPNT(30))
      APOC4     = PMSA(IPNT(31))
      POC3      = PMSA(IPNT(32))
      POC4      = PMSA(IPNT(33))
C
      IF (SW_UIT.EQ.0) THEN
C
C  calculate extinction coefficients - no UITZICHT
C
        EXTIM  =  A1 * AIM1  + A2 * AIM2 + A3 * AIM3
        EXTPOC =  APOC1*POC1 + APOC2*POC2 + APOC3*POC3 + APOC4*POC4
        EXTDOC =  ADOC*DOC
        SALIN  =  MIN(SALIN,SALMAX)
        SALIN  =  MAX(SALIN,0.0)
        EXTSAL =  XTSAL0 * (1.0-SALIN/SALMAX)
        EXT    =  EXT0 + EXTIM + EXTPOC + EXTDOC + EXTALG  + EXTMAC
     J            + EXTSAL
C
        IF ( EXT .LT. 1.0E-20 ) THEN
           IF ( NR_MES .LT. 25 ) THEN
              NR_MES = NR_MES + 1
              WRITE(*,*) ' WARNING : zero or negative extinction'
              WRITE(*,*) ' Extinction due to inorganic matter:',EXTIM
              WRITE(*,*) ' Extinction due to organic matter  :',EXTPOC
              WRITE(*,*) ' Extinction due to algae           :',EXTALG
              WRITE(*,*) ' Background extinction             :',EXT0
              WRITE(*,*) ' Extinction by macrophytes         :',EXTMAC
              WRITE(*,*) ' In segment number                 :',ISEG
              WRITE(*,*) ' Background extinction is assumed.'
           ENDIF
           IF ( NR_MES .EQ. 25 ) THEN
              NR_MES = NR_MES + 1
              WRITE(*,*) ' 25 WARNINGS on extinction'
              WRITE(*,*) ' Further messages on extinction surpressed'
           ENDIF
           IF ( EXT0 .LT. 1.E-20 ) THEN
              EXT = 1.E-15
           ELSE
              EXT = EXT0
           ENDIF
        ENDIF
C
      ELSE
C
C  calculate extinction coefficients - with UITZICHT
C
        CHLORP = 0.0
        DETRIC = MAX ( 0.0, DETCDM * (POC1 +POC2 +POC3 + POC4))
        AH_380 = DOC*ADOC
        GLOEIR = AIM1 + AIM2 + AIM3
C
C  total extinction coefficient exclusive of algae, macrophytes and background
C
        CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1               C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2               DETRIC, GLOEIR, AH_380, SECCHI, D_1   ,
     3               EXT   , EXTP_D,.FALSE.)
C
C  total extinction coefficient of detritus
C
        CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1               C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2               0.0   , GLOEIR, AH_380, SECCHI, D_1   ,
     3               EXTDET, EXTP_D,.FALSE.)
        EXTDET = EXT - EXTDET
C
C  total extinction coefficient of inorganic sediment
C
        CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1               C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2               DETRIC, 0.0   , AH_380, SECCHI, D_1   ,
     3               EXTGL , EXTP_D,.FALSE.)
        EXTGL  = EXT - EXTGL
C
C  total extinction coefficient of DOC (humic acids)
C
        CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1               C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2               DETRIC, GLOEIR, 0.0   , SECCHI, D_1   ,
     3               EXTHUM, EXTP_D,.FALSE.)
        EXTHUM = EXT - EXTHUM
C
        EXTIM  =  EXTGL
        EXTPOC =  EXTDET
        EXTDOC =  EXTHUM
        EXTSAL =  0.0
        EXT    =  EXT0 + EXT + EXTALG + EXTMAC
C
      ENDIF
C
      PMSA(IPNT(34)) = EXT
      PMSA(IPNT(35)) = EXTIM
      PMSA(IPNT(36)) = EXTPOC
      PMSA(IPNT(37)) = EXTDOC
      PMSA(IPNT(38)) = EXTSAL
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IPNT  = IPNT  + INCREM
C
 9000 CONTINUE
C
      RETURN
C
      END
