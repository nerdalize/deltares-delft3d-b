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

      subroutine oxymin ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Potential daily mimimum dissolved oxygen concentration

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  : Marnix van der Vat
C     Date    : 950905             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C     950905  Marnix vd Vat   Create first version, for Azov T1113
C     010710  Johannes Smits  Modified to take minimum of two estimates
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        CALCULATES THE POTENTIAL MINIMUM OXYGEN CONCENTRATION
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C OXY     R*4 1 I  oxygen concentration                          [gO2/m3]
C GREEN   R*4 1 I  concentration green algae                      [gC/m3]
C PGREEN  R*4 1 I  production green algae                           [1/d]
C RGREEN  R*4 1 I  respiration green algae                          [1/d]
C DIAT    R*4 1 I  concentration diatoms                          [gC/m3]
C PDIAT   R*4 1 I  production diatoms                               [1/d]
C RDIAT   R*4 1 I  respiration diatoms                              [1/d]
C CMINDO  R*4 1 O  minimum oxygen concentration                  [gO2/m3]
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C

      OXY    = PMSA(IP1 )
      GREEN  = PMSA(IP2 )
      PGREEN = PMSA(IP3 )
      RGREEN = PMSA(IP4 )
      DIAT   = PMSA(IP5 )
      PDIAT  = PMSA(IP6 )
      RDIAT  = PMSA(IP7 )
      DL     = PMSA(IP8 )


C     CALCULATE MINIMUM OXYGEN FROM PRODUCTION AND RESPIRATION
C     CORRECT FOR DAY - NIGHT SEQUENCE
C     original tentative estimate:
C     CMINDO = OXY - 2.67 * (PROD - DL*RESP)
C     replaced with the minimum of two estimates, one assuming
C     occurrence or non-occurrence of compensation by reaeration
C
C     first calculate total gross production and respiration
C
      PROD = (PGREEN + RGREEN) * GREEN + (PDIAT + RDIAT) * DIAT
      RESP = RGREEN * GREEN + RDIAT * DIAT
C
C     estimate without compensation
C
      CMINDO1 = OXY - 0.5 * 2.67 * RESP *(1 - DL)
C
C     estimate with compensation on a daily average basis
C
      CMINDO2 = OXY - 0.5 * 2.67 * PROD *(1 - DL)
C
C     take the minimal value of the two estimates
C
      CMINDO = MIN (CMINDO1 , CMINDO2)

      PMSA(IP9 ) = CMINDO

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
c
 9000 CONTINUE
c
      RETURN
C
      END
