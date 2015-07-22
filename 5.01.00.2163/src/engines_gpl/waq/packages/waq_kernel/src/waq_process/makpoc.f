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

      subroutine makpoc ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Derive OOC from IM-fractions and percentage POM in IMx

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES
C     Author  : Pascal Boderie
C     Date    : 940730             Version : 0.04
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C     940730  Pascal Boderie  Create first version
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Uni
C ----    --- -  -    -------------------                            ---
C IMx     R*4 1 I  conversion factor for gX->dry matter substy [gDM/gm3]
C FrOMx   R*4 1 I  percentage OM in IMx                        [gOM/gDM]
C POC     R*4 1 I  particulate carbon content                   [gOC/m3]
C DMCF    R*4 1 I  Dry weitght of Organic Carbonin Part.Org.Mat[gDW/gOC]

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library

C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local
C
      REAL     IM1   , IM2   , IM3
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
C
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C
      IM1   = MAX(0.0, PMSA(IP1))
      IM2   = MAX(0.0, PMSA(IP2))
      IM3   = MAX(0.0, PMSA(IP3))
      FROM1 = PMSA(IP4) / 100.
      FROM2 = PMSA(IP5) / 100.
      FROM3 = PMSA(IP6) / 100.
      OCPOM = PMSA(IP7)

C***********************************************************************
C**** Calculations connected to the POC calculation
C***********************************************************************

C     Calculate amount OOC to be made
      OOC  =  IM1 * FROM1 + IM2 * FROM2 + IM3 * FROM3

C     Correct amount OOC
      SUM  = IM1 + IM2 + IM3
      IF (SUM .GT. 0.0) THEN
           FRAC = OOC / SUM
           OOC  = OOC / (1. - FRAC)
      ENDIF

      PMSA (IP8) = OOC
C
      ENDIF
C
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
      IP7   = IP7   + INCREM (  7 )
      IP8   = IP8   + INCREM (  8 )
C
 9000 CONTINUE
C
      RETURN
C
      END
