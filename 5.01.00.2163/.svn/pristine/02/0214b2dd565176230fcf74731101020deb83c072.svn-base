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

      subroutine resant ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Resuspension of nutrients in organic carbon matrix

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
C     980428  Jos van Gils    Use f-flux in stead of d-flux
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C RFLS1-2 R*4 1 I  sedimention flux organic from S1 or S2        [gX/m2/d]
C CNS1-2  R*4 1 I  C-N ratio substance in S1-S2                    [gC/gN]
C CPS1-2  R*4 1 I  C-P ratio substance in S1-S2                    [gC/gP]
C CSS1-2  R*4 1 I  C-S ratio substance in S1-S2                    [gC/gS]
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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C

      RFLS1  = PMSA(IP1 )
      CNS1   = PMSA(IP2 )
      CPS1   = PMSA(IP3 )
      CSS1   = PMSA(IP4 )
      DEPTH  = PMSA(IP5 )
      IF ( DEPTH .GT. 0.0 ) THEN
          RFLS1 = RFLS1 / DEPTH
      ELSE
          RFLS1 = 0.0
      ENDIF

C*******************************************************************************
C**** Processes connected to the SEDIMENTAION and RESUSENSION
C***********************************************************************

C     RESUSPENSION

C
         FL( 1 + IFLUX ) = RFLS1 * CNS1

         FL( 2 + IFLUX ) = RFLS1 * CPS1

         FL( 3 + IFLUX ) = RFLS1 * CSS1

      ENDIF
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
c
 9000 CONTINUE
c

      RETURN
C
      END
