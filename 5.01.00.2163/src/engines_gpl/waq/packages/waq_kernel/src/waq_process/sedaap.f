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

      subroutine sedaap ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Sedimentation flux and velocity for PAP and AAP (adsorbed PO4)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    Water Resources and Environment     |
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
C     930210  Pascal Boderie  Version with adaptions for T692 (Delsta st
C     950216  M. Bokhorst     Add calculation sedimentation velocity
C     980904  Jos van Gils    Use of PMSA as work space removed
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C
C Name    T   L I/O   Description                                    Uni
C ----    --- -  -    -------------------                            ---
C SFL1    R*4 1 I  sedimentation flux carriers                 [gC/m2/d]
C Q1      R*4 1 I  quality of carrier                          [gOMV/gC]
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      REAL     HULP

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

      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
      IN8  = INCREM( 8)
      IN9  = INCREM( 9)
      IN10 = INCREM(10)
      IN11 = INCREM(11)
      IN12 = INCREM(12)
      IN13 = INCREM(13)
      IN14 = INCREM(14)
      IN15 = INCREM(15)
      IN16 = INCREM(16)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C

      SFL1  = PMSA(IP1 )
      SFL2  = PMSA(IP2 )
      SFL3  = PMSA(IP3 )
      Q1    = PMSA(IP4 )
      Q2    = PMSA(IP5 )
      Q3    = PMSA(IP6 )
      DEPTH = PMSA(IP10)
      SWITCH= PMSA(IP11)

C***********************************************************************
C**** Processes connected to the SEDIMENTATION of AAP
C***********************************************************************

C     SEDIMENTATION
      HULP = SFL1 * Q1 + SFL2 * Q2 + SFL3 * Q3
      PMSA(IP15) = HULP
      IF (ABS(SWITCH).LT.0.5) THEN
C       NO SWITCH
        FL( 1 + IFLUX ) =  HULP  / DEPTH
        FL( 2 + IFLUX ) =  0.0
      ELSE
C       SWITCH
        FL( 1 + IFLUX ) =  0.0
        FL( 2 + IFLUX ) =  HULP / DEPTH
      ENDIF

      ENDIF
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + IN1
      IP2   = IP2   + IN2
      IP3   = IP3   + IN3
      IP4   = IP4   + IN4
      IP5   = IP5   + IN5
      IP6   = IP6   + IN6
      IP10  = IP10  + IN10
      IP11  = IP11  + IN11
      IP15  = IP15  + IN15
c
 9000 CONTINUE

c.....Exchangeloop over de horizontale richting
      DO 8000 IQ= 1 , NOQ1+NOQ2

c........VxSedAAP op nul
         PMSA(IP16) = 0.0

         IP16 = IP16 + IN16

 8000 CONTINUE

c.....Startwaarde in de PMSA voor VxSedIMX in de 3e richting
      IP12= IP12+ ( NOQ1+NOQ2 ) * IN12
      IP13= IP13+ ( NOQ1+NOQ2 ) * IN13
      IP14= IP14+ ( NOQ1+NOQ2 ) * IN14

c.....Exchangeloop over de verticale richting
      DO 7000 IQ = NOQ1+NOQ2+1 , NOQ1+NOQ2+NOQ3

         IVAN  = IEXPNT(1,IQ)

         IF ( IVAN .GT. 0 ) THEN
            FPIM1 = PMSA( IP7 + (IVAN-1) * IN7 )
            FPIM2 = PMSA( IP8 + (IVAN-1) * IN8 )
            FPIM3 = PMSA( IP9 + (IVAN-1) * IN9 )
            VSIM1 = PMSA(IP12)
            VSIM2 = PMSA(IP13)
            VSIM3 = PMSA(IP14)
c...........berekenen VxSedAAP
            PMSA(IP16) = FPIM1*VSIM1+FPIM2*VSIM2+FPIM3*VSIM3
         ENDIF

c........Exchangepointers ophogen
         IP12= IP12+ IN12
         IP13= IP13+ IN13
         IP14= IP14+ IN14
         IP16= IP16+ IN16

 7000 CONTINUE

      RETURN
      END
