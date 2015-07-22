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

      subroutine sedhm  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Sedimentation flux and velocity of adsorbed heavy metals

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
C     921210  Pascal Boderie  Create first version, based on T890 SLIB
C     930210  Pascal Boderie  Version with adaptions for T692 (Delsta study)
C     950216  M. Bokhorst     Add calculation sedimentation velocity
C   20000419  Jan van Beek    Check on dummy exchanges (0->0)
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C FL1-5   R*4 1 I  flux from a layer                               [gX/m2/d]
C Q1-5    R*4 1 I  fraction substance the layer                    [gOMV/gX]
C DEPTH   R*4 1 I  depth                                                 [m]
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
      IN17 = INCREM(17)
      IN18 = INCREM(18)
      IN19 = INCREM(19)
      IN20 = INCREM(20)
      IN21 = INCREM(21)
      IN22 = INCREM(22)
      IN23 = INCREM(23)

      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN

      FL1    = PMSA(IP1 )
      FL2    = PMSA(IP2 )
      FL3    = PMSA(IP3 )
      FL4    = PMSA(IP4 )
      FL5    = PMSA(IP5 )
      Q1     = PMSA(IP6 )
      Q2     = PMSA(IP7 )
      Q3     = PMSA(IP8 )
      Q4     = PMSA(IP9 )
      Q5     = PMSA(IP10)
      DEPTH  = PMSA(IP11)

C*******************************************************************************
C**** Processes connected to the BURIAL and DIGGING
C***********************************************************************

C.....SEDIMENTATION
      FL( 1 + IFLUX) =  ( FL1 * Q1 +
     &            FL2 * Q2 +
     &            FL3 * Q3 +
     &            FL4 * Q4 +
     &            FL5 * Q5 ) / DEPTH

C.....SEDIMENTATION SCALED
      PMSA(IP22) =  FL (1+IFLUX) * DEPTH

      ENDIF
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
      IP22  = IP22  + INCREM ( 22 )

 9000 CONTINUE

c.....Exchangeloop over de horizontale richting
      DO 8000 IQ=1,NOQ1+NOQ2

c........VxSedHM op nul
         PMSA(IP23) = 0.0

         IP23 = IP23 + IN23

 8000 CONTINUE

c.....Startwaarden VxSedIM1, 2 en 3, VxSedPOC en VxSedPhyt
      IP17 = IP17 + ( NOQ1+NOQ2 ) * IN17
      IP18 = IP18 + ( NOQ1+NOQ2 ) * IN18
      IP19 = IP19 + ( NOQ1+NOQ2 ) * IN19
      IP20 = IP20 + ( NOQ1+NOQ2 ) * IN20
      IP21 = IP21 + ( NOQ1+NOQ2 ) * IN21

c.....Exchangeloop over de verticale richting
      DO 7000 IQ=NOQ1+NOQ2+1,NOQ1+NOQ2+NOQ3

         IVAN  = IEXPNT(1,IQ)

         IF ( IVAN .GT. 0 ) THEN
            FHMIM1 = PMSA( IP12 + (IVAN-1) * IN12 )
            FHMIM2 = PMSA( IP13 + (IVAN-1) * IN13 )
            FHMIM3 = PMSA( IP14 + (IVAN-1) * IN14 )
            FHMPOC = PMSA( IP15 + (IVAN-1) * IN15 )
            FHMPHY = PMSA( IP16 + (IVAN-1) * IN16 )

            VSIM1 = PMSA(IP17)
            VSIM2 = PMSA(IP18)
            VSIM3 = PMSA(IP19)
            VSPOC = PMSA(IP20)
            VSPHY = PMSA(IP21)

c...........Berekenen VxSedHM
            VSHM = FHMIM1*VSIM1 +
     &             FHMIM2*VSIM2 +
     &             FHMIM3*VSIM3 +
     &             FHMPOC*VSPOC +
     &             FHMPHY*VSPHY

c...........VxSedHM toekennen aan de PMSA
            PMSA(IP23) = VSHM
         ENDIF

c........Exchangepointers ophogen
         IP17 = IP17 + IN17
         IP18 = IP18 + IN18
         IP19 = IP19 + IN19
         IP20 = IP20 + IN20
         IP21 = IP21 + IN21
         IP23 = IP23 + IN23

 7000 CONTINUE

      RETURN
      END
