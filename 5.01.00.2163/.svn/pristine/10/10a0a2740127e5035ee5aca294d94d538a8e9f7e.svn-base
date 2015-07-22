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

      subroutine calsed ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Sedimentation velocity IMx, DetC OOC, BODC, all algea = f (Temp SS Sal)

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
C     Date    : 941122             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     941122  Pascal Boderie  Create first version
C   20000419  Jan van Beek    Check on dummy exchanges (0->0)
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        SEDIMENTATION VELOCITY BASED ON TEMP, SUSPENDED SOLID CONC AND
C        SALINITY
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            -----
C CRSUSP  R*4 1 I  critical susp solid conc. for flocculation     [gDM/m3]
C N       R*4 1 I  coefficient in sedimentation formulation            [-]
C SUSP    R*4 1 I  total suspended solid concentration            [gDM/m3]
C SEDTC   R*4 1 I  temperature coefficient for sedimentation           [-]
C TEMP    R*4 1 I  ambient temperature                             [gradC]
C V0SED   R*4 1 I  sedimentaion velocity (no temp, sal, ss influence)[m/d]
C SAL     R*4 1 I  salinity                                         [g/kg]
C MAXSAL  R*4 1 I  salinity where salinity function is at max       [g/kg]
C ENHFAC  R*4 1 I  enhancement factor in salinity functin              [-]
C SALFUN  R*4 1 I  salinity function on sedimentation velocity         [-]
C FLOFUN  R*4 1 I  flocculation function on sedimentation velocity     [-]
C TEMFUN  R*4 1 I  temperature function on sedimentation velocity      [-]
C VSED    R*4 1 I  sedimentaion velocity, temp, sal, ss corrected    [m/d]

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
      PARAMETER ( PI     = 3.14159265 )
      INTEGER  NOQ
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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
c     CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
c     IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C
      V0SED   = PMSA( IP1 )
      SUSP    = MAX (PMSA( IP2 ), 0.0)
      CRSUSP  = PMSA( IP3 )
      N       = PMSA( IP4 )
      TEMP    = PMSA( IP5 )
      SEDTC   = PMSA( IP6 )
      SAL     = MAX (PMSA( IP7 ), 0.0)
      MAXSAL  = PMSA( IP8 )
      ENHFAC  = PMSA( IP9 )

      IF (CRSUSP .LT. 1E-20 )  CALL ERRSYS ('CRSUSP in CALSED zero', 1 )

C*******************************************************************************
C**** Processes connected to the sedimentation VELOCITY
C***********************************************************************


C     Initialisatie
      FLOFUN = 1.0
      SALFUN = 1.0
      TEMFUN = 1.0

C     Flocculatie functie

      IF ( SUSP/CRSUSP .GE. 1.E-30 ) THEN
          FLOFUN = (SUSP / CRSUSP)**N
      ENDIF

C     Temperatuur functie

      IF (SEDTC .NE. 1.0) THEN
          TEMFUN = SEDTC **(TEMP-20.0)
      ENDIF

C     Salinity functie

      IF ( SAL .LT. MAXSAL ) THEN
         SALFUN = (ENHFAC + 1.)/2.+ ((ENHFAC-1.)/2.)*COS(PI*SAL/MAXSAL)
      ELSEIF (MAXSAL .GE. 0.0) THEN
         SALFUN = ENHFAC
      ELSE
         SALFUN = 1.0
      ENDIF

C     Bereken VSED
      VSED = V0SED * TEMFUN * SALFUN * FLOFUN

C     Output of calculated sedimentation rate
      PMSA ( IP10 ) = VSED
      PMSA ( IP11 ) = SALFUN
      PMSA ( IP12 ) = FLOFUN
C
c     ENDIF
      ENDIF
C
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
C
 9000 CONTINUE
C

      NOQ = NOQ1 + NOQ2 + NOQ3

      IP10 = IPOINT(10)
      IN10 = INCREM(10)
      IP13 = IPOINT(13)
      IN13 = INCREM(13)

      DO 8000 IQ=1,NOQ1+NOQ2

         PMSA(IP13) = 0.0

         IP13 = IP13 + IN13

 8000 CONTINUE

      DO 7000 IQ=NOQ1+NOQ2+1,NOQ

         IVAN = IEXPNT(1,IQ)
C
C        Sedimentation velocity from segment to exchange-area
C
         IF ( IVAN .GT. 0 ) THEN
            PMSA(IP13) = PMSA( IP10 + (IVAN-1) * IN10 )
         ENDIF

         IP13 = IP13 + IN13

 7000 CONTINUE

      RETURN
C
      END
