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

      subroutine calwav ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Wave characteristics

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
C     Date    : 921223             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     9202    Pascal Boderie  Create xxx version for T890 SLIB
C     9207    Jos van Gils    Create xxx version for Djakarta Bay
C     920701  Pascal Boderie  Create first version vor T721.72
C     970701  Johan Boon      Create first version for Promise (z2025)
C     980421  Jos van Gils    Change for application in 3D too!
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C        CALCULATE WAVE CHARACTERISTICS FROM EMIPRICAL FORMULAES
C        MODEL IS VERTICALLY AVERAGED.
C
C        AVERAGED MODELS
C
C Name    T   L I/O  Description                              Units
C ----    --- -  -   -------------------                      ----
C DEPTH   R   1  I   Water depth                                       [m]
C DS      R   1  L   Standaard deptch                                  [-]
C FETCH   R   1  I   Fetch                                             [m]
C FS      R   1  L   Standaard fetch                                   [-]
C G       R   1  I   Acceleration of gravity                        [m/s2]
C RHOW    R   1  I   Density of water                              [kg/m3]
C VWIND   R   1  I   Wind velocity at 10 m                           [m/s]

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     Local declarations, constants in source
C
      PARAMETER ( G      =     9.8    ,
     +            RHOW   =  1000.0    ,
     +            PI     = 3.14159265 )
      REAL        INIDEP
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
c     IF (IKMRK2.EQ.0) THEN
      IF (IKMRK2.EQ.0 .OR. IKMRK2.EQ.3) THEN
C
      VWIND   = PMSA(IP1 )
      FETCH   = PMSA(IP2 )
      DEPTH   = PMSA(IP3 )
      INIDEP  = PMSA(IP4 )

      IF (FETCH .LT. 1E-20 )  CALL ERRSYS ('FETCH in CALWAVE zero', 1)
C     IF (VWIND .LT. 1E-20 )  CALL ERRSYS ('VWIND in CALTAU zero', 1 )

C     Initialisation
      H       = 0.0
      RL      = 0.0
      T       = 0.0

C     Check if user wants to use value of inidepth (pos values only)
      IF (INIDEP .LT. 0.0) INIDEP = DEPTH

C     Shear stress by wind
      IF (VWIND .LT. 0.0001) GOTO 150

C     dimensieloze strijklengte
      FS   = G * FETCH / VWIND**2
c     write(*,*) 'FS:', FS

C     dimensieloze diepte
      DS   = G * INIDEP / VWIND**2
c     write(*,*) 'DS:', DS

C     bepaal golfhoogte H
      A1   = 0.710 *( DS**0.763 )
      A2   = 0.855 *( DS**0.365 )
      A3   = 0.0150*( FS**0.450 ) / TANH(A1)
      A4   = 0.0345*( FS**0.370 ) / TANH(A2)
      HS   = 0.240 * TANH( A1) * TANH( A3)
c     write(*,*) 'HS:', HS
      TS   = 2.0*PI* TANH( A2) * TANH( A4)
c     write(*,*) 'TS:', TS
      H    = HS * ( VWIND * VWIND) / G
c     write(*,*) 'H:', H

C     bepaal golfperiode T
      T    = TS * VWIND /G
c     write(*,*) 'T:', T

C     bepaling golflengte (iteratief hier niet opgenomen)
      RL0  = G * T * T / ( 2.0 * PI)
c     write(*,*) 'L0:', RL0
      A5   = 2.0 * PI * INIDEP / RL0

C     afscherming voor te groot argument functie tanh
c     voor diepe systemen tov golflengte
c     A5 groot, tanh=1, rl=rl0, a5=a6,sinh(>9)->0,ubg->fwg->0
      if (A5 .gt. 9.) then
         RL  = RL0
      else
C     let op sommige documentatie stelt: RL = RL0*SQRT (TANH (A5))
         RL  = RL0 * TANH(A5)
      endif
c     write(*,*) 'L:', RL

  150 CONTINUE
c
      PMSA (IP5) = H
      PMSA (IP6) = RL
      PMSA (IP7) = T
C
      ENDIF
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
C
 9000 CONTINUE
C

      RETURN
      END
