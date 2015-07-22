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

      subroutine densed ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Denitrification in sediment

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
C     921210  Pascal Boderie  Create first version, based on T721.13
C                             created by Jos van Gils
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C
C Name    T   L I/O   Description                                    Units
C ----    --- -  -    -------------------                            ----
C CRTEMP  R*4 1 I critical temperature for both processes             [xC]
C DEPTH   R*4 1 I depth                                                [m]
C DENR    R*4 1 I zeroth order denitrification rate              [gN/m2/d]
C DENRC   R*4 1 I firstt order denitrification rate                  [m/d]
C DENTC   R*4 1 I temperature coefficient for denitrif.                [-]
C FL (1)  R*4 1 O denitrification flux                           [gN/m3/d]
C NO3     R*4 1 I nitrate concentration                            [gN/m3]
C TEMP    R*4 1 I ambient temperature                                 [xC]
C TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [xC]
C TEMPC   R*4 1 L temperatuur coefficient                              [-]

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      LOGICAL  TMPOPT
C
      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)
      IN4  = INCREM( 4)
      IN5  = INCREM( 5)
      IN6  = INCREM( 6)
      IN7  = INCREM( 7)
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
C
      IF ( IN1 .EQ. 0 .AND. IN3 .EQ. 0 .AND. IN4 .EQ. 0 .AND.
     *     IN5 .EQ. 0 .AND. IN6 .EQ. 0                        ) THEN
         DENR   = PMSA(IP1)
         TEMP   = PMSA(IP5)
         CRTEMP = PMSA(IP6)
         IF ( TEMP .LE. CRTEMP ) THEN
            TEMFAK = 0.0
         ELSE
            DENRC  = PMSA(IP3)
            DENTC  = PMSA(IP4)
            TEMP20 = TEMP - 20.0
            TEMFAK = DENRC * DENTC ** TEMP20
         ENDIF
         TMPOPT = .FALSE.
      ELSE
         TMPOPT = .TRUE.
      ENDIF
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL DHKMRK(2,IKNMRK(ISEG),IKMRK2)
      IF ((IKMRK2.EQ.0).OR.(IKMRK2.EQ.3)) THEN
C
      IF ( TMPOPT ) THEN
         DENR   = PMSA(IP1)
         TEMP   = PMSA(IP5)
         CRTEMP = PMSA(IP6)
         IF ( TEMP .LE. CRTEMP ) THEN
            TEMFAK = 0.0
         ELSE
            DENRC  = PMSA(IP3)
            DENTC  = PMSA(IP4)
            TEMP20 = TEMP - 20.0
            TEMFAK = DENRC * DENTC ** TEMP20
         ENDIF
      ENDIF
C
      NO3    = MAX ( 0.0, PMSA(IP2 ) )
      DEPTH  = PMSA(IP7)

C***********************************************************************
C**** Processes connected to the DENITRIFICATION
C***********************************************************************
C
C     Denitrification is assumed to take place in the sediment
C     Calculation of denitrification flux ( M.L-3.t-1)

      FL( 1 + IFLUX ) = ( DENR +  TEMFAK * NO3 ) / DEPTH
C
      ENDIF
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
         IP1 = IP1 + IN1
      IP2   = IP2   + IN2
         IP3 = IP3 + IN3
         IP4 = IP4 + IN4
         IP5 = IP5 + IN5
         IP6 = IP6 + IN6
      IP7   = IP7   + IN7
c
 9000 CONTINUE
c
      RETURN
C
      END
