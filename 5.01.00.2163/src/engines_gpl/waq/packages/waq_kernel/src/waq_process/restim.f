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

      subroutine restim ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Residence time per volume, for advective transport only

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    Water Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : T1519 Clyde river estuary
C     Author  : M. Bokhorst
C     Date    : 13-1-95            Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     26-7-96 P. Boderie      Workarray as in- and output
C     13-1-95 M. Bokhorst     Create first version
C
C***********************************************************************
C
C     Description of the module :
C
C
C Name    T   L I/O   Description                                  Units
C ----    --- -  -    -------------------                          -----

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)

c.....Zero the workspace
      DO 6000 ISEG=1,NOSEG

         PMSA(IP2) = 0.0

         IP2 = IP2 + IN2

 6000 CONTINUE

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)

c.....Exchange loop
      DO 7000 IQ=1,NOQ1+NOQ2+NOQ3

c........Bepaal het van- en naar- segment
         IFROM = IEXPNT(1,IQ)
         ITO   = IEXPNT(2,IQ)

         FLOW = PMSA(IP3)

c........Absolute flows per segment sommeren in de workspace
         IF (IFROM .GT. 0) THEN
            PMSA ( IP2 + (IFROM-1) * IN2 ) =
     +      PMSA ( IP2 + (IFROM-1) * IN2 ) + ABS(FLOW)
         ENDIF
         IF (ITO  .GT. 0)  THEN
            PMSA ( IP2 + (ITO  -1) * IN2 ) =
     +      PMSA ( IP2 + (ITO  -1) * IN2 ) + ABS(FLOW)
         ENDIF

c........Ophogen van de exchange-pointers
         IP3 = IP3 + IN3

 7000 CONTINUE

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)

c.....Segmentloop
      DO 8000 ISEG=1,NOSEG

c........Niet-actieve segmenten afhandelen
         CALL DHKMRK(1,IKNMRK(ISEG),IKMRK)
         IF ( IKMRK .EQ. 0 ) THEN
            PMSA(IP4) = -999.999
            GOTO 100
         ENDIF

         VOLUME = PMSA(IP1)
         SOMFLW = PMSA(IP2)

c........Oneindige verblijftijden afhandelen
         IF ( SOMFLW .LT. 1.0E-20 ) THEN
            PMSA(IP4) = 1.0E7
            GOTO 100
         ENDIF

c........Bereken de verblijftijd
         RTIME = VOLUME / (SOMFLW/2)

c........Toekennen aan de PMSA
         PMSA(IP4) = RTIME

  100    CONTINUE

c........Ophogen van de segment-pointers
         IP1 = IP1 + IN1
         IP2 = IP2 + IN2
         IP4 = IP4 + IN4
         IP5 = IP5 + IN5

 8000 CONTINUE

      RETURN
      END
