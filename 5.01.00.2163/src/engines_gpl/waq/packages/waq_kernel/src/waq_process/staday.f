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

      subroutine staday ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Periodic (day) average of a given substance

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    Water Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : Delft3D-WAQ
C     Author  : Arjen Markus
C     Date    : 8-1-2002           Version : 0.1
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C      8-1-02 Arjen Markus    Create first version
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                  Units
C ----    --- -  -    -------------------                          -----
C
C CONC           I    Concentration of the substance            1
C TINIT         I/O   Initial time (reset at end period)        2
C PERIOD         I    Period of te periodic average             3
C TIME           I    Time in calculation                       4
C DELT           I    Timestep                                  5
C
C TCOUNT         O    Count of times (must be imported!)        6
C AVCUM          O    Work array for summing over time          7
C AVPERD         O    Periodic average (calcuated at the end)   8
C
C Note: to prevent strange results, the actual output parameter is
C       AVPERD. This is updated once in a while!
C

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
      INTEGER  IP1   , IP2   , IP3   , IP4   , IP5   ,
     +         IP6   , IP7   , IP8   ,
     +         IN1   , IN2   , IN3   , IN4   , IN5   ,
     +         IN6   , IN7   , IN8
      INTEGER  IKMRK , IKMRK1, IKMRK2, ISEG  , IQ    , IFROM , ITO
      INTEGER  ITYPE
      REAL     TINIT , PERIOD, TIME  , DELT  , TCOUNT

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)
      IN8 = INCREM(8)

      TINIT  = PMSA(IP2)
      PERIOD = PMSA(IP3)
      TIME   = PMSA(IP4)
      DELT   = PMSA(IP5)
      TCOUNT = PMSA(IP6)

C
C      Start and stop criteria are somewhat involved:
C      - The first time for the first period is special, as this
C        is the only time there is no previous period.
C      - If there is a previous period, update the averages
C        for that period and reset the accumulative values
C        for the next
C
C      To formulate the ideas more clearly:
C      - The first period is a closed interval
C      - All other periods are half-open intervals (the last time
C        of the previous period should not be reused.)
C
      ITYPE  = 0
      IF ( TIME .GE. TINIT-0.001*DELT ) THEN
         ITYPE = 2
         IF ( TCOUNT .EQ. 0.0 ) ITYPE = 1
      ENDIF
      IF ( TIME .GE. (TINIT+PERIOD)-0.999*DELT ) THEN
         ITYPE  = 3
      ENDIF

      IF ( ITYPE  .EQ. 0 ) RETURN

      TCOUNT    = TCOUNT + DELT
      PMSA(IP6) = TCOUNT

      DO 9000 ISEG=1,NOSEG
         IF (BTEST(IKNMRK(ISEG),0)) THEN

C
C        The first time is special. Initialise the arrays.
C        The last time requires additional processing.
C
         IF ( ITYPE .EQ. 1 ) THEN
            PMSA(IP7)  = 0.0
         ENDIF
         PMSA(IP7)  = PMSA(IP7) + PMSA(IP1) * DELT

         IF ( ITYPE .EQ. 3 ) THEN
            IF ( TCOUNT .GT. 0.0 ) PMSA(IP8)  = PMSA(IP7) / TCOUNT
            PMSA(IP7) = 0.0
         ENDIF

         ENDIF

         IP1  = IP1  + IN1
         IP7  = IP7  + IN7
         IP8  = IP8  + IN8

 9000 CONTINUE

C
C     Be sure to reset the initial time, so that we can reset the
C     averaging
C
      IF ( ITYPE .EQ. 3 ) THEN
         PMSA(IP2) = TINIT  + PERIOD
         PMSA(IP6) = 0.0
      ENDIF

      RETURN
      END
