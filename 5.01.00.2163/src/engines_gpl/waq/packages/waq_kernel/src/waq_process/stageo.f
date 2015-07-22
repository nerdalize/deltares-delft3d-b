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

      subroutine stageo ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Geometric mean of a variable during a certian time span

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
C      9-1-02 Arjen Markus    Create first version
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C
C        Calculates the geometric mean of a series of values. To take
C        care of the problem of very small values (that would distort
C        the calculated mean), a threshold is set.
C
C
C Name    T   L I/O   Description                                  Units
C ----    --- -  -    -------------------                          -----
C
C CONC           I    Concentration of the substance              1
C TSTART         I    Start of statistical period                 2
C TSTOP          I    Stop of statistical period                  3
C TIME           I    Time in calculation                         4
C DELT           I    Timestep                                    5
C THRESH         I    Threshold for considering the value         6
C
C TCOUNT         O    Count of timesteps                          7
C TCNTAB         O    Count of timesteps with values above        8
C GEOMN          O    Geometric mean, values above threshold      9
C GEOALL         O    Geometric mean, all values                 10
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
     +         IP6   , IP7   , IP8   , IP9   , IP10  ,
     +         IN1   , IN2   , IN3   , IN4   , IN5   ,
     +         IN6   , IN7   , IN8   , IN9   , IN10
      INTEGER  IKMRK , IKMRK1, IKMRK2, ISEG  , IQ    , IFROM , ITO
      INTEGER  LUNREP, ITYPE
      REAL     TSTART, TSTOP , TIME  , DELT
      REAL     THRESH, TCOUNT, TCNTAB, THRLOG, PMLOG

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)
      IP9 = IPOINT(9)
      IP10= IPOINT(10)

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)
      IN8 = INCREM(8)
      IN9 = INCREM(9)
      IN10= INCREM(10)

C
C     There are five cases, defined by the time:
C                        TIME <  TSTART-0.5*DELT : do nothing
C     TSTART-0.5*DELT <= TIME <  TSTART+0.5*DELT : initialise
C     TSTART          <  TIME <  TSTOP           : accumulate
C     TSTOP           <= TIME <  TSTOP+0.5*DELT  : finalise
C     TSTOP+0.5*DELT  <  TIME                    : do nothing
C
C     (Use a safe margin)
C
      TSTART = PMSA(IP2)
      TSTOP  = PMSA(IP3)
      TIME   = PMSA(IP4)
      DELT   = PMSA(IP5)
      THRESH = PMSA(IP6)

      IF ( THRESH .LE. 0.0 ) THEN
         CALL GETMLU( LUNREP )
         WRITE( LUNREP, * ) 'ERROR in STAGEO'
         WRITE( LUNREP, * )
     &'Threshold must be a positive value'
         WRITE( LUNREP, * )
     &'Threshold: ', THRESH
         WRITE( *     , * ) 'ERROR in STAGEO'
         WRITE( *     , * )
     &'Threshold must be a positive value'
         WRITE( LUNREP, * )
     &'Threshold: ', THRESH
         CALL SRSTOP( 1 )
      ENDIF

      THRLOG = LOG(THRESH) * DELT

      TCOUNT = PMSA(IP7)

C
C      Start and stop criteria are somewhat involved. Be careful
C      to avoid spurious calculations (initial and final) when
C      none is expected.
C      Notes:
C      - The initial value for TCOUNT must be 0.0
C      - Time is expected to be the model time (same time frame
C        as the start and stop times of course)
C      - Check that the NEXT timestep will not exceed the stop time,
C        otherwise this is the last one
C
      ITYPE  = 0
      IF ( TIME .GE. TSTART-0.001*DELT ) THEN
         ITYPE = 2
         IF ( TCOUNT .EQ. 0.0 ) ITYPE = 1
      ENDIF
      IF ( TIME .GE. TSTOP-0.999*DELT ) THEN
         ITYPE  = 3
         IF ( TCOUNT .LE. 0.0 ) ITYPE = 0
      ENDIF

      IF ( ITYPE  .EQ. 0 ) RETURN

      TCOUNT    = TCOUNT + DELT
      PMSA(IP7) = TCOUNT

      DO 9000 ISEG=1,NOSEG
         IF (BTEST(IKNMRK(ISEG),0)) THEN

C
C        The first time is special. Initialise the arrays.
C        The last time requires additional processing.
C
         IF ( ITYPE .EQ. 1 ) THEN
            PMSA(IP8) = 0.0
            PMSA(IP9) = 0.0
            PMSA(IP10)= 0.0
         ENDIF

         IF ( PMSA(IP1) .GE. THRESH ) THEN
            PMLOG      = LOG(PMSA(IP1)) * DELT
            PMSA(IP8)  = PMSA(IP8) + DELT
            PMSA(IP9)  = PMSA(IP9) + PMLOG
            PMSA(IP10) = PMSA(IP10)+ PMLOG
         ELSE
            PMSA(IP10) = PMSA(IP10)+ THRLOG
         ENDIF

         IF ( ITYPE .EQ. 3 ) THEN
            IF ( TCOUNT .GT. 0.0 ) THEN
               IF ( PMSA(IP8) .GT. 0.0 ) THEN
                  PMSA(IP9) = EXP( PMSA(IP9) / PMSA(IP8) )
               ELSE
                  PMSA(IP9) = 0.0
               ENDIF
               PMSA(IP10)= EXP( PMSA(IP10)/ TCOUNT )
            ELSE
               PMSA(IP9) = 0.0
               PMSA(IP10)= 0.0
            ENDIF
         ENDIF

         ENDIF

         IP1  = IP1  + IN1
         IP8  = IP8  + IN8
         IP9  = IP9  + IN9
         IP10 = IP10 + IN10

 9000 CONTINUE

C
C     Be sure to turn off the statistical procedure, once the end has been
C     reached (by setting TCOUNT (PMSA(IP7)) to a non-positive value)
C
      IF ( ITYPE .EQ. 3 ) THEN
         PMSA(IP7) = -TCOUNT
      ENDIF

      RETURN
      END
