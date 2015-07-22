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

      SUBROUTINE STEPYN (ITIME , IDT   , ISTRT , ISTOP , ISTEP ,
     +                   LFLAG , LFIRST)
C
C     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED:            : march 1993 by Jan van Beek
C
C     FUNCTION            : Evaluates if action is necessary
C                           according to timers
C
C     SUBROUTINES CALLED  : -
C
C     FILES               : -
C
C     COMMON BLOCKS       : -
C
C     PARAMETERS          : 6
C
C     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
C     ----    -----    ------     ------- -----------
C     ITIME   INTEGER       1     INPUT   Time in system clock units
C     IDT     INTEGER       1     INPUT   Simulation timestep
C     IMSTRT  INTEGER       1     INPUT   start time of timer
C     IMSTOP  INTEGER       1     INPUT   stop time of timer
C     IMSTEP  INTEGER       1     INPUT   time step of timer
C     LFLAG   LOGICAL       1     OUTPUT  If .T. then action else not
C     LFIRST  LOGICAL       1     OUTPUT  If .T. then first step
C
C     Declaration of arguments
C
      use timers

      INTEGER       ITIME , IDT   , ISTRT , ISTOP , ISTEP
      LOGICAL       LFLAG , LFIRST
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "stepyn", ithandl )
C
C     Evaluate timer
C
      LFLAG  = .TRUE.
      LFIRST = .FALSE.
      IF ( ISTEP                  .LE. 0         ) THEN
         LFLAG = .FALSE.
         GOTO 100
      ENDIF
      IF ( ISTRT                  .GT. ITIME     ) THEN
         LFLAG = .FALSE.
         GOTO 100
      ENDIF
      IF ( ISTOP                  .LE. ITIME-IDT ) THEN
         LFLAG = .FALSE.
         GOTO 100
      ENDIF
      IF ( MOD(ITIME-ISTRT,ISTEP) .GE. IDT       ) LFLAG = .FALSE.
      IF ( LFLAG ) THEN
         IF ( ITIME-ISTRT .LT. ISTEP ) LFIRST = .TRUE.
      ENDIF
C
  100 CONTINUE
C
      if ( timon ) call timstop ( ithandl )
      RETURN
C
      END
