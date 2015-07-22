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

      SUBROUTINE DHGARG ( IARG  , LINE  )
C
C     Returns the IARG'th argument from the command line
C
      USE DHCOMMAND

      IMPLICIT NONE
C
C     Arguments
C
      INTEGER       IARG
      CHARACTER*(*) LINE
C
C     Local
C
      INTEGER*2     N, STATUS
      CHARACTER*256 BUFFER
      INTEGER       COUNT
      INTEGER       NARGS
      INTEGER       STORED
      LOGICAL       OPENED, EXISTS
      INTEGER       IERR
      INTEGER       LUN
      INTEGER       I

C
C     Any stored arguments?
C
      STORED = DHSTORED_NUMBER_ARGS()
      IF ( STORED > 0 ) THEN
          LINE = DHSTORED_ARG(IARG)
      ELSE
C
C         Call system routine
C
          N = IARG
          CALL GETARG( N     , BUFFER, STATUS)
C
C         Store result
C
          IF ( STATUS .GT. 0 ) THEN
             LINE = BUFFER
          ELSE
             LINE = ' '

             !
             ! Read from file?
             !
             INQUIRE( FILE = 'delwaq.options', EXIST = EXISTS )
             IF ( EXISTS ) THEN
                 LUN = -1
                 DO I = 1,100
                     INQUIRE( UNIT = I, OPENED = OPENED )
                     IF ( .NOT. OPENED ) THEN
                         LUN = I
                         EXIT
                     ENDIF
                 ENDDO
                 IF ( LUN .GT. 0 ) THEN
                     IF ( NARGS() .GT. 0 ) THEN
                         COUNT = IARG - NARGS()
                     ELSE
                         COUNT = IARG - 1
                     ENDIF

                     OPEN( LUN, FILE = 'delwaq.options' )
                     DO
                         READ( LUN, '(A)', IOSTAT = IERR ) LINE
                         IF ( IERR .NE. 0 ) THEN
                             EXIT
                         ENDIF
                         IF ( LINE .NE. ' ' ) THEN
                             IF ( COUNT .EQ. 0 ) THEN
                                 EXIT
                             ENDIF
                             COUNT = COUNT - 1
                         ENDIF
                     ENDDO
                     CLOSE( LUN )
                 ENDIF
             ENDIF
          ENDIF
      ENDIF
C
      RETURN
      END
