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

      INTEGER FUNCTION DHCARG ( )
C
C     Delwaq Hulp Count ARGuments
C
C     Arguments
C
C
C     Local
C
      USE DHCOMMAND

      INTEGER           NARGS
      LOGICAL           EXISTS, OPENED
      INTEGER           I     , LUN   , IERR
      CHARACTER(LEN=20) LINE
C
C     Any stored arguments?
C
      DHCARG = DHSTORED_NUMBER_ARGS()

      IF ( DHCARG .EQ. 0 ) THEN
C
C         Call system routine
C
          DHCARG = NARGS()
C
C         Take care of virtual zeroth argument
C
          IF ( DHCARG .EQ. 0 ) THEN
              DHCARG = 1
          ENDIF

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
                  OPEN( LUN, FILE = 'delwaq.options' )
                  DO
                      READ( LUN, '(A)', IOSTAT = IERR ) LINE
                      IF ( IERR .NE. 0 ) THEN
                          EXIT
                      ENDIF
                      IF ( LINE .NE. ' ' ) THEN
                          DHCARG = DHCARG + 1
                      ENDIF
                  ENDDO
                  CLOSE( LUN )
              ENDIF
          ENDIF
      ENDIF
C
      RETURN
      END
