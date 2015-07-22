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

      SUBROUTINE DHERRS(STRING,IERR)
C
      CHARACTER*(*) STRING

      INTEGER       LUNREP

C     message to screen

      WRITE(*,*) STRING

C     message to monitor or report file

      CALL GETMLU(LUNREP)
      IF ( LUNREP .GT. 0 ) THEN
         WRITE(LUNREP,*) STRING
      ENDIF
C
      CALL SRSTOP(1)
C
      RETURN
      END
      SUBROUTINE DHERR2 ( NAME  , VALUE , ISEG  , MODULE )
      CHARACTER*(*) NAME
      REAL          VALUE
      INTEGER       ISEG
      CHARACTER*(*) MODULE

      INTEGER       LUNREP

C     message to screen

      WRITE (*,*) ' Coefficient value out of range'
      WRITE (*,*) ' Coefficient name:',NAME
      WRITE (*,*) ' Coefficient value',VALUE
      WRITE (*,*) ' Coefficient value',MODULE
      IF ( ISEG .GT. 0 ) WRITE(*,*) ' in segment number:',ISEG

C     message to monitor or report file

      CALL GETMLU(LUNREP)
      IF ( LUNREP .GT. 0 ) THEN
         WRITE (LUNREP,*) ' Coefficient value out of range'
         WRITE (LUNREP,*) ' Coefficient name:',NAME
         WRITE (LUNREP,*) ' Coefficient value',VALUE
         IF ( ISEG .GT. 0 ) WRITE(LUNREP,*) ' in segment number:',ISEG
         WRITE (LUNREP,*) ' In subroutine ',MODULE
      ENDIF

      CALL SRSTOP(1)

      RETURN
      END
