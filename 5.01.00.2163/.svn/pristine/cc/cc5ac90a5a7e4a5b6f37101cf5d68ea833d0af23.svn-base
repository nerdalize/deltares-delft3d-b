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

      SUBROUTINE SRSTOP ( IEXIT )
C
C
C     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
C
C     CREATED            : June  '91  BY  Jan van Beek
C
C     FUNCTION           : stops execution if possible with return value
C
C*********************************************************************
C     System dependent routine
C     configuration
C     PC - MS DOS operating system - MSF 4.1 compiler
C     PC - OS2    operating system - MSF 4.1 compiler
C     PC - NT     operating system - Powerstation compiler
C*********************************************************************
C
C     LOGICAL UNITS      : -
C
C     PARAMETERS    :
C
C     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
C     ---------------------------------------------------------
C     IEXIT   INTEGER    1         INPUT   return value
C     ---------------------------------------------------------
C
      INTEGER   IEXIT
C
C     PC VERSION, for MicroSoft Fortran 4.01
C
      IF ( IEXIT .NE. 0 ) THEN
         WRITE (*,*) 'Stopped with error code :',IEXIT
      ELSE
         WRITE (*,*) 'Normal end'
      ENDIF
      OPEN  ( 13 , FILE = 'delwaq.rtn' )
      WRITE ( 13 , * ) IEXIT
      CLOSE ( 13 )
      IF ( IEXIT .LE. 0 ) THEN
         STOP ' '
      ELSEIF ( IEXIT .EQ. 1 ) THEN
         STOP 1
      ELSEIF ( IEXIT .EQ. 2 ) THEN
         STOP 2
      ELSEIF ( IEXIT .EQ. 3 ) THEN
         STOP 3
      ELSEIF ( IEXIT .EQ. 4 ) THEN
         STOP 4
      ELSEIF ( IEXIT .EQ. 5 ) THEN
         STOP 5
      ELSEIF ( IEXIT .EQ. 6 ) THEN
         STOP 6
      ELSEIF ( IEXIT .EQ. 7 ) THEN
         STOP 7
      ELSEIF ( IEXIT .EQ. 8 ) THEN
         STOP 8
      ELSE
         STOP 255
      ENDIF
C
      END
