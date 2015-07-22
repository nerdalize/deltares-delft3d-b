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

C
C  *********************************************************************
C  *     SUBROUTINE TO PRINT LATEST NEWS ON PROGRAM                    *
C  *********************************************************************
C
      SUBROUTINE NEWS
      CHARACTER*24 CMD
      INTEGER CMS
      INCLUDE 'ioblck.inc'
C
C Read and print the news file.
C
      CMD = 'BROWSE \BLOOM\BLMNEWS'
      IRC = CMS (CMD, 24)
      IF (IRC .NE. 0) THEN
         CALL ALARM
         CALL VIDEO (1)
         CALL VIDEO (5)
         WRITE (OUUNI,10)
   10    FORMAT (' News file "BLMNEWS" does not exist.',/,
     *           ' No news available.')
         CALL VIDEO (0)
      END IF
      RETURN
      END
