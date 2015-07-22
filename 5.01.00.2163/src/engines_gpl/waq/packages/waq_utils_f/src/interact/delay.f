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
C  Subroutine DELAY (NSEC)
C  This program pauses execution for NSEC seconds, unless the user hits
C  any key, in which case execution continues immediately.
C
C  Program written by Hans Los
C  Version 1.0
C  Released july 1989.
C  Part of the INTERACT.LIB.
C
C
      SUBROUTINE DELAY (NSEC)
      INTEGER*2 HOUR, MINUTE, SEC, SEC100
      INTEGER*4 SECMID, SECEND, NSEC, KEY
C
      CALL GETTIM (HOUR, MINUTE, SEC, SEC100)
      SECMID = HOUR * 3600 + MINUTE * 60 + SEC
      SECEND = SECMID + NSEC
10    CONTINUE
      CALL CHKKEY (KEY)
      IF (KEY .NE. 0) GO TO 20
      CALL GETTIM (HOUR, MINUTE, SEC, SEC100)
      SECMID = HOUR * 3600 + MINUTE * 60 + SEC
      IF (SECMID .LT. SECEND) GO TO 10
20    CONTINUE
      RETURN
      END
