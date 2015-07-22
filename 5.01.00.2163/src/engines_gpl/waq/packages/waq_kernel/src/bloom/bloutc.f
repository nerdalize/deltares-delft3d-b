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

C    Date:       7 Januari 1994
C    Program:    BLOUTC.FOR
C    Version:    0.1
C    Programmer: Jos van Gils
C
C    Set output controls for BLOOM
C
C    Called by: BLOOMC
C    Calls    : -

      SUBROUTINE BLOUTC (HISTOR,LPRINO,LDUMPO)
C
C     Arguments
C
C     Name    Type  Length   I/O  Description
C
C     HISTOR  L     1        I    Flag to activate output
C     LPRINO  I     1        I    Saves original value of LPRINT
C     LDUMPO  I     1        I    Saves original value of IDUMP

      LOGICAL         HISTOR
      INTEGER         LPRINO, LDUMPO

C     Common block variables used
C
C     Name    Type  Length   I/O  Inc-file  Description
C
C     LPRINT  I     1        I    sumout    Print flag
C     IDUMP   I     1        I    phyt2     Print flag

      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'sumout.inc'
C
C     Local variables
C
C     Name    Type  Length   I/O  Description
C
C     -
C
C     LPRINT and IDUMP are output control flags of Bloom
C     They have been read from the input and their value has been
C     saved in LPRINO and LDUMPU
C     Here we set them to their original values, ONLY if HISTOR is
C     true, that is for history elements at history times.
C     This is to avoid excessively sized output files of Bloom

      LPRINT = 0
      IDUMP  = 0
      IF (HISTOR) THEN
          LPRINT = LPRINO
          IDUMP  = LDUMPO
      ENDIF

      RETURN
      END

