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

C    Date:       30 December 1993
C    Program:    BLFILE.FOR
C    Version:    0.1
C    Programmer: Jos van Gils
C
C    Module that opens files for autonomous I/O of BLOOM
C
C    Called by: BLOOMC
C    Calls    : SETUNI

      SUBROUTINE BLFILE (RUNNAM)

C     Arguments
C
C     Name    Type  Length   I/O  Description
C
C     RUNNAM  C*(*) 1        I    Filename consisting of runid (no ext)
C
      CHARACTER*(*)   RUNNAM
C
C     Common block variables used
C
C     Name    Type  Length   I/O  Inc-file  Description
C
C     IOU     I     99       I    ioblck    Array of logical unit numbers
C
      INCLUDE 'ioblck.inc'
C
C     Local variables
C
C     Name    Type  Length   I/O  Description
C
C     IOST    I     1             I/O-status

      INTEGER      IOST

C
C  Call subroutine SETUNI to set I/O unit numbers for BLOOM II.
C
      CALL SETUNI
C
C  Open statement for BLOOM II input files.
C
      WRITE (RUNNAM(10:12),'(''frm'')')
      OPEN (IOU(12),FILE=RUNNAM,SHARED,IOSTAT = IOST)
      IF (IOST .NE. 0) GOTO 901

      WRITE (RUNNAM(10:12),'(''d09'')')
      OPEN (IOU( 9),FILE=RUNNAM,SHARED,IOSTAT = IOST)
      IF (IOST .NE. 0) GOTO 902
C
C Open BLOOM output file as unformatted, binary = transparent.
C
      WRITE (RUNNAM(10:12),'(''blm'')')
      OPEN (IOU(26),FILE=RUNNAM,SHARED,
     &         FORM='UNFORMATTED', IOSTAT=IOST)
c     ENDFILE (IOU(26))
      IF (IOST .NE. 0) GOTO 903
C
C  Open statement for BLOOM II debug file.
C
      WRITE (RUNNAM(10:12),'(''dbg'')')
      OPEN (IOU(10),FILE=RUNNAM,SHARED,IOSTAT = IOST)
      IF (IOST .NE. 0) GOTO 904

      RETURN

C     $Neatly process these error messages

  901 STOP 'BLFILE: Error opening .frm file'
  902 STOP 'BLFILE: Error opening .d09 file'
  903 STOP 'BLFILE: Error opening .blm file'
  904 STOP 'BLFILE: Error opening .dbg file'

      END
