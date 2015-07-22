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
C  *      SUBROUTINE CHHELP TO PRINT CURRENT PARAMETER SETTINGS        *
C  *********************************************************************
C
      SUBROUTINE CHHELP (OUTUNI)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'cal1.inc'
      INTEGER OUTUNI
C
C  Clear the screen and write a formfeed character to OUTUNI.
C
C  Print everything which can be altered in subroutine CHANGE.
C
C
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      CALL FORMFE (OUTUNI)
      WRITE (OUTUNI,99999) IYEAR,(CASE(I),I=1,9)
      WRITE (OUTUNI,99990) COM
      WRITE (OUTUNI,99980) TEMPMU,TEMPAD
      WRITE (OUTUNI,99970) SOLAMU,SOLAAD
      WRITE (OUTUNI,99965) DLGTMU,DLGTAD
      WRITE (OUTUNI,99960) BACKMU,BACKAD
      WRITE (OUTUNI,99950) DEPTMU,DEPTAD
      DO 10 I = 1,NUNUCO
   10 WRITE (OUTUNI,99940) CSTRA(I),BNUT(I),DNUT(I)
      DO 30 I = 1,NUNUCO
      IF (RNUT(2,I) .LT. 0.5) THEN
         WRITE (OUTUNI,99920) CSTRA(I),RNUT(1,I)
      ELSE
         WRITE (OUTUNI,99930) CSTRA(I),RNUT(1,I)
      END IF
   30 CONTINUE
      NNRUN = NRUN
      IF (NNRUN .EQ. 0) NNRUN = 1
      WRITE (OUTUNI,99910) NNRUN
      DO 40 J=1,NRUN
   40 WRITE (OUTUNI,99900) NPER(J,1),NPER(J,2),NPER(J,3)
C
      IF (LCAL .EQ. 4) THEN
         WRITE (OUTUNI,99870)
      ELSE
         WRITE (OUTUNI,99890) FLUSH
      END IF
      IF (IOFLAG .EQ. 1) CALL MORESC
      WRITE (OUTUNI,99850) SEDRAT
      WRITE (OUTUNI,99840) FLUSH
      WRITE (OUTUNI,99830) REMIOR
      WRITE (OUTUNI,99820) REMILI(1),REMILI(2)
      WRITE (OUTUNI,99810) AVAILN
      IF (IOFLAG .EQ. 1) CALL MORESC
C
C  Formats for this subroutine
C
99999 FORMAT (1X,I4,1X,9A8)
99990 FORMAT (9A8)
99980 FORMAT (1X,'Temperature calculated as    ',2X,F6.2,2X,
     1        ' * nominal Temp   + ',2X,F10.2)
99970 FORMAT (1X,'Solar radiation calculated as',2X,F6.2,2X,
     1        ' * nominal rad    + ',2X,F10.2)
99965 FORMAT (1X,'Daylength calculated as      ',2X,F6.2,2X,
     1        ' * nominal daylen + ',2X,F10.2)
99960 FORMAT (1X,'Backgr. ext. calculated as   ',2X,F6.2,2X,
     1        ' * nominal Kb     + ',2X,F10.2)
99950 FORMAT (1X,'Depth calculated as          ',2X,F6.2,2X,
     1        ' * nominal depth  + ',2X,F10.2)
99940 FORMAT (1X,'Concentration of',2X,A8,2X,' is',
     1        2X,F6.2,2X,' * nominal conc. + ',2X,F6.2)
99930 FORMAT (1X,'Mineralization of',2X,A8,2X,' = ',2X,F6.4,2X,
     1       ' * Temparature')
99920 FORMAT (1X,'Mineralization of',2X,A8,2X,' = ',2X,F6.4)
99910 FORMAT (1X,'Blooms will be calculated for the following',
     1        2X,I2,2X,'periods:')
99900 FORMAT (1X,'First period: ',I5,' Last period: ',I5,
     1        ' Increment: ',I5)
99890 FORMAT (1X,'Mortality is input + ',F6.4)
99870 FORMAT (1X,'Mortality rate calculated as exponential function ',
     1        'of temperature.')
99850 FORMAT (1X,'Sedimentation rate =',F6.3)
99840 FORMAT (1X,'Flushing rate =',F6.3)
99830 FORMAT (1X,'Mineralization rate of detritus =',2X,F6.3,2X,
     1        '*  Temperature')
99820 FORMAT (1X,'Mineralization rate of chlorophyll is:',1X,
     1        'EXP (',2X,F6.4,' * Temperature- ',2X,F6.4,' )')
99810 FORMAT (1X,'Nutrient fraction becoming detritus =',F6.3)
      RETURN
      END
