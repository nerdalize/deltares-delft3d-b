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
C  *    SUBROUTINE CZOOPL TO MODIFY ZOOPLANKTON CHARACTERISTICS        *
C  *********************************************************************
C
C    0895 MvdV  dimension for more than one grazer typoe added to ZOONUT
      SUBROUTINE CZOOPL(LERR)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'ioblck.inc'
      PARAMETER (NZOO = 9)
      CHARACTER*8 WZOO(NZOO),WORD,WORD2
      CHARACTER*3 NEW,OLD
      DATA NEW    /'New'/
      DATA Old    /'Old'/
      DATA WZOO  /'?       ','END     ','ESCAPE  ','GRAZINIT',
     1            'GRAZRATE','HALFSAT ','MAXITER ','NUTCOEFF',
     2            'PRINT   '/
C
C  General comment: in case an error is detected in this subroutine
C  -- for instance a missing or misspelled control word --
C  LERR will be put to 1 and subroutine CHANGE will set LRUN = 0.
C  Hence a batch job will be terminated,
C  but re-entry is possible in an interactive run.
C
      LERR=0
   10 IF (IOFLAG .EQ. 1) CALL BLSELECT (WZOO, NZOO, 1041)
      I=INPTDT(1041,WORD,LENWRD)
      IF (MATCH(WZOO,NZOO,8,WORD,LENWRD,0,NUMCA) .NE. 1) NUMCA=NZOO+1
      GO TO (   900,  1000,   300,   500,   400,
     1          200,   600,   100,   700,    20), NUMCA
C
C  Error detected in input of change zooplankton characteristic
C
   20 WRITE (OUUNI,99999) WORD
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 10
   30 WRITE (OUUNI,99990)
      LERR=1
      RETURN
C
C  Nutrient coefficients.
C
  100 CONTINUE
      IF (IOFLAG .EQ. 1) CALL BLSELECT (CSTRA, NUNUCO, 1042)
      I = INPTDT(1042,WORD2,LENWRD)
      IF (MATCH(CSTRA,NUNUCO,8,WORD2,LENWRD,0,NUMCO).EQ.1) GOTO 110
      WRITE (OUUNI,99980) WORD2
      WRITE (OUUNI,99970) (CSTRA(I),I=1,NUNUCO)
      POSIT=0
      IF (IOFLAG .EQ. 0) GO TO 30
      GO TO 100
  110 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99960) OLD,CSTRA(NUMCO),
     1                   ZOONUT(NUMCO,0)
      I=INPTNM(1043,ZOONUT(NUMCO,0),0,1)
      WRITE (OUUNI,99960) NEW,CSTRA(NUMCO),ZOONUT(NUMCO,0)
      GO TO 10
C
C  Modify half-saturation constant.
C
  200 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),ZOOK
      I=INPTNM(1044,ZOOK,0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),ZOOK
      GO TO 10
C
C  Modify phytoplankton fraction escaping grazing.
C
  300 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),XMIN
      I=INPTNM(1045,XMIN,0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),XMIN
      GO TO 10
C
C  Modify grazing rate constant.
C
  400 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),ZOOGR
      I=INPTNM(1046,ZOOGR,0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),ZOOGR
      GO TO 10
C
C  Modify initial grazing rate.
C
  500 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99950) OLD,WZOO(NUMCA),GRAMO1
      I=INPTNM(1047,GRAMO1,0,1)
      WRITE (OUUNI,99950) NEW,WZOO(NUMCA),GRAMO1
      GO TO 10
C
C  Modify maximum permissible iteration number.
C
  600 CONTINUE
      IF (IOFLAG .EQ. 1) WRITE (OUUNI,99940) OLD,WZOO(NUMCA),IPERM
      I=INPTNM(1048,0.0D0,IPERM,2)
      WRITE (OUUNI,99940) NEW,WZOO(NUMCA),IPERM
      GO TO 10
C
C  Print present set of zooplankton characteristics.
C
  700 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE (OUUNI,99930)
      DO 710 I=1,NUNUCO
  710 WRITE (OUUNI,99920) CSTRA(I),ZOONUT(I,0)
      WRITE (OUUNI,99910) ZOOK
      WRITE (OUUNI,99900) XMIN
      WRITE (OUUNI,99890) ZOOGR
      WRITE (OUUNI,99880) GRAMO1
      WRITE (OUUNI,99870) IPERM
      GO TO 10
C
C  Print characteristics that may be modified
C
  900 CONTINUE
      IF (IOFLAG .EQ. 1) CALL CLRSCR
      WRITE (OUUNI,99850)
      CALL VIDEO (1)
      WRITE (OUUNI,99845) (WZOO(I),I=3,NZOO)
      CALL VIDEO (0)
      WRITE (OUUNI,99840)
      GO TO 10
C
C  Exit
C
 1000 CONTINUE
C
C  Formats for this subroutine
C
99999 FORMAT (1X,'Invalid zooplankton command',2X,A8)
99990 FORMAT (//,1X,'Excecution terminates following an error in',
     1       ' subroutine "CZOOPL"',//)
99980 FORMAT (1X,'Incorrect nutrient name ',A8)
99970 FORMAT (1X,'Valid nutrients are: ',6A9)
99960 FORMAT (1X,A3,' nutrient ',A8,' coefficient = ',F14.4)
99950 FORMAT (1X,A3,' value of ',A8,' = ',F14.4)
99940 FORMAT (1X,A3,' value of ',A8,' = ',I2)
99930 FORMAT (1X,'Present zooplankton characteristics: ')
99920 FORMAT (' Zooplankton nutrient coefficient for ',A8,2X,F6.4)
99910 FORMAT (' Half saturation constant ZOOK = ',F6.1)
99900 FORMAT (' Phytoplankton escaping grazing XMIN = ',F6.1)
99890 FORMAT (' Grazing rate constant ZOOGR = ',F8.4,' per day')
99880 FORMAT (' Grazing rate first iteration step = ',F8.4)
99870 FORMAT (' Maximum number of iterations = ',I2)
99850 FORMAT (1X,'You are in zooplankton command mode.',/,' Select on',
     1        ' of the following characteristics:')
99845 FORMAT (9(1X,A8))
99840 FORMAT (1X,'To exit to parameter command mode enter "END".',/,
     1        1X,'Use "PRINT" command to show present dataset.',/)
      RETURN
      END
