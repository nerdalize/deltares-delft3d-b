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
C  *    SUBROUTINE TO CALCULATE DIURNAL PRODUCTION AND RESPIRATION     *
C  *                        RATES OF THE BLOOM                         *
C  *********************************************************************
C
      SUBROUTINE DIEL(SUMPOX,SUMROX,DAY,CDATE)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'ioblck.inc'
      DIMENSION PRORA(MS),RESRA(MS),PRONET(MS),TIME(7),DIST1(7),
     1          DIST2(7),TINVAL(6),TIMUL(3),DAYPER(3)
      CHARACTER*8 CDATE
      DATA DIST1 /0.16,0.18,0.16,0.16,0.18,0.16,0.00/
      DATA DIST2 /0.363,0.280,0.122,0.093,0.102,0.040,0.000/
      DATA DAYPER /0.23,0.41,0.5/
C
C  Calculate production and respiration rates per hour during 6 daily
C  time intervals, the lenght which varies with the time of year.
C  For a more detailed distribution, the dimensions, DO-loops and
C  FORMATS of this subroutine should be changed.
C  All data are in mg O2 / m3 / hour, except "TOTAL".
C  It is assumed that equinox is at 12.00 hours sharp.
C  Normal output of the subroutine is to unit IOU(18), but
C  if option "OXMODOUT" is chosen, a specially formatted output will
C  be wrtitten to unit IOU(19) for the oxygen model "OXYMOD".
C
      IF (NPRODU .GT. 1) GO TO 40
C
C  print heading for output on unit IOU(18)
C
      CALL FORMFE (IOU(18))
      WRITE (IOU(18),10)
   10 FORMAT ('   Diurnal production and respiration rates at',
     1       ' equilibrium.',/,'  All data are in mg O2 / m3 / hour ',
     2       'except "NIGHT", which is in mg O2 / m3 / night,',/,
     3       '  and "TOTAL", which is in mg O2 / m3 / day.',/,
     4       '  Two different diurnal patterns are assumed:',/,
     5       '  1. Distribution 1 assumes',
     6       ' an equal production in morning and afternoon.',/,
     7       '  2. Distribution 2 assumes that 75% of gross production',
     8       ' and 25% of photorespiration',/,'     takes place in the',
     9       ' morning but the reverse in the afternoon.',//)
      WRITE (IOU(18),20)
   20 FORMAT (//,1X,116('-'))
      LINEPA = 1
C
C  Print heading for output on unit IOU(19)
C
      IF (LOXOUT .NE. 1) GO TO 40
      WRITE (IOU(19),30) (DIST1(K),K=1,7),(DIST2(K),K=1,7),
     1                   (DAYPER(K),K=1,3)
   30 FORMAT ('Net production per hour for oxygen model.',/,
     1        'Distribution 1',2X,7(F5.3,2X),/,'Distribution 2',2X,
     2        7(F5.3,2X),/,'Dayperiod',2X,3(F5.3,2X))
   40 CONTINUE
C
C  Establish time interval for this period
C
      DO 50 K=1,3
   50 TIMUL(K)=DAYPER(K)*DAY
      TIME(1)=12.00-TIMUL(3)
      TIME(2)=TIME(1)+TIMUL(1)
      TIME(3)=TIME(1)+TIMUL(2)
      TIME(4)=12.00
      TIME(7)=12.00+TIMUL(3)
      TIME(6)=TIME(7)-TIMUL(1)
      TIME(5)=TIME(7)-TIMUL(2)
      DO 60 K=1,6
   60 TINVAL(K)=TIME(K+1)-TIME(K)
      LINEPA = LINEPA + 1
      IF (LINEPA .LT. 6) GO TO 80
      CALL FORMFE (IOU(18))
      LINEPA = 1
   80 WRITE (IOU(18),90) CDATE
   90 FORMAT (' Date: ',A4)
      WRITE (IOU(18),100) (TIME(K),K=1,7)
  100 FORMAT (' Time: ',9X,7(F5.2,7X),3X,'Night',4X,'Total')
      WRITE (IOU(18),110)
  110 FORMAT (' Distribution 1',3X,']',6(11X,']'))
C
C  Calculate gross-, net-production and respiration according to
C  distribution 1 per hour for each interval.
C
      RHOUR=SUMROX/24.0
      DO 120 K=1,6
      PRORA(K)=DIST1(K)*SUMPOX/TINVAL(K)
      RESRA(K)=RHOUR
  120 PRONET(K)=PRORA(K)-RHOUR
      PRORA(7)=0.
      RESRA(7)=(24.0-DAY)*RHOUR
      PRONET(7)=PRORA(7)-RESRA(7)
      SUMNET=SUMPOX-SUMROX
C
C  Print results of calculations
C
      IDIST=0
  130 CONTINUE
      WRITE (IOU(18),140) (PRORA(K),K=1,7),SUMPOX
  140 FORMAT (' Production',7X,']',2X,6(F6.1,3X,']',2X),8X,F7.1,2X,
     1         F7.1)
      WRITE (IOU(18),150) (RESRA(K),K=1,7),SUMROX
  150 FORMAT (' Respiration',6X,']',2X,6(F6.1,3X,']',2X),8X,F7.1,2X,
     1         F7.1)
      WRITE (IOU(18),160) (PRONET(K),K=1,7),SUMNET
  160 FORMAT (' Net-Production',3X,']',2X,6(F6.1,3X,']',2X),8X,F7.1,2X,
     1         F7.1)
C
C  Optionally print output for "OXYMOD".
C
      IF (LOXOUT .NE. 1) GO TO 180
      PRONIG=-RHOUR
      WRITE (IOU(19),170) CDATE,DAY,(PRONET(K),K=1,6),PRONIG
  170 FORMAT (A4,3X,F5.2,7(F9.2,1X))
  180 CONTINUE
C
      IF (IDIST .EQ. 1) GO TO 220
C
C  Calculate gross-, net-production and respiration according to
C  distribution 2 per hour for each interval.
C
      J=7
      PHORES=DAY*RHOUR
      DO 190 K=1,6
      PRORA(K)=DIST2(K)*SUMPOX/TINVAL(K)
      J=J-1
  190 RESRA(J)=DIST2(K)*PHORES/TINVAL(K)
      DO 200 K=1,6
  200 PRONET(K)=PRORA(K)-RESRA(K)
      WRITE (IOU(18),210)
  210 FORMAT (/,' Distribution 2',3X,']',6(11X,']'))
      IDIST=1
      GO TO 130
  220 CONTINUE
      WRITE (IOU(18),230)
  230 FORMAT (1X,116('-'))
      RETURN
      END
