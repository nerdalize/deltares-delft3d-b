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
C  *    SUBROUTINE TO CALCULATE THE SUM OF BIOMASSES                   *
C  *********************************************************************
C
      SUBROUTINE DOMINA(XDEF,TOTAL,PHYTID,PHYMIN,NPER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'sumout.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'dynam.inc'
      DIMENSION XDEF(*),XAVER(MT),XREL(MT)
C
C  If called while computations continue (NPER = 0),
C  increase XSUM, OBSSUM, NOBS, CHI2 and exit.
C  Otherwise create final table.
C
      IF (NPER .GT. 0) GO TO 20
      CHLSUM = CHLSUM + TOTAL
      DO 10 I=1,NUSPEC
   10 XSUM(I)=XSUM(I)+XDEF(I+NUROWS)
      IF (PHYTID .EQ. PHYMIN) RETURN
      OBSSUM = OBSSUM + PHYTID
      NOBS = NOBS + 1
      CHI2 = CHI2 + (PHYTID - TOTAL) ** 2
      RETURN
C
C  Compute averages.
C
   20 CONTINUE
      IF (IOFLAG .EQ. 1) CALL MORESC
      CHLAV = CHLSUM / NPER
      TOTSUM=0.0
      DO 30 I=1,NUSPEC
      XAVER(I)=XSUM(I)/NPER
   30 TOTSUM=TOTSUM+XAVER(I)
      IF (TOTSUM .LT. 1.0D-6) TOTSUM = 1.0
C
C  Compute relative dominance.
C
      DO 40 I=1,NUSPEC
   40 XREL(I)=100.0*XAVER(I)/TOTSUM
C
C Compute goodness of fit coefficient.
C
      IF (NOBS .GT. 0) THEN
         OBSSUM = OBSSUM / NOBS
         CHI2 = DSQRT (CHI2 / (NOBS * OBSSUM) )
      ELSE
         OBSSUM = -1.0
         CHI2 = 0.0
      END IF
C
C  Create and print summary table. In a batch run, write this input
C  to the standard output unit only (OUUNI in the stand-alone version
C  of BLOOM II, 22 in the dynamic version of the program. In an
C  interactive run write a copy to the terminal too.
C
      IF (IOFLAG .EQ. 1) THEN
         NOUT = OUUNI
      ELSE
         IF (LDYN .EQ. 0) THEN
            NOUT=OUUNI
         ELSE
            NOUT=IOU(22)
         END IF
      END IF
      TOTSUM = TOTSUM / 1000.
   50 CONTINUE
      CALL FORMFE (NOUT)
      WRITE (NOUT,1000) NPER
      WRITE (NOUT,1010)
      DO 60 I=1,NUSPEC
   60 WRITE (NOUT,1020) SPNAME(I),XSUM(I),XAVER(I),XREL(I)
      WRITE (NOUT,1030) TOTSUM
      WRITE (NOUT,1040) CHLAV
      WRITE (NOUT,1050) OBSSUM
      WRITE (NOUT,1060) CHI2,NOBS
      IF (IOFLAG .EQ. 0 .OR. NOUT .GE. 21) GO TO 70
      IF (LDYN .EQ. 0) THEN
         NOUT=IOU(21)
      ELSE
         NOUT=IOU(22)
      END IF
      GO TO 50
   70 CONTINUE
      IF (IOFLAG .EQ. 1) CALL MORESC
 1000 FORMAT ('  Summary of output for ',I2,' periods.',/,' ')
 1010 FORMAT ('  Species name',5X,'Total',5X,'Average',5X,'Dominance')
 1020 FORMAT (2X,A8,5X,F9.1,4X,F8.1,7X,F5.1,' %')
 1030 FORMAT (' ',/,'  Average total biomass         = ',F5.2,
     1        ' grams dry weight per cubic meter.')
 1040 FORMAT ('  Average chlorophyll           = ',F5.1,
     1        ' mg per cubic meter.')
 1050 FORMAT ('  Average observed chlorophyll  = ',F5.1,
     1        ' mg per cubic meter.')
 1060 FORMAT ('  Goodness of fit coefficient   = ',F5.1,
     1        ' for ',I3,' observations.')
      RETURN
      END
