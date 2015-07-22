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
C  *    SUBROUTINE TO CALCULATE THE CONCENTRAION OF LIVING AND DEAD    *
C  *         ALGAE AND THE FLOW OF DEAD ALGAE TO THE BOTTOM            *
C  *********************************************************************
C
      SUBROUTINE POOLS(CDATE,DEATH,ALIVE,TEMP)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'cal1.inc'
      INCLUDE 'ioblck.inc'
      CHARACTER*8 CDATE
C
C  Calculate the total concentrations of living and dead algae
C  at equlibrium.
C  All values are expressed in carbon-units.
C  ALIVE-has already been calculated in subroutine PRODUC.
C  This subroutine writes output on unit IOU(20).
C
      IF (NPRODU .GT. 1) GO TO 30
      WRITE (IOU(20),10)
   10 FORMAT (' Estimated concentrations of living and dead algae, and',
     1        /,' sedimentation rate of dead algae.',////)
C
C  Print heading for weeks on unit IOU(20).
C
      WRITE (IOU(20),20)
   20 FORMAT(2X,'Date',5X,'Temp',11X,'Living Algae',21X,'Dead Algae',
     1       20X,'Sedimentation rate',/,27X,'Mg C / m3',24X,
     2       'Mg C / m3',23X,'Mg C / m3 / Day',/)
   30 CONTINUE
C
C  Calculate concentrations of dead algae in C-units,
C  and the sedimentation rate to the bottom.
C
      DEAD=DEATH*AVAILN*ALIVE/(REMIOR*TEMP+SEDRAT+FLUSH)
      SEDEAD=SEDRAT*DEAD
C
C  Print results.
C
      WRITE (IOU(20),40) CDATE,TEMP,ALIVE,DEAD,SEDEAD
   40 FORMAT (2X,A4,5X,F4.1,13X,3(F7.1,25X))
      RETURN
      END
