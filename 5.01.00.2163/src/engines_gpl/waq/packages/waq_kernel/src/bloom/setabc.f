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

C    Date:       4 Nov 1992
C    Time:       14:32
C    Program:    SETABC.FOR
C    Version:    1.0
C    Programmer: Hans Los
C    Previous version(s):
C    0.0 -- 26 Sep 1989 -- 14:15 -- Operating System: DOS
C
C    Update 1.0: Added DEP to parameter list
C              : Include new section to compute surface light intensity
C                and mixing depth fraction of bottom algae (Ulva)
C
C    0895 MvdV  dimension for more than one grazer type added to ZOOD
C
C  *********************************************************************
C  *          SUBROUTINE TO SET MATRIX A AND B                         *
C  *********************************************************************
C
      SUBROUTINE SETABC(XINIT,EXTB,EXTTOT,ZOOD,CSOL,DSOL,T,DEP,ID,NSET,
     1                  LCOUPL)
      IMPLICIT REAL*8 (A-H,O-Z)
      SAVE
      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'cal1.inc'
      INCLUDE 'arran.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'dynam.inc'
      INCLUDE 'ioblck.inc'
      REAL*8 XINIT(*),PMAX20(MT),TCORR(MT),SDMIXN(MT),ZOOD(0:MG)
C
C  If this is the first time through the subroutine,
C  then initiate A, B and C
C

Cjvb  , perform this every time for Ulva
C
      IDREM = IDUMP
      IDUMP = 0
      CALL MAXPRD(TEFCUR)
      DO 20 K = 1,NUSPEC
   20 PMAX20(K) = PMAX(K)
      CALL MAXPRD(T)
      IDUMP = IDREM
C
      IDREM = 0
      DO 30 K = 1, NUSPEC
      IF (SDMIX(K) .LT. 0.0) THEN
         SDMIXN(K) = 1.0D0 + SDMIX(K)
         DMIX(K) = DABS(SDMIX(K)) * DEP
         IDREM = 1
      ELSE
         SDMIXN(K) = 0.0D0
      END IF
30    CONTINUE
Cjvb
      NSET = NSET + 1
      IF (NSET .GT. 1) GO TO 70
C
C  Initialize "C" values for all species to 1.0: maximize.
C  (See also subroutines SOLVLP and MAXGROGR)
C
      DO 10 J=1,NUSPEC
   10 C(J)=1.0
C
C  Initiate multiplier for exponential term at zero:
C  start with steady state solution for the dead algal pool
C
      EXPMUL=0.0
C
C  Find PMAX values at TEFCUR degrees to determine temperature
C  correction for the efficiency curves.
C
C     IDREM = IDUMP
C     IDUMP = 0
C     CALL MAXPRD(TEFCUR)
C     DO 20 K = 1,NUSPEC
C  20 PMAX20(K) = PMAX(K)
C     CALL MAXPRD(T)
C     IDUMP = IDREM
C
C  Update november 1992.
C  If a negative value of SDMIX was specified in the input, we are
C  dealing with a type that is only mixed over the lower fraction SDMIX
C  of the water column. Determine:
C  1. The fraction of the depth over which this type is NOT mixed
C     (SDMIXN). This is used to compute the "surface" light intensity.
C  2. The fraction of the depth over which this type DOES get mixed.
C  Note: DMIX has been already computed by RUN, but as we reset SDMIX
C  here we must also recompute DMIX for the initial time step.
C
c     IDREM = 0
c     DO 30 K = 1, NUSPEC
c     IF (SDMIX(K) .LT. 0.0) THEN
c        SDMIXN(K) = 1.0D0 + SDMIX(K)
c        DMIX(K) = DABS(SDMIX(K)) * DEP
c        IDREM = 1
c     ELSE
c        SDMIXN(K) = 0.0D0
c     END IF
c  30 CONTINUE
      IF (IDREM .EQ. 1) THEN
         WRITE (IOU(10), 99996) (DABS(SDMIX(K)), K = 1, NUSPEC)
         WRITE (IOU(10), 99995) (SDMIXN(K), K = 1, NUSPEC)
      END IF
C
C  Print warning message if a non-zero value is specified for the
C  sedimentation or flushing rate
C
      IF ( SEDRAT .LT. 1.0D-6) GO TO 60
      WRITE (IOU(10),99999) SEDRAT
   60 CONTINUE
      IF ( FLUSH .LT. 1.0D-6) GO TO 70
      WRITE (IOU(10),99998) FLUSH
   70 CONTINUE
C
C  Calculate solar radiation level for particular week.
C
C
C  Determine surface reflectance and transmitted radiation
C  -- average reflection modified according to WETZEL--
C  Do not coreect for reflection when ID is negative: BLOOM model is
C  is applied to a bottom segment.
C
      ALPHA=0.95
      IF ((ID .LE. 17) .OR. (ID .GE. 32)) ALPHA=0.94
      IF ((ID .LE. 13) .OR. (ID .GE. 36)) ALPHA=0.92
      IF ((ID .LE.  4) .OR. (ID .GE. 45)) ALPHA=0.90
      IF (ID .GT. 0) CSOL=ALPHA * CSOL
C
C
C  Convert CSOL from:
C  Joules per cm2 per week to Joules per m2 per day.
C  Determine temperature correction, assuming that the nominal
C  efficiency curves are all for temperatures of TEFCUR deg. centigrade.
C
      DSOL=1428.57*CSOL
C
C  Determine the base level for the growth constraints (optionally).
C  If there is a discontinuity in the period numbers, EXTTOT and XINIT
C  are reinitialized.
C
      IF (NRUN .LE. 1) GO TO 90
      IF (IMU .EQ. 1) GO TO 90
      IDPREV = ID - MI
      IF (IDPREV .GE. NPER(IMU,1)) GO TO 90
      IMPREV = IMU - 1
      IF (IDPREV .GE. NPER(IMPREV,1) .AND.
     1    IDPREV .LE. NPER(IMPREV,2)) GO TO 90
      EXTTOT = EXTB
      IF (LGROCH .NE. 1) GO TO 90
      DO 80 J=1,NUECOG
   80 XINIT(J)=1.D+6
   90 CONTINUE
C
C  Compute equivalent radiation level.
C  Update November 1992.
C110  SURF(K)= TCORR(K) * DSOL
C  Multiply by the light reduction of overlying water columns. Usually
C  this factor is 1.0 as SDMIXN = 0.0; for types attached to the bottom
C  (Ulva) this factor is not 1.0, however.
C
      DO 100 K=1,NUSPEC
 100  TCORR(K) = PMAX20(K)/PMAX(K)
      DO 110 K=1,NUSPEC
 110  SURF(K)= TCORR(K) * DSOL * DEXP (- EXTTOT * SDMIXN(K) * DEP)
      IF (IDUMP .EQ. 1) WRITE (IOU(6),99997) (TCORR(K),K=1,NUSPEC)
C
C  Calculate nutrient coefficients, unless BLOOM II is invoked by the
C  coupled model. In this case resetting of A-coefficients can be
C  skipped.
C
C  If RNUT(2,I)=0.0, remineralisation rate I =RNUT(1,I)
C  If RNUT(2,I)=1.0, remineralisation rate I =RNUT(1,I) * temperature
C
      QMREM = 0.0
      IF (LCOUPL .NE. 0) GO TO 170
      DO 150 J=1,NUNUCO
      IF (RNUT(2,J) .LT. 1.0D-6) THEN
         RNUTRI=RNUT(1,J)+SEDRAT+FLUSH
      ELSE
         RNUTRI=RNUT(1,J)*T+SEDRAT+FLUSH
      END IF
      EXPNUT=DEXP(-RNUTRI*TSTEP*MI)
      EXPNUT=EXPMUL*EXPNUT
      REMEXP(J)=EXPNUT
      REMINU(J)=RNUTRI
      DO 140 K=1,NUSPEC
  140 A(J,K)=AA(J,K)*(AVAILN*RMORT(K)*(1.0-EXPNUT)+RNUTRI)/RNUTRI
  150 CONTINUE
C
C  Calculate extinction coefficients
C  Update nov 4 1992:
C  Use absolute value of SDMIX; SDMIX can be negative for types attached
C  to the bottom.
C
C
      REMIT=DEXP(REMILI(1)*T-REMILI(2))
      QMREM=AVAILN/(REMIT+SEDRAT+FLUSH)
      DO 160 K=1,NUSPEC
      ATEMP=EKX(K)*(QMREM*RMORT(K)*DABS(SDMIX(K))+1.0)
      DO 160 J=NUFILI,NUABCO
  160 A(J,K)=ATEMP
C
C  Set "B" values for nutrients by substracting the amount in
C  zooplankton from the input values and correcting for deviations
C  from steady state if option DYNADEAD was selected
C
  170 DO 180 K=1,NUNUCO
        IF (NUGRAZ .GT. 0) THEN
          B(K)=CONCEN(K)-DETRIT(K)*REMEXP(K)

C         0895 MvdV subtract nutrients for all grazer types
          DO 190 J=1,NUGRAZ
            B(K)=B(K)-ZOONUT(K,J)*ZOOD(J)
  190     CONTINUE
        ELSE
          B(K)=CONCEN(K)-ZOONUT(K,0)*ZOOD(0)-DETRIT(K)*REMEXP(K)
        ENDIF
  180 CONTINUE
      IF (LDYDEA .EQ. 0) RETURN
      EXPMUL=1.0
C
C  Formats for this subroutine
C
99999 FORMAT (//,1X,'* WARNING *   A sedimentation rate of',2X,F5.3,2X,
     1        'has been specified.',/,1X,'In order to keep the total',
     2        ' amount of nutrients constant, the program assumes',/,
     3        1X,'the amount of sedimented nutrients to be replaced',/,
     4        ' by dissolved nutrients from the bottom.',//)
99998 FORMAT (//,1X,'* WARNING *  A flushing rate of',2X,F5.3,2X,
     1        'has been specified.',/,1X,'In order to keep the total',
     2        ' amount of nutrients constant, the program assumes',/,
     3        1X,'the amount of nutrients flushed from the dead algal',
     4        ' pool',/,' to be replaced by dissolved nutrients',
     5        ' from the intake water.',//)
99997 FORMAT ('  Tcorr(j):   ',10(F5.2,1X))
99996 FORMAT (//,1X,'Computation with inhomogeneous mixing.',/,
     1        '  SDMIX(J):   ',20(F4.2,1X))
99995 FORMAT ('  SDMIXN(J):  ',20(F4.2,1X))
      RETURN
      END
