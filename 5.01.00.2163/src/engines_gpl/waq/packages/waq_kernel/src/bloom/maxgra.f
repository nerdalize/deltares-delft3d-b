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
C    Time:       14:12
C    Program:    MAXGRA.FOR
C    Version:    1.0
C    Programmer: Hans Los
C    Previous version(s):
C    0.0 -- 27 Sep 1989 --  8:42 -- Operating System: DOS
C
C  *********************************************************************
C  *      SUBROUTINE TO CALCULATE MAXIMUM GRAZING RATES AT WHICH       *
C  *                   ECOGROUPS CAN SURVIVE                           *
C  *********************************************************************
C
C 0895 MvdV GRAMX added for output of maximum grazing rate and
C           dimension ZOOD and ZOOPR adapted for more than one grazer
      SUBROUTINE MAXGRA(ROOT,EXTB,DAY,DSURF,EADJ,PMAXJ,CDATE,ZOOD,NUMGR,
     1                  NUMSP,DEP,GRAMX)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'ioblck.inc'
      DIMENSION ROOT(2),OUTP(2,20),ZOOD(0:MG)
      CHARACTER*8 CDATE
C
C  Stop criteria:
C  1. Extinction of ecogroup j = background ext. +/- 0.05
C     (equivalent to a chlorophyll concentration < 10.0 mg/m3).
C  2. GHI-GLOW < 0.001.
C  3. After 20 iteration steps.
C If the algorithm stops by either criterium 2 or 3,
C Gmax will be preceeded by a "-" sign in the output.
C
C
C Print heading and compute ZMAX.
C
C
      IF (NUMSP .GT. 1) GO TO 50
      IF (NREP .GT. 1) GO TO 30
      CALL FORMFE (IOU(16))
      WRITE (IOU(16),10)
   10 FORMAT('  Maximum possible grazing rate computation.',/,
     1       '  Determine: 1. Gmax = the maximum grazing rate constan',
     2       't at which',/,'                a phytoplankton',
     3       ' type has a non-zero net growth rate.',/,
     4       '             2. Zmax = the corresponding zooplankton',
     5       ' concentration.',/,'  When more zooplankton is present',
     6       ' than Zmax, the growth rate of type j',/,'  MIGHT',
     7       ' (but does not necessarily) become negative.')
      LINEPA = 2
      WRITE (IOU(16),20) (SPNAME(K),K=1,NUSPEC)
   20 FORMAT (///,' Date',2X,'ZOOD',10X,20(A6,1X),/)
      SARG=ZOOK*XMIN
      ZMAX=(ZOOK+XMIN+2*DSQRT(SARG))/ZOOGR
   30 CONTINUE
      IF (NUGRAZ.EQ.0) THEN
        NG = 0
      ELSE
        NG = 1
      ENDIF
      LINEPA = LINEPA + 1
      IF (LINEPA .LE. 27) GO TO 50
      LINEPA = 1
      CALL FORMFE (IOU(16))
      WRITE (IOU(16),20) (SPNAME(K),K=1,NUSPEC)
   50 CONTINUE
      IF (ZOOPR(NUMSP,NG) .GT. 0.0 ) GO TO 70
C
C   Exit for unedible types, putting a large number in OUTP.
C
      DO 60 I=1,2
   60 OUTP(I,NUMSP+1)=1.D7
      IF (NUMSP .EQ. NUSPEC) GO TO 400
      RETURN
   70 IF (ROOT(2) .GT. EXTB) GO TO 90
C
C  Net growth rate is negative, even if G = 0.0
C
      DO 80 I=1,2
   80 OUTP(I,NUMSP+1)=0.0
      IF (NUMSP .EQ. NUSPEC) GO TO 400
      RETURN
   90 CONTINUE
      ITE=0
C
C  Establish initial boundaries for G interval. Start with:
C  Glow=0.0, and GHI is obtained by demanding EMIN=0.3 and R(T) and
C  M(T) of ecogroup j are both 0.1*PGMAX(T).
C
      GLOW=0.0
      GHI=0.03*PMAXJ*DAY-0.5
  100 GHI=GHI+0.5
      EADJST=EADJ+GHI/PMAXJ

C
C  Update nov 4 1992: use total depth rather than partial depth.
C  See also MAXGRO and BLOOM
C
*     CALL CONSTR(DSURF,DMIX(NUMSP),EADJST,ROOT,NUMGR)
      CALL CONSTR(DSURF,DEP,EADJST,ROOT,NUMGR)
C
C  If groupp j still has a positive growth rate (Kmax > Kb), increase
C  GHI with 0.5.
C
      IF (ROOT(2) .LT. EXTB) GO TO 110
      GLOW=GHI
      GO TO 100
C
C  Half G interval.
C
  110 GMID=(GHI+GLOW)/2.
  120 ITE=ITE+1
      EADJST=EADJ+GMID/PMAXJ
C
C  Update nov 4 1992: use total depth rather than partial depth.
C  See also MAXGRO and BLOOM
C
*     CALL CONSTR(DSURF,DMIX(NUMSP),EADJST,ROOT,NUMGR)
      CALL CONSTR(DSURF,DEP,EADJST,ROOT,NUMGR)
C
C  Test whether ROOT(2) (UKmax) is close enough to EXTB:
C  stop criterium 1.
C
      IF (ROOT(2) .GT. EXTB+0.05) GO TO 160
      IF (ROOT(2) .LT. EXTB-0.05) GO TO 130
      ZMAXJ=ZMAX*GMID/ZOOPR(NUMSP,NG)
      OUTP(1,NUMSP+1)=GMID
      OUTP(2,NUMSP+1)=ZMAXJ
      IF (NUMSP .EQ. NUSPEC) GO TO 400
      RETURN
C
C  Establish the next (lower) grazing rate.
C
  130 GHI=GMID
      GMID=(GMID+GLOW)/2.
C
C  Test for stop criterium 2: GHI-GLOW.
C
  140 GSTOP=GHI-GLOW
      IF (GSTOP .LT. 1.D-3) GO TO 150
      IF (ITE .LT. 20) GO TO 120
  150 ZMAXJ=ZMAX*GMID/ZOOPR(NUMSP,NG)
      OUTP(1,NUMSP+1)=GMID
      OUTP(2,NUMSP+1)=ZMAXJ
      IF (NUMSP .EQ. NUSPEC) GO TO 400
      RETURN
C
C  Establish the next (higher) grazing rate.
C
  160 GLOW=GMID
      GMID=(GHI+GMID)/2.
      GO TO 140
C
C   Print output for each ecogoup for this time-step.
C
  400 CONTINUE
      NTOT=NUSPEC+1

      GRAMX = 0.0
      DO 340 J=2,NTOT
        IF (OUTP(1,J).GT.GRAMX) GRAMX = OUTP(1,J)
  340 CONTINUE

      WRITE(IOU(16),180) CDATE,(OUTP(1,K),K=2,NTOT)
      WRITE(IOU(16),190) ZOOD(0),(OUTP(2,K),K=2,NTOT)
  180 FORMAT(1X,A4,10X,'Gmax',2X,20(F6.2,1X))
  190 FORMAT(8X,F6.1,1X,'Zmax',2X,20(F6.1,1X))
      RETURN
      END
