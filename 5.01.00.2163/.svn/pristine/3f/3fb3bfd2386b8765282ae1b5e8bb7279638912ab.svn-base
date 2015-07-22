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

C    Date:       26 Feb 1992
C    Time:       19:10
C    Program:    FIXINF.FOR
C    Version:    2.0
C    Programmer: Hans Los
C    Previous version(s):
C    1.1 -- 25 Feb 1992 -- 13:42 -- Operating System: DOS
C    1.0 -- 11 Oct 1990 -- 13:09 -- Operating System: DOS
C    0.0 -- 12 Dec 1989 -- 10:19 -- Operating System: DOS
C
C  Update 1.1: store total biomass not only in BIO(2) but also in
C  X(NUCOLS+2)
C
C  Update 2.0: Included a new section for coupled model versions.
C              Changed the subroutine call.
C              Cosmetics and comments.
C
C  Purpose of this module: modify some of the boundary conditions for
C  infeasible systems. We follow a different approach for different
C  model versions for reasons to be explained here. Infeasible systems
C  occur, when a minimum constrain (usually a mortality constraint)
C  conflicts with a maximum constraint (nutrient; energy). To deal with
C  this problem we must either lower the value of the mortality
C  constraint, or increase the available amount of the maximum
C  constraint, which is violated.
C
C
C  1. Stand alone BLOOM II
C     a. Steady state computation.
C        If there is no valid solution, simply put all biomasses to 0.0:
C        this is obviously the correct steady state solution.
C     b. Dynamic computation.
C        Drop the mortality constraint which is violated. Argument:
C        the boundary conditions are based on measured conditions and
C        therefore relatively well defined. Little is known about the
C        mortality of declining species. Therefore drop the moratlity
C        constraint.
C   2. BLOOM as a module (DELWAQ; JSBACH)
C        The mortality is pre-calculated and so are the corresponding
C        nutrient fluxes. Conflicts between nutrient and mortality
C        constraints cannot occur in theory (they do occasionally though
C        due to numerical round-off errors and small time-step
C        problems). In almost all cases the infeasibility is caused by
C        a mortality constraint and the energy constraint. Therefore
C        try to increase the energy consrtaint sufficiently and rerun
C        the problem. If this won't help, drop the mortality constraint.
C        Note: this may lead to NEGATIVE production rates as BLOOM
C        returns a value below the mortality constraint!
C
C  *********************************************************************
C  * SUBROUTINE TO DEAL WITH CASES WHERE ALL INTERVALS ARE INFEASIBLE  *
C  *********************************************************************
C
      SUBROUTINE FIXINF(X,BIO,EXTTOT,EXTB,INHIB,NI,IRERUN,IRS,INFEAS,
     &                  ERRIND,JKMAX,AROOT,CDATE,LCOUPL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'size.inc'
      INCLUDE 'ioblck.inc'
      INCLUDE 'dynam.inc'
      INCLUDE 'sumout.inc'
      DIMENSION X(*),BIO(*),SUMNUT(MN),AROOT(*)
      INTEGER IRS (*),JKMAX(*),LCOUPL
      CHARACTER*8 CDATE
      CHARACTER*1 ERRIND
C
C Determine output unit for error messages.
C
      IF (IOFLAG .EQ. 0) THEN
         NOUT = IOU(6)
      ELSE
         NOUT = IOU(10)
      END IF
C
C Set flag for non-unique solutions (LST) to 0.
      LST = 0

C ----------------------------------------------------------------------
C Start of steady state BLOOM section.
C In a run WITHOUT mortality constraints, set all biomasses to zero,
C all nutrient slacks to the total available concentrations and the
C total extinction to the background extinction.
C Note: this is also the final solution in dynamic runs when everything
C else fails.
C
      IF (LMORCH .EQ. 1) GO TO 40
   10 CONTINUE
      DO 20 J=1,NUNUCO
   20 X(J)=B(J)
      X(NUABCO)=0.0
      IF (INHIB .EQ. 1) THEN
         X(NUFILI)=0.0
      ELSE
         X(NUFILI)=1.0
      END IF
      DO 30 J=NUROWS,NUCOLS
   30 X(J)=0.0
      BIO(2)=-1.0
      X(NUCOLS+2) = BIO(2)
      EXTTOT=EXTB
      IRERUN = 0
      RETURN

C ----------------------------------------------------------------------
C The ultimate solution: put all biomasses to 0!
C Was the problem rerun already? Is there still hope for a neat
C solution?
C
   40 CONTINUE
      IF (IRERUN .EQ. 3) THEN
         WRITE (NOUT,50) CDATE
   50    FORMAT (/,' ',' !!! SEVERE ERROR MESSAGE time ',A8,'.')
         WRITE (NOUT,60) IRS(2)
   60    FORMAT ('  Problem remains infeasible due to constraint ',I2,
     &           '.',/,'  All biomasses set to 0.0.')
         ERRIND = '!'
         GO TO 10
      END IF

C ----------------------------------------------------------------------
C If there are no intervals at all, or if the infeasibility is not due
C to a mortality constraint, set all species at their mortality
C constraint.
C
      IF (NI .EQ. 0 .OR. IRS(3) .LE. NUEXRO + NUECOG) GO TO 130

C ----------------------------------------------------------------------
C Problem is infeasible due to a mortality constraint. In stand-alone
C BLOOM (LCOUPL = 0) drop the violated mortality constraint. Rerun.
C
C In coupled model versions, it is almost always sufficient to increase
C the energy constraint. Compute the minimum extinction value that could
C make the problem feasible. This is the background extinction EXTB
C (which includes detritus) plus the extinction of all phytoplankton
C species at their minimum level. If the old Kmax is high enough, the
C violation must be due to a nutrient constraint afterall so drop the
C mortality constraint. Rerun.
C Note: we have assumed that the type whose objective function is the
C highest of a species also has the highest specific extinction
C coefficient. This is quite logical, and therefore UNCHECKED!
C
C Note: Restore the original growth constraint vector.
C
         INDEX = IRS(3) - NUEXRO - NUECOG
         IF (INDEX .GT. NUECOG) GO TO 130
         IF (IRERUN .NE. 2) IRMAX = 0
         IRMAX = IRMAX + 1
         IF (IRMAX .GT. NUSPEC) GO TO 130
         IRERUN = 2
         ERRIND = '*'
         EXTREM = EXTB
         IF (LPRINT .EQ. 2) WRITE (NOUT,90) CDATE
   90    FORMAT (/,' ',' *** Warning message for time ',A8,'.')
         DO 100 K = 1,NUECOG
            B(K + NUEXRO) = BGRO(K)
            EXTREM = EXTREM + B(NUEXRO+NUECOG+K) * A(NUABCO,JKMAX(K))
  100    CONTINUE
         IF (LCOUPL .EQ. 0) THEN
            B(IRS(3)) = 0.0
            IF (LPRINT .EQ. 2) WRITE (NOUT,110) GRNAME (INDEX)
  110          FORMAT (' Mortality constraint of species ',A8,' is '//
     &         'violated.',/,' This constraint is dropped.')
         ELSE
            DO 125 K = IT2(INDEX,1), IT2(INDEX,2)
               IF (AROOT(2*K) .LT. EXTREM) THEN
                   AROOT(2*K) = EXTREM
                   IF (LPRINT .EQ. 2) WRITE (NOUT, 120) GRNAME(INDEX)
  120              FORMAT (' Mortality constraint of species ',A8,
     &             'is violated.',/,' KMAX NOW set above mortality '//
     &             'constaint.')
               ELSE
                   B(IRS(3)) = 0.0
                   IF (LPRINT .EQ. 2) WRITE (NOUT, 110) GRNAME(INDEX)
               END IF
  125       CONTINUE
         END IF
         RETURN

C ----------------------------------------------------------------------
C In a final attempt to fix the problem set all biomasses to their
C minimum permissible values: solve the differential equation for
C the mortality. This solution has been optained elsewhere; the result
C must have been stored in the X-vector. It is computed as:
C
C     XDEFJ = X(NUROWS + J) * DEXP(-MI * TSTEP * RMORT(J))
C
C Note: the total extinction will be computed in subroutine BLOOM no
C matter wheter the solution was feasible.
C Get minimum (=actual) biomasses from X-vector.
C Compute nutrients in phytoplankton and (in a non-dynamic run) in
C detritus. Compute the total biomass. Set slacks for mortality
C constraints of all species, whose biomasses are set equal to the
C mortality constraint.
C
  130 CONTINUE
      DO 140 I = 1,NUNUCO
  140 SUMNUT(I) = 0.0
      BIOMAX = 0.0
      DO 170 J = 1,NUECOG
         MOF = 0
         DO 160 K = IT2(J,1),IT2(J,2)
            XDEFK = X(NUROWS + K)
            ISPLIM(K) = 0
            IF (XDEFK .LT. 1.D-6) GO TO 160
            MOF = MOF + 1
            ISPLIM(K) = NUROWS - NUECOG + J
            BIOMAX = BIOMAX + XDEFK
            DO 150 I = 1,NUNUCO
  150       SUMNUT(I) = SUMNUT(I) + A(I,K) * XDEFK
  160    CONTINUE
         IF (MOF .GT. 0) X(NUROWS-NUECOG+J) = 0.0D1
  170 CONTINUE
      BIO(2) = BIOMAX
      X(NUCOLS+2) = BIO(2)
C
C  Compute the nutrient slacks. Check, whether they are positive.
C  If not, perform various actions depending on the kind of run:
C
C  1. In a dynamic run, declare the problem to be infeasible.
C     This situation should NEVER occur!
C  Otherwise,
C  2. Set all species equal to their mortality constraint, if
C     no extinction intervals exist,
C  3. Release the mortality constraints and re-run the problem if
C     valid extinction intervals do exist.
C
      DO 220 I = 1,NUNUCO
      XI = B(I) - SUMNUT(I)
      IF (XI .LT. 0.0) THEN
         IF (LDYN .EQ. 1) THEN
            IF (LPRINT .EQ. 2) THEN
              WRITE (NOUT,50) CDATE
              WRITE (NOUT,180) CSTRA(I)
  180         FORMAT (' One of the mortality constraints violates the ',
     &                 A8,' constraint.',/,' Problem is infeasible.')
              ERRIND = '!'
            END IF
            GO TO 10
         ELSE
            IF (NI .EQ. 0) THEN
              XI = 0.0
              IF (LPRINT .EQ. 2) THEN
                WRITE (NOUT,90) CDATE
                WRITE (NOUT,190) CSTRA(I)
  190         FORMAT (' One of the mortality constraints violates the ',
     &                 A8,' constraint.',/,' Negative concentrations ',
     &                 'are tolerated.',/,' All species set to their ',
     &                 'mortality constraints.')
                ERRIND = '*'
            END IF
            ELSE
              IF (LPRINT .EQ. 2) THEN
                WRITE (NOUT,90) CDATE
                WRITE (NOUT,200) CSTRA(I)
  200         FORMAT (' One of the mortality constraints violates the ',
     &                 A8,' constraint.',/,' All mortality constaints ',
     &                 'will be released.')
                ERRIND = '*'
              END IF
              DO 210 K = 1,NUECOG
              B(K + NUEXRO + NUECOG) = 0.0
  210         B(K + NUEXRO) = BGRO(K)
              IRERUN = 3
              RETURN
            END IF
         END IF
      END IF
  220 X(I) = XI
      IF (INHIB .EQ. 1) THEN
         X(NUFILI)=0.0D0
      ELSE
         X(NUFILI)=1.0
      END IF
C     IF (NI .EQ. 0 .AND. BIOMAX .LT. 1.0D-6) THEN
      IF (NI .EQ. 0) THEN
         X(NUABCO) = 0.0D0
      ELSE
         X(NUABCO) = 1.0
      END IF
      IRERUN = 0
      INFEAS = 0
      RETURN
      END
