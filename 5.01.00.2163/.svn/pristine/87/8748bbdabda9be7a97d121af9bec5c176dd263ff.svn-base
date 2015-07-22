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
C  * SUBROUTINE EXCLUD TO DETERMINE SPECIES PERMITTED IN EACH INTERVAL *
C  *********************************************************************
C
      SUBROUTINE EXCLUD (INOW,LINF,IRS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'size.inc'
      INTEGER NTYPES (MS),IRS(3)
      SAVE NTYPES
      DATA NEXCLU /0/
C
C If the subroutine is called for the first time, compute and store
C the number of types in each phytoplankton group.
C
      NEXCLU = NEXCLU + 1
      IF (NEXCLU .EQ. 1) THEN
         DO 10 I = 1,NUECOG
         NTYPES(I) = IT2(I,2) - IT2(I,1) + 1
   10    CONTINUE
      END IF
C
C  If a species is not permitted in a feasibility interval,
C  put 1.0 in the exclusion row of matrix A.
C  If no species is permitted in interval INOW, exit with LINF = 2
C
      DO 20 K=1,NUSPEC
      IF(ACO(INOW,K) .LT. 1.0D-6) GO TO 30
   20 CONTINUE
      LINF=2
      RETURN
C
C  Determine species permitted in interval INOW
C
   30 CONTINUE
C
C Correct for species with a positive mortality constraint, that are
C no longer permitted in interval INOW: allow all types in this interval,
C but limit their biomasses requiring that the B values for the growth
C and mortality constraint are the same.
C
      IF (LMORCH .EQ. 0) THEN
         DO 40 K=1,NUSPEC
   40    A(NUEXRO,K)=ACO(INOW,K)
         GO TO 90
      END IF
C
C Use ACO (INOW,K) if the Kmax of SOME type of species I is not yet
C exceeded, or if the mortality constraint is 0.0: nothing to conserve.
C Otherwise allow EACH type of I, but make the growth constraint equal
C to the mortality constraint.
C Note: make a copy of this adjusted exclusion row in ACO to record
C all types premitted in any interval. This information is used to
C determine, whether or not simplex should be called AFTER infeasible
C intervals have been detected.
C
      DO 80 I = 1,NUECOG
      NOTPRS = 0
      DO 50 K = IT2(I,1),IT2(I,2)
      IF (ACO(INOW,K) .GT. 0.0) NOTPRS = NOTPRS + 1
   50 CONTINUE
      IF (NOTPRS .LT. NTYPES(I) .OR. B(NUEXRO+NUECOG+I) .LT. 1.D-6) THEN
         DO 60 K = IT2(I,1),IT2(I,2)
   60       A(NUEXRO,K)=ACO(INOW,K)
      ELSE
         DO 70 K = IT2(I,1),IT2(I,2)
            ACO(INOW,K) = 0.0
   70       A(NUEXRO,K)=0.0
         B(NUEXRO + I) = B(NUEXRO + NUECOG + I)
      END IF
   80 CONTINUE
C
C  Exit if the previous interval was feasible (IRS(2) = 0)).
C  Exit if the previous interval was infeasible due to a mortality
C  contstraint: call simplex for the next interval, it might be
C  feasible.
C
   90 CONTINUE
      IF (IRS(2) .EQ. 0) RETURN
      IF (LMORCH .EQ. 1 .AND. IRS(3) .GT. NUEXRO + NUECOG) THEN
         LINF = 0
         RETURN
      END IF
C
C  If interval IFORM was infeasible and if no new types
C  are permitted in INOW,
C  INOW must be infeasible too: exit with LINF = 1
C
      LINF = 1
      IFORM = INOW-1
      DO 100 K=1,NUSPEC
      IF (ACO(INOW,K) .LT. ACO(IFORM,K)) THEN
         LINF = 0
         RETURN
      END IF
  100 CONTINUE
      RETURN
      END
