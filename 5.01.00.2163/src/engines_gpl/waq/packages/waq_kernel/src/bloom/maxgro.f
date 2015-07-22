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
C    Time:       14:23
C    Program:    MAXGRO.FOR
C    Version:    1.0
C    Programmer: Hans Los
C    Previous version(s):
C    0.0 -- 21 Feb 1992 --  8:47 -- Operating System: DOS
C
C  *********************************************************************
C  *      SUBROUTINE TO CALCULATE MAXIMUM ATTAINABLE EQUILIBRIUM       *
C  *            VALUES BASED UPON THE INITIAL GROWTH RATE              *
C  *********************************************************************
C
C    Module revision november 1992.
C    Use the TOTAL depth and the TOTAL extinction to compute the
C    growth rate of inhomogeously mixed species. To that purpose
C    "DEP" was added to the parameter list.
C    Corrected (old) error in format 99995.
C
C    Module revision december 1991 and february 1992.
C    Two important modifications:
C    1. All types of a species now have the same KMAX, but a different
C       objective function. (See also BLOOM.FOR).
C    2. Use XINIT for the growth constraint if the total extinction
C       gets too large; NO extra biomass reduction.
C
      SUBROUTINE MAXGRO(XINIT,ROOT,EXTTOT,EADJ,GRAMOJ,J,ISKMAX,DEP)

      USE DATA_3DL

      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'putin1.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt1.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'cal1.inc'
      INCLUDE 'matri.inc'
      INCLUDE 'dynam.inc'
      INCLUDE 'arran.inc'
      INCLUDE 'ioblck.inc'
      REAL*8 ROOT(*),XINIT(*)
C----------------------------------------------------------------------
C Purpose of this subroutine: find the growth efficiency EFFI for each
C phytoplankton species at the current total extinction level EXTTOT.
C
C  get average efficiency over the layers
C
      IF ( IFIX_3DL(IT2(J,1)) .LT. 0 ) THEN
         EFFI = EFFIC_3DL(ISKMAX,ISEG_3DL)
      ELSE
         CALL EFFI_3DL(EFFI,ISKMAX)
      ENDIF
C
C  Check whether ROOT(2) (UKmax) is larger than EXTTOT.
C
      IF (ROOT(2) .LE. EXTTOT) THEN
C
C  Kmax {=ROOT(2)} is smaller than the total extinction, indicating that
C  the species cannot maintain its current biomass.
C  We do need the average efficiency
C  EFFI, however, to compute the coefficients for the objective
C  function.
C  When Kmax is negative for a species, its biomass should approach
C  zero as quickly as possible. Therefore put 0.01 in the objective
C  function.
C  Note: do not use a negative number because the mortality constraint
C  of the species could be positive, indicating that it should be
C  included in the final solution.
C
         IF (ROOT(2) .LE. 0.0) THEN
            EFFI = 0.01
         END IF
         IF (LGROCH .EQ. 1) THEN
C
C Compute the right hand side of the growth constraint for a species,
C whose Kmax < EXTTOT of previous time-step. Normally we might put ANY
C value into the growth constraint as long as it exceeds the energy
C limitation level: the biomass will most likely get energy limited.
C However, as an extra precaution for dealing with infeasible
C solutions put XINIT into the growth constraint: whatever
C is done with the other constraints, this particular type will not
C increase in biomass.
C Check and correct for growth constraints which are lower than the
C mortality constraints.
C
            GRLIM = XINIT(J)
            IF (LMORCH .EQ. 0) THEN
               B(J+NUEXRO) = GRLIM
               RETURN
            END IF
            IF (GRLIM .GT. B(J+NUEXRO+NUECOG)) THEN
               B(J+NUEXRO) = GRLIM
            ELSE
               B(J+NUEXRO) = B(J+NUEXRO+NUECOG)
            END IF
         END IF
      ELSE
C
C  Compute value BT for the growth constraint.
C
         BT=DEXP( ( (PMAX(ISKMAX)-LPMORT*RMORT(ISKMAX))*EFFI - FLUSH
     *              - GRAMOJ - RESP(ISKMAX) ) *TSTEP*MI)
         BT = BT*XINIT(J)
C
C Set the growth constraint to 0.0 when BT is negative.
C
         IF (BT .LT. 0.0) THEN
            WRITE(IOU(6), 99995) BT, GRNAME(J)
            BT = 0.0D0
         END IF
C
C  Store growth constraint value in B-vector. Optionally print results
C  to unit IOU(6).
C
         B(J+NUEXRO)=BT
         IF (IDUMP .EQ. 1) WRITE (IOU(6),99990) GRNAME(J),EFFI,
     *                     EFFI*PMAX(ISKMAX),B(J+NUEXRO)
C
      END IF
C
C Store the nett growth rate of each phytoplankton type in the row
C for the objective function if growth is maximized.
C
      IF (LOBFUN .EQ. 1) THEN
         DO K = IT2(J,1),IT2(J,2)
            C(K) = DMAX1 ((EFFI * PMAX(K) - RESP(K)), 1.0D-6)
         END DO
      END IF
C
99995 FORMAT (' Warning from MAXGRO: negative growth constraint of ',
     1        ' species ',A8,' = ',F10.5,/,' replaced by 0.0')
99990 FORMAT ('  Species ',A8,' EFFI = ',F5.2,' Growth rate = ',F5.2,
     1        ' B-value = ',F8.1)
      RETURN
      END
