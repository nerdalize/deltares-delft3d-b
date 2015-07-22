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
C    Time:       14:27
C    Program:    SPCSD.FOR
C    Version:    1.0
C    Programmer: Hans Los
C    Previous version(s):
C    0.0 -- 6 Jun 1989 -- 10:01 -- Operating System: DOS
C
C  *********************************************************************
C  *  SUBROUTINE FOR ORDERING EXTINCTION COEFFICIENTS AND DETERMINING  *
C  *         EXISTENCE OF SPECIES IN COEFFICIENT INTERVALS             *
C  *********************************************************************
C
      SUBROUTINE SPCSD(XVEC,RVEC,ACO,EXTLIM,EXTB,NI)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      DIMENSION ACO(MT,MT),RVEC(2*MT),SVEC(2*MT),DVEC(2*MT),XVEC(2*MT)
C
C  Initialize.
C
      DO 10 I=1,NUSPEC
      DO 10 J=1,NUSPEC
   10 ACO(I,J)=1.0
      DO 20 I=1,2*NUSPEC
      DVEC(I)=0.0
      SVEC(I)=0.0
   20 RVEC(I)=0.0
C
C  Determine type roots, subtract EXTB * SDMIX(I).
C  Update nov 4 1992:
C  Use absolute value of SDMIX; SDMIX can be negative for types attached
C  to the bottom.
C
      N=0
      DO 40 I=1,NUSPEC
      IJ=2*I-1
      DO 40 K3=1,2
      N=N+1
      JK=IJ+K3-1
      SVEC(N)=XVEC(JK)-EXTB*DABS(SDMIX(I))
      DVEC(N)=SVEC(N)
   40 CONTINUE
C
C  Order values in vector DVEC.
C
      NN=N-1
      DO 80 I=1,NN
      IL=I+1
      DO 80 J=IL,N
      IF (DVEC(I) .LE. DVEC(J)) GO TO 80
      RTEMP=DVEC(I)
      DVEC(I)=DVEC(J)
      DVEC(J)=RTEMP
   80 CONTINUE
C
C  Are there any valid intervals?
C
      IF (DVEC(N) .LE. 0.0) GO TO 130
C
C  Eliminate intervals whose maximum root is either negative or
C  (in a dynamic run) smaller than the extinction of the remaining
C  biomass.
C
      DO 90 K=1,N
      IF (DVEC(K) .GT. EXTLIM) GO TO 100
   90 CONTINUE
  100 CONTINUE
C     MvdV 960515 - K cannot become 0, because of DVEC(K) at line 77
      IF (K.GT.1) K=K-1
C
C  Eliminate duplicates in output vector.
C
      M=1
      RVEC(1)=DVEC(K)
      DO 110 I=K,NN
      IF (DVEC(I+1) .LE. DVEC(I)) GO TO 110
      M=M+1
      RVEC(M)=DVEC(I+1)
  110 CONTINUE
C
C  Determine types in intervals.
C
      NI=M-1
      DO 125 I=1,NUSPEC
      IJ=2*I-1
      IF ((SVEC(IJ+1) .LT. 0.)) GO TO 125
      DO 120 J=1,NI
      IF ((SVEC(IJ) .LE. RVEC(J)) .AND. (SVEC(IJ+1) .GE. RVEC(J+1)))
     1ACO(J,I)=0.0
  120 CONTINUE
  125 CONTINUE
      RETURN
  130 NI=0
      RETURN
      END
