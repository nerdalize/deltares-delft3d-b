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
C  *  SUBROUTINE INTPOL TO PERFORM A LINEAR INTERPOLATION              *
C  *********************************************************************
C
      SUBROUTINE INTPOL_bloom (ARRAY,TESTVA,FIRST,LAST)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 ARRAY(1),TESTVA
      INTEGER FIRST,LAST
C
C  Data in the input array ARRAY are linearly interpolated
C  starting with element FIRST and ending with element LAST.
C  Missing values for begin and end points are assumed to be equal
C  to the first (last) number.
C  The interpolation points are determined by the value TESTVA.
C  The resulting data are returned in ARRAY.
C
      NFIRST = FIRST
      NLAST = LAST
      DO 5 K=FIRST,LAST
      IF (ARRAY(K) .EQ. TESTVA) GO TO 5
      NFIRST = K
      GO TO 6
    5 CONTINUE
    6 CONTINUE
      KK=LAST+1
      DO 7 K=FIRST,LAST
      KK=KK-1
      IF (ARRAY(KK) .EQ. TESTVA) GO TO 7
      NLAST = KK
      GO TO 8
    7 CONTINUE
    8 CONTINUE
      ARRAY(FIRST)=ARRAY(NFIRST)
      ARRAY(LAST)=ARRAY(NLAST)
C
C  start interpolation.
C
      N2=FIRST
      DO 20 K=FIRST,LAST
      IF (ARRAY(K) .EQ. TESTVA) GO TO 20
      N1=N2
      N2=K
      IF ((N2-N1).EQ. 0) GO TO 20
      DIFF=(ARRAY(N2)-ARRAY(N1))/(N2-N1)
      ID1=N1+1
      ID2=N2-1
      ID3=0
      DO 10 KK=ID1,ID2
      ID3=ID3+1
      ARRAY(KK)=ARRAY(N1)+DIFF*ID3
   10 CONTINUE
   20 CONTINUE
      RETURN
      END
