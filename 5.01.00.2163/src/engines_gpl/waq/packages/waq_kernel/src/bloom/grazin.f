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
C  *         SUBROUTINE TO CALCULATE GRAZING RATE CONSTANTS            *
C  *********************************************************************
C
C    0895 MvdV  dimension for more than one grazer type added to ZOOD
C               and ZOOPR
C               dimension of X changed from MT to MX + 1
C
      SUBROUTINE GRAZIN(X,GRAMOR,ZOOD,ITNUM,LGRAZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'blmdim.inc'
      INCLUDE 'size.inc'
      INCLUDE 'phyt2.inc'
      INCLUDE 'graas.inc'
      INCLUDE 'ioblck.inc'
      DIMENSION X(MX+1),GRAMOR(MT)
C
C  SET INITIAL VALUES
C
      LGRAZ=0
      TOTGRA=0.
      BITOT=0.
      KK=0
   10 KK=KK+1
      IF (ZOOPR(KK,0) .LT. 1.0D-6 .AND. KK .LT. NUSPEC) GO TO 10
      GRAMOR(KK)=0.
C
C  CALCULATE TOTAL BIOMASS EATABLE SPECIES
C
      K1 = NUROWS
      DO 20 K=1,NUSPEC
      K1 = K1 + 1
      IF (X(K1) .LT. 1.D-6 .OR. ZOOPR(K,0) .LT. 1.0D-6) GO TO 20
      BITOT=BITOT+X(K1)
   20 CONTINUE
      IF (IDUMP .EQ. 0) GO TO 35
      WRITE (IOU(6),30) BITOT
   30 FORMAT(2X,'Total biomass edible species: ',F8.2)
   35 CONTINUE
      IF (BITOT .LT. 1.D-6) GO TO 45
C
C  CALCULATE GRAZING DEATHRATE CONSTANT
C
      GRACON=ZOOGR*ZOOD*(BITOT-XMIN)/((ZOOK+BITOT-XMIN)*BITOT)
      DO 40 K=1,NUSPEC
      GRAMOR(K)=ZOOPR(K,0)*GRACON
      IF (GRAMOR(K) .LT. 0.) GRAMOR(K)=0.0
      TOTGRA=TOTGRA+GRAMOR(K)
   40 CONTINUE
   45 CONTINUE
      IF (IDUMP .EQ. 0) GO TO 55
      WRITE (IOU(6),50) GRAMOR(KK)
   50 FORMAT(2X,'Grazing deathrate constant: ',F5.2,//)
   55 CONTINUE
      IF (TOTGRA .GT. 1.D-6 ) GO TO 70
      IF (IDUMP .EQ. 0) GO TO 65
      WRITE (IOU(6),60)
   60 FORMAT (3X,'*** No grazing ***',//)
   65 CONTINUE
      RETURN
   70 IF (ITNUM .LT. IPERM ) LGRAZ=1
      RETURN
      END
