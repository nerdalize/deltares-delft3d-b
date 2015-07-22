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

      subroutine phcomp ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Composition of phytoplankton by summing algae fractions - Dynamo - GEM

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  : Jos van Gils
C     Date    : 940725             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C     940725  Jos van Gils    Create first version
C     980603  Jos van Gils    Generalized for use with GEM as well
C
C***********************************************************************
C
C     Description of the module :
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library

C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  ITEL  , ISEG  , IKMRK1, IKMRK2
      INTEGER  NTYPE , ITYPE
      REAL     PHYT  , ALGN  , ALGP  , ALGSI , ALGDM , CHLFA , BIOMAS,
     J         NCRAT , PCRAT , SICRAT, DMCF  , CATOCL

      NTYPE   = PMSA(IPOINT(1))
C
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.EQ.1 .OR. IKMRK1.EQ.3) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN

          PHYT = 0.0
          ALGN = 0.0
          ALGP = 0.0
          ALGSI = 0.0
          ALGDM = 0.0
          CHLFA = 0.0

          DO 100 ITYPE = 1,NTYPE

              ITEL   = 1 + ITYPE
              BIOMAS = PMSA ( IPOINT(ITEL) + (ISEG-1)*INCREM(ITEL) )
              ITEL   = 1 + ITYPE + NTYPE
              NCRAT  = PMSA ( IPOINT(ITEL) + (ISEG-1)*INCREM(ITEL) )
              ITEL   = 1 + ITYPE + NTYPE*2
              PCRAT  = PMSA ( IPOINT(ITEL) + (ISEG-1)*INCREM(ITEL) )
              ITEL   = 1 + ITYPE + NTYPE*3
              SICRAT = PMSA ( IPOINT(ITEL) + (ISEG-1)*INCREM(ITEL) )
              ITEL   = 1 + ITYPE + NTYPE*4
              DMCF   = PMSA ( IPOINT(ITEL) + (ISEG-1)*INCREM(ITEL) )
              ITEL   = 1 + ITYPE + NTYPE*5
              CATOCL = PMSA ( IPOINT(ITEL) + (ISEG-1)*INCREM(ITEL) )

C***********************************************************************
C**** Calculations connected to the status of the algae
C***********************************************************************

C             Total Carbon in algae

              PHYT = PHYT + BIOMAS

C             Total nitrogen

              ALGN = ALGN + BIOMAS * NCRAT

C             Total phosphorus

              ALGP = ALGP + BIOMAS * PCRAT

C             Total silica

              ALGSI = ALGSI + BIOMAS * SICRAT

C             Total dry matter

              ALGDM = ALGDM + BIOMAS * DMCF

C             Chlorophyll

              CHLFA = CHLFA + BIOMAS * CATOCL

  100     CONTINUE

          ITEL = 1 + 6*NTYPE + 1
          PMSA (IPOINT(ITEL)+(ISEG-1)*INCREM(ITEL)) = PHYT
          ITEL = 1 + 6*NTYPE + 2
          PMSA (IPOINT(ITEL)+(ISEG-1)*INCREM(ITEL)) = ALGN
          ITEL = 1 + 6*NTYPE + 3
          PMSA (IPOINT(ITEL)+(ISEG-1)*INCREM(ITEL)) = ALGP
          ITEL = 1 + 6*NTYPE + 4
          PMSA (IPOINT(ITEL)+(ISEG-1)*INCREM(ITEL)) = ALGSI
          ITEL = 1 + 6*NTYPE + 5
          PMSA (IPOINT(ITEL)+(ISEG-1)*INCREM(ITEL)) = ALGDM
          ITEL = 1 + 6*NTYPE + 6
          PMSA (IPOINT(ITEL)+(ISEG-1)*INCREM(ITEL)) = CHLFA

      ENDIF
c
 9000 CONTINUE
c
      RETURN
C
      END
