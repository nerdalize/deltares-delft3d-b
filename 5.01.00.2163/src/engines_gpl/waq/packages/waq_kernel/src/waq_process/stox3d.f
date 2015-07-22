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

      subroutine stox3d ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Vertical dispersion (segment -> exchange)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     +----------------------------------------+
C***********************************************************************
C
C     Project :
C     Author  : M. Bokhorst
C     Date    : 19-3-96            Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     19-3-96 M. Bokhorst     Create first version
C   20000419  Jan van Beek    Check on dummy exchanges (0->0)
C***********************************************************************
C
C     Description of the module :
C
C***********************************************************************

      IMPLICIT NONE
C
C     declaration of arguments
C
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
C
C     i/o from PMSA array
C
      REAL     SPARAM                      ! process parameter on segment
      REAL     FACTOR                      ! scaling factor
      REAL     QPARAM                      ! process parameter on segment
C
C     local declarations
C
      INTEGER  IP1, IP2, IP3               ! index pointers in PMSA array
      INTEGER  IN1, IN2, IN3               ! increments in PMSA array
      INTEGER  IQ                          ! loop counter exchanges
      INTEGER  IFROM                       ! number from-segment
C
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)

      IN1  = INCREM( 1)
      IN2  = INCREM( 2)
      IN3  = INCREM( 3)

C     Exchange-loop over de eerste twee richtingen

      DO IQ=1,NOQ1+NOQ2

C        Uitvoer op exchange niveau gelijk aan nul

         PMSA(IP3 ) =  0.0

         IP3  = IP3  + IN3

      ENDDO

C     Exchange-loop over de derde richting

      DO IQ=NOQ1+NOQ2+1,NOQ1+NOQ2+NOQ3

         IFROM = IEXPNT(1,IQ)

         IF ( IFROM .GT. 0 ) THEN

C           Invoer op segment niveau naar uitvoer op exchange niveau

            SPARAM = PMSA(IP1+(IFROM-1)*IN1)
            FACTOR = PMSA(IP2+(IFROM-1)*IN2)
            QPARAM = SPARAM*FACTOR
         ELSE
            QPARAM = 0.0
         ENDIF

         PMSA(IP3) = QPARAM

C        Ophogen pointering uitvoer op exchange niveau

         IP3  = IP3  + IN3

      ENDDO

      RETURN
      END
