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

      subroutine pprlim ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Limitation (numerical) on primary production DYNAMO

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : STANDAARDISATIE PROCES FORMULES T721.72
C     Author  : Pascal Boderie
C     Date    : 921210             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     ......  ..............  ..............................
C
C***********************************************************************
C
C     Description of the module :
C
C Name    T   L I/O   Description                                   Unit
C ----    --- -  -    -------------------                            ---

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
!!    CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!    IF (IKMRK1.GT.0) THEN
      IF (BTEST(IKNMRK(ISEG),0)) THEN
C

      PROD1     = PMSA(IP1 )
      NCRAT1    = PMSA(IP2 )
      PCRAT1    = PMSA(IP3 )
      PROD2     = PMSA(IP4 )
      NCRAT2    = PMSA(IP5 )
      PCRAT2    = PMSA(IP6 )
      SCRAT2    = PMSA(IP7 )
      DELT      = PMSA(IP8 )
      NH4       = PMSA(IP9 )
      NO3       = PMSA(IP10)
      PO4       = PMSA(IP11)
      SI        = PMSA(IP12)
C
      CONMXN = AMAX1(NO3 + NH4,0.0)
      CONMXP = AMAX1(PO4      ,0.0)
      CONMXS = AMAX1(SI       ,0.0)
      PRDMXN = CONMXN / NCRAT1 / DELT
      PRDMXP = CONMXP / PCRAT1 / DELT
      PRDMXA = AMIN1(PRDMXN,PRDMXP)
      PRDMXD = AMIN1(PRDMXN,PRDMXP)
      IF (SCRAT2.GT.0.) THEN
        PRDMXS = CONMXS / SCRAT2 / DELT
        PRDMXD = AMIN1(PRDMXD,PRDMXS)
      ENDIF
      PRDMX  = AMAX1(PRDMXA,PRDMXD)
C
C     THE PRODUCTION RATE OF BOTH DIATOMS AND ALGEA IS CORRECTED
C     RELATIVE TO THEIR INITIAL VALUES.
C     CHECK IF NEWLY CALCULATED RATES MATCH THE MAXIMUM GROWTH RATE
C     ACCORDING TO THE NUTRIENT AVAILABILITY
C
      PRODTA = PROD1 + PROD2
      IF (PRODTA .GT. PRDMX) THEN
         PRODTA = PRDMX
         PROD2C = AMIN1(PRDMX * PROD2 / (PROD2 + PROD1), PRDMXD)
         PROD1C = PRODTA - PROD2C
C
C     CORRECTION ON Nett primary production 1 and 2
C
         PMSA(IP13) = PROD1C
         PMSA(IP14) = PROD2C
         FL ( 1 + IFLUX )  = PROD1C - PROD1
         FL ( 2 + IFLUX )  = PROD2C - PROD2
C
      ELSE
C
         PMSA(IP13) = PROD1
         PMSA(IP14) = PROD2
         FL ( 1 + IFLUX )  = 0.0
         FL ( 2 + IFLUX )  = 0.0
C
      ENDIF
C
      ENDIF
C
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
      IP7   = IP7   + INCREM (  7 )
      IP8   = IP8   + INCREM (  8 )
      IP9   = IP9   + INCREM (  9 )
      IP10  = IP10  + INCREM ( 10 )
      IP11  = IP11  + INCREM ( 11 )
      IP12  = IP12  + INCREM ( 12 )
      IP13  = IP13  + INCREM ( 13 )
      IP14  = IP14  + INCREM ( 14 )
c
 9000 CONTINUE
c
      RETURN
      END
