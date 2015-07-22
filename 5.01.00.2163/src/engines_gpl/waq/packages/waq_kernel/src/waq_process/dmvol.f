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

      subroutine dmvol  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Volume of dry matter in a segment

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : GEM T2087
C     Author  : Rik Sonneveldt
C     Date    : 02jun97            Version : 0.00
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     02jun97 Rik Sonneveldt  first version, some existing code of WKCOMP
C                             by PascalBoderie/Jos van Gils has been used.
C     25sep97 Jos van Gils    Porosity is output variable!
C
C***********************************************************************
C
C     Description of the module :
C
C        General water quality module for DELWAQ:
C
C Name    T   L I/O   Description                                    Uni
C ----    --- -  -    -------------------                            ---

C     Logical Units : -

C     Modules called : -

C     Name     Type   Library

C     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      PARAMETER (RHOWAT = 1000000.)

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
C
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
         CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!       IF (IKMRK1.EQ.1.OR.IKMRK1.EQ.3) THEN
         IF (BTEST(IKNMRK(ISEG),0)) THEN

            Surf    = PMSA(IP1 )
            Volume  = PMSA(IP2 )
            TIM     = PMSA(IP3 )
            POM     = PMSA(IP4 )
            RhoIM   = PMSA(IP5 )
            RhoOM   = PMSA(IP6 )

            VolDM  = (TIM/RhoIM + POM/RhoOM)
            VolDM  = min ( 1.0 , VolDM )
            VolDM  = max ( 0.0 , VolDM )
            Poros  = 1.0 - VolDM
            Poros = min(0.999,Poros)
            Poros = max(0.02 ,Poros)
            VolDM  = VolDM * Volume
            Rho    = (TIM + POM + Poros*RHOWAT)

            IF (IKMRK1.EQ.3) THEN
               ActTh  =  VOLUME/SURF
            ELSE
               ActTh = 0.0
            ENDIF

            PMSA(IP7)  =  Poros
            PMSA(IP8)  =  Rho
            PMSA(IP9)  =  VolDM
            PMSA(IP10) =  ActTh

         ELSE
            PMSA(IP7)  =  1.0
            PMSA(IP8)  =  RHOWAT
            PMSA(IP9)  =  0.0
            PMSA(IP10) =  0.0
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
c
 9000 CONTINUE
c
      RETURN
C
      END
