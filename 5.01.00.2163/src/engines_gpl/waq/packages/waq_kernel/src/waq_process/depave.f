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

      subroutine depave ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Average depth for a Bloom time step (typically a day)

C**********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     |    WAter Resources and Environment     |
C     +----------------------------------------+
C
C***********************************************************************
C
C     Project : Venetie WQ study, 1998
C     Author  : Jos van Gils
C     Date    : 980612             Version : 0.01
C
C     History :
C
C     Date    Author          Description
C     ------  --------------  -----------------------------------
C     971221  Jos van Gils    First Version
C***********************************************************************
C
C     Description of the module :
C
C     Logical Units : -

C     Modules called : -

C     Name     Type   Library
C     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4

      INTEGER  IP1 , IP2 , IP3 , IP4 , IP5 , IP6
      REAL     DEPTH , ADEPTH
      INTEGER  TELLER, NAVERA, NSWITS, ISEG  , IKMRK1
      LOGICAL  FIRST
      SAVE     FIRST
      DATA     FIRST /.TRUE./
      SAVE     TELLER
      DATA     TELLER /0/

      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)

c     Check whether certain input parameters are independent of X

      IF (FIRST) THEN
          FIRST = .FALSE.
          IF ( (INCREM(1) .GT. 0) .OR.
     J         (INCREM(2) .GT. 0) ) THEN
              WRITE (*,*)
     J        ' DEPAVE: INPUT parameters function(x) not ALLOWED'
              CALL SRSTOP(1)
          ENDIF
      ENDIF

C     Retrieve switch for averaging and nr. of steps to be averaged

      NSWITS = NINT(PMSA(IP1))
      NAVERA = NINT(PMSA(IP2))

C     Add 1 to counter and check for period

      TELLER = TELLER + 1
      IF ( TELLER .GT. NAVERA ) TELLER = TELLER - NAVERA

C     Loop over segments

      DO 9000 ISEG = 1 , NOSEG

!!        CALL DHKMRK(1,IKNMRK(ISEG),IKMRK1)
!!        IF (IKMRK1.EQ.1) THEN
          IF (BTEST(IKNMRK(ISEG),0)) THEN

              DEPTH = PMSA(IP3)
              ADEPTH = PMSA(IP4)
              PMSA(IP6) = ADEPTH

              IF ( NSWITS .EQ. 0 ) THEN

C                 No averaging: copy depth to average depth

                  PMSA(IP5) = DEPTH

              ELSE

C                 Averaging: FANCY FORMULA!!!!!

                  PMSA(IP5) = ( ADEPTH*REAL(TELLER-1) + DEPTH )
     J                          / REAL(TELLER)
              ENDIF
          ENDIF
C
          IP1  = IP1  + INCREM( 1)
          IP2  = IP2  + INCREM( 2)
          IP3  = IP3  + INCREM( 3)
          IP4  = IP4  + INCREM( 4)
          IP5  = IP5  + INCREM( 5)
          IP6  = IP6  + INCREM( 6)
C
 9000 CONTINUE
C
      RETURN
C
      END
